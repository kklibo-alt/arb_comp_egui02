use crate::diff::{self, HexCell};
use arb_comp06::{bpe::Bpe, matcher, re_pair::RePair, test_patterns, test_utils};
use egui::{Color32, Context, RichText, Ui, Vec2, Pos2, Rect, Stroke, Sense, Response};
use egui_extras::{Column, TableBody, TableBuilder, TableRow};
use eframe::epaint::StrokeKind;
use rand::Rng;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    mpsc, Arc, Mutex,
};

#[derive(Debug, PartialEq)]
enum WhichFile {
    File0,
    File1,
}
fn drop_select_text(selected: bool) -> &'static str {
    if selected {
        "⬇ Loading dropped files here ⬇"
    } else {
        "⬇ Load dropped files here ⬇"
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum DiffMethod {
    ByIndex,
    BpeGreedy00,
    RePairGreedy00,
}

pub struct HexApp {
    source_name0: Option<String>,
    source_name1: Option<String>,
    pattern0: Arc<Mutex<Option<Vec<u8>>>>,
    pattern1: Arc<Mutex<Option<Vec<u8>>>>,
    diffs0: Arc<Mutex<Vec<HexCell>>>,
    diffs1: Arc<Mutex<Vec<HexCell>>>,
    file_drop_target: WhichFile,
    diff_method: DiffMethod,
    update_new_id_rx: Option<mpsc::Receiver<usize>>,
    egui_context: Context,
    job_running: Arc<AtomicBool>,
    cancel_job: Arc<AtomicBool>,
    table_scroll_top: f32,
    document_map_dragging: bool,
}

fn random_pattern() -> Vec<u8> {
    let mut rng = rand::thread_rng();
    (0..4000).map(|_| rng.gen_range(0..=255)).collect()
}

impl HexApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut result = Self {
            source_name0: Some("zeroes0".to_string()),
            source_name1: Some("zeroes1".to_string()),
            pattern0: Arc::new(Mutex::new(Some(vec![0; 1000]))),
            pattern1: Arc::new(Mutex::new(Some(vec![0; 1000]))),
            diffs0: Arc::new(Mutex::new(vec![])),
            diffs1: Arc::new(Mutex::new(vec![])),
            file_drop_target: WhichFile::File0,
            diff_method: DiffMethod::ByIndex,
            update_new_id_rx: None,
            egui_context: cc.egui_ctx.clone(),
            job_running: Arc::new(AtomicBool::new(false)),
            cancel_job: Arc::new(AtomicBool::new(false)),
            table_scroll_top: 0.0,
            document_map_dragging: false,
        };

        result.update_diffs();
        result
    }

    fn try_set_pattern0(&mut self, pattern: Vec<u8>) -> bool {
        if self.job_running.load(Ordering::Acquire) {
            false
        } else {
            let mut pattern0 = self.pattern0.lock().unwrap();
            *pattern0 = Some(pattern);
            true
        }
    }
    fn try_set_pattern1(&mut self, pattern: Vec<u8>) -> bool {
        if self.job_running.load(Ordering::Acquire) {
            false
        } else {
            let mut pattern1 = self.pattern1.lock().unwrap();
            *pattern1 = Some(pattern);
            true
        }
    }

    fn update_diffs(&mut self) {
        if self.job_running.load(Ordering::Acquire) {
            return;
        }
        self.job_running.store(true, Ordering::Release);

        let pattern0 = self.pattern0.clone();
        let pattern1 = self.pattern1.clone();

        let diffs0 = self.diffs0.clone();
        let diffs1 = self.diffs1.clone();

        let diff_method = self.diff_method;
        #[cfg(not(target_arch = "wasm32"))]
        let egui_context = self.egui_context.clone();

        let (tx, rx) = mpsc::channel::<usize>();
        self.update_new_id_rx = Some(rx);

        #[cfg(target_arch = "wasm32")]
        // Spawn an async task to request egui repaints from the main thread.
        // (When attempted from a Web Worker thread, the program panics.)
        let refresh_egui_tx = {
            use futures::{channel::mpsc, StreamExt as _};

            let (tx, mut rx) = mpsc::unbounded::<()>();
            let egui_context = self.egui_context.clone();

            wasm_bindgen_futures::spawn_local(async move {
                while let Some(()) = rx.next().await {
                    egui_context.request_repaint();
                }
                log::info!("loop ENDED");
                // One more repaint after the last worker thread finishes.
                egui_context.request_repaint();
            });

            tx
        };

        let cancel_job = self.cancel_job.clone();
        let worker = move |_s: &rayon::Scope<'_>| {
            let pattern0 = pattern0.lock().unwrap();
            let pattern1 = pattern1.lock().unwrap();

            let request_repaint = || {
                #[cfg(target_arch = "wasm32")]
                refresh_egui_tx.unbounded_send(()).unwrap();

                #[cfg(not(target_arch = "wasm32"))]
                egui_context.request_repaint();
            };

            let (new_diffs0, new_diffs1) =
                if let (Some(pattern0), Some(pattern1)) = (&*pattern0, &*pattern1) {
                    let len = std::cmp::max(pattern0.len(), pattern1.len());
                    match diff_method {
                        DiffMethod::ByIndex => diff::get_diffs(pattern0, pattern1, 0..len),
                        DiffMethod::BpeGreedy00 => {
                            let f = |x| {
                                tx.send(x).unwrap();
                                request_repaint();
                            };
                            println!("starting new_iterative");
                            let mut bpe = Bpe::new_iterative(&[pattern0, pattern1]);
                            println!("finished new_iterative");
                            while bpe.init_in_progress.is_some() {
                                bpe.init_step(Some(f));

                                if cancel_job.load(Ordering::Acquire) {
                                    return;
                                }
                            }

                            let pattern0 = bpe.encode(pattern0);
                            let pattern1 = bpe.encode(pattern1);

                            let matches = matcher::greedy00(&pattern0, &pattern1);
                            test_utils::matches_to_cells(&matches, |x| bpe.decode(x.clone()))
                        }
                        DiffMethod::RePairGreedy00 => {
                            let f = |x| {
                                tx.send(x).unwrap();
                                request_repaint();
                            };
                            
                            let mut re_pair = RePair::new_iterative(&[pattern0, pattern1]);
                            while re_pair.init_in_progress.is_some() {
                                re_pair.init_step(Some(f));

                                if cancel_job.load(Ordering::Acquire) {
                                    return;
                                }
                            }

                            let pattern0 = re_pair.encode(pattern0);
                            let pattern1 = re_pair.encode(pattern1);

                            let matches = matcher::greedy00(&pattern0, &pattern1);
                            test_utils::matches_to_cells(&matches, |x| re_pair.decode(x.clone()))
                        }
                    }
                } else {
                    (vec![], vec![])
                };
            log::info!("started updating diffs");
            {
                let mut diffs0 = diffs0.lock().unwrap();
                *diffs0 = new_diffs0;
            }
            {
                let mut diffs1 = diffs1.lock().unwrap();
                *diffs1 = new_diffs1;
            }
            log::info!("finished updating diffs");

            request_repaint();
        };

        let job_running = self.job_running.clone();
        let cancel_job = self.cancel_job.clone();
        rayon::spawn(move || {
            rayon::scope(|s| {
                s.spawn(worker);
            });
            job_running.store(false, Ordering::Release);
            cancel_job.store(false, Ordering::Release);
        });
    }

    fn add_header_row(&mut self, mut header: TableRow<'_, '_>) {
        let no_pattern = "[none]".to_string();

        header.col(|ui| {
            ui.heading("address");
        });
        header.col(|ui| {
            ui.vertical(|ui| {
                ui.heading(self.source_name0.as_ref().unwrap_or(&no_pattern));
                ui.horizontal(|ui| {
                    let text = drop_select_text(self.file_drop_target == WhichFile::File0);
                    ui.selectable_value(&mut self.file_drop_target, WhichFile::File0, text)
                        .highlight();
                    if ui.button("randomize").clicked() && self.try_set_pattern0(random_pattern()) {
                        self.source_name0 = Some("random".to_string());
                        self.update_diffs();
                    }
                });
            });
        });
        header.col(|_| {});
        header.col(|ui| {
            ui.vertical(|ui| {
                ui.heading(self.source_name1.as_ref().unwrap_or(&no_pattern));
                ui.horizontal(|ui| {
                    let text = drop_select_text(self.file_drop_target == WhichFile::File1);
                    ui.selectable_value(&mut self.file_drop_target, WhichFile::File1, text)
                        .highlight();
                    if ui.button("randomize").clicked() && self.try_set_pattern1(random_pattern()) {
                        self.source_name1 = Some("random".to_string());
                        self.update_diffs();
                    }
                });
            });
        });
    }

    fn draw_document_map(&mut self, ui: &mut Ui, _table_rect: Rect, visible_rows: usize, total_rows: usize) -> Response {
        let map_width = 120.0;
        let map_height = 300.0;
        
        let (response, painter) = ui.allocate_painter(Vec2::new(map_width, map_height), Sense::click_and_drag());
        let map_rect = response.rect;
        
        // Draw background
        painter.rect_filled(map_rect, 0.0, Color32::from_gray(40));
        painter.rect_stroke(map_rect, 0.0, Stroke::new(1.0, Color32::from_gray(100)), StrokeKind::Outside);
        
        if total_rows == 0 {
            return response;
        }
        
        // Get diffs data
        let diffs0 = if let Ok(diffs0) = self.diffs0.try_lock() { diffs0 } else { return response; };
        let diffs1 = if let Ok(diffs1) = self.diffs1.try_lock() { diffs1 } else { return response; };
        
        let hex_grid_width = 16;
        let max_len = std::cmp::max(diffs0.len(), diffs1.len());
        let actual_total_rows = (max_len + hex_grid_width - 1) / hex_grid_width;
        
        if actual_total_rows == 0 {
            return response;
        }
        
        // Calculate scale factors
        let pixels_per_row = map_height / actual_total_rows as f32;
        let file_width = map_width / 2.0;
        
        // Draw miniature representation of files
        for row in 0..actual_total_rows {
            let y = map_rect.top() + row as f32 * pixels_per_row;
            let row_height = pixels_per_row.max(1.0);
            
            // Draw file 0 (left side)
            let mut has_diff0 = false;
            for col in 0..hex_grid_width {
                let index = row * hex_grid_width + col;
                if let Some(cell) = diffs0.get(index) {
                    if matches!(cell, HexCell::Diff { .. }) {
                        has_diff0 = true;
                        break;
                    }
                }
            }
            
            let color0 = if has_diff0 { Color32::from_rgb(255, 100, 100) } else { Color32::from_gray(80) };
            painter.rect_filled(
                Rect::from_min_size(
                    Pos2::new(map_rect.left(), y),
                    Vec2::new(file_width - 1.0, row_height)
                ),
                0.0,
                color0
            );
            
            // Draw file 1 (right side)
            let mut has_diff1 = false;
            for col in 0..hex_grid_width {
                let index = row * hex_grid_width + col;
                if let Some(cell) = diffs1.get(index) {
                    if matches!(cell, HexCell::Diff { .. }) {
                        has_diff1 = true;
                        break;
                    }
                }
            }
            
            let color1 = if has_diff1 { Color32::from_rgb(255, 100, 100) } else { Color32::from_gray(80) };
            painter.rect_filled(
                Rect::from_min_size(
                    Pos2::new(map_rect.left() + file_width + 1.0, y),
                    Vec2::new(file_width - 1.0, row_height)
                ),
                0.0,
                color1
            );
        }
        
        // Calculate current view position
        let view_start_row = (self.table_scroll_top / 18.0) as usize; // 18.0 is row height
        let _view_end_row = view_start_row + visible_rows;
        
        // Draw viewport overlay
        if actual_total_rows > 0 {
            let viewport_start_y = map_rect.top() + (view_start_row as f32 * pixels_per_row);
            let viewport_height = (visible_rows as f32 * pixels_per_row).min(map_height - (viewport_start_y - map_rect.top()));
            
            let viewport_rect = Rect::from_min_size(
                Pos2::new(map_rect.left(), viewport_start_y),
                Vec2::new(map_width, viewport_height)
            );
            
            painter.rect_stroke(viewport_rect, 0.0, Stroke::new(2.0, Color32::from_rgb(100, 150, 255)), StrokeKind::Outside);
            painter.rect_filled(viewport_rect, 0.0, Color32::from_rgba_unmultiplied(100, 150, 255, 30));
        }
        
        // Handle interaction
        if response.clicked() {
            if let Some(click_pos) = response.interact_pointer_pos() {
                let relative_y = click_pos.y - map_rect.top();
                let clicked_row = (relative_y / pixels_per_row) as usize;
                
                // Update scroll position to center the clicked row
                self.table_scroll_top = (clicked_row as f32 * 18.0) - (visible_rows as f32 * 18.0 * 0.5);
                self.table_scroll_top = self.table_scroll_top.max(0.0);
            }
        }
        
        if response.dragged() {
            self.document_map_dragging = true;
            let drag_delta = response.drag_delta();
            let row_delta = drag_delta.y / pixels_per_row;
            self.table_scroll_top += row_delta * 18.0;
            self.table_scroll_top = self.table_scroll_top.max(0.0);
        }
        
        if response.drag_stopped() {
            self.document_map_dragging = false;
        }
        
        response
    }

    fn add_body_contents(&self, body: TableBody<'_>) {
        fn color(c: usize) -> Color32 {
            let hi: u8 = 255;
            let lo: u8 = 128;
            match c % 6 {
                0 => Color32::from_rgb(hi, lo, lo),
                1 => Color32::from_rgb(hi, hi, lo),
                2 => Color32::from_rgb(lo, hi, lo),
                3 => Color32::from_rgb(lo, hi, hi),
                4 => Color32::from_rgb(lo, lo, hi),
                5 => Color32::from_rgb(hi, lo, hi),
                _ => unreachable!(),
            }
        }
        fn contrast(color: Color32) -> Color32 {
            Color32::from_rgb(
                u8::wrapping_add(color.r(), 128),
                u8::wrapping_add(color.g(), 128),
                u8::wrapping_add(color.b(), 128),
            )
        }

        let diffs0 = if let Ok(diffs0) = self.diffs0.try_lock() {
            diffs0
        } else {
            return;
        };

        let diffs1 = if let Ok(diffs1) = self.diffs1.try_lock() {
            diffs1
        } else {
            return;
        };

        let hex_grid_width = 16;

        let row_height = 18.0;
        let num_rows = 1 + std::cmp::max(diffs0.len(), diffs1.len()) / hex_grid_width;

        body.rows(row_height, num_rows, |mut row| {
            let row_index = row.index();

            let add_hex_row = |ui: &mut Ui, diffs: &Vec<HexCell>| {
                (0..hex_grid_width).for_each(|i| {
                    let cell = diffs.get(i + row_index * hex_grid_width);

                    match cell {
                        Some(&HexCell::Same { value, source_id }) => ui.label(
                            RichText::new(format!("{value:02X}"))
                                .color(color(source_id))
                                .monospace(),
                        ),
                        Some(&HexCell::Diff { value, source_id }) => {
                            let color = color(source_id);
                            let contrast = contrast(color);
                            ui.label(
                                RichText::new(format!("{value:02X}"))
                                    .color(contrast)
                                    .background_color(color)
                                    .monospace(),
                            )
                        }

                        Some(&HexCell::Blank) => ui.monospace("__"),
                        None => ui.monospace("xx"),
                    };
                });
            };

            let add_ascii_row = |ui: &mut Ui, diffs: &Vec<HexCell>| {
                (0..hex_grid_width).for_each(|i| {
                    let cell = diffs.get(i + row_index * hex_grid_width);

                    match cell {
                        Some(&HexCell::Same { value, source_id }) => ui.label(
                            RichText::new(format!("{}", value as char))
                                .color(color(source_id))
                                .monospace(),
                        ),
                        Some(&HexCell::Diff { value, source_id }) => {
                            let color = color(source_id);
                            let contrast = contrast(color);

                            ui.label(
                                RichText::new(format!("{}", value as char))
                                    .color(contrast)
                                    .background_color(color)
                                    .monospace(),
                            )
                        }
                        Some(&HexCell::Blank) => ui.monospace("_"),
                        None => ui.monospace("x"),
                    };
                });
            };

            row.col(|ui| {
                ui.label(RichText::new(format!("{:08X}", row_index * hex_grid_width)).monospace());
            });
            row.col(|ui| add_hex_row(ui, &diffs0));
            row.col(|ui| add_ascii_row(ui, &diffs0));
            row.col(|ui| add_hex_row(ui, &diffs1));
            row.col(|ui| add_ascii_row(ui, &diffs1));
        });
    }
}

impl eframe::App for HexApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let mut new_id = None;
        if let Some(rx) = &mut self.update_new_id_rx {
            for x in rx.try_iter() {
                new_id = Some(x);
            }
        }

        ctx.input(|i| {
            if let Some(dropped_file) = i.raw.dropped_files.first() {
                // This should only be Some when running as a native app.
                if let Some(path) = &dropped_file.path {
                    if let Ok(pattern) = std::fs::read(path) {
                        match self.file_drop_target {
                            WhichFile::File0 => {
                                if self.try_set_pattern0(pattern) {
                                    self.source_name0 = Some(path.to_string_lossy().to_string());
                                }
                            }
                            WhichFile::File1 => {
                                if self.try_set_pattern1(pattern) {
                                    self.source_name1 = Some(path.to_string_lossy().to_string());
                                }
                            }
                        }
                    } else {
                        log::error!("failed to read file: {path:?}");
                    }
                }
                // This should only be Some when running as a web app.
                else if let Some(bytes) = &dropped_file.bytes {
                    match self.file_drop_target {
                        WhichFile::File0 => {
                            if self.try_set_pattern0(bytes.to_vec()) {
                                self.source_name0 = Some(dropped_file.name.clone());
                            }
                        }
                        WhichFile::File1 => {
                            if self.try_set_pattern1(bytes.to_vec()) {
                                self.source_name1 = Some(dropped_file.name.clone());
                            }
                        }
                    }
                }
                self.update_diffs();
            }
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.heading("hex diff test (egui UI)");

                ui.label("diff method:");
                use DiffMethod::*;
                if ui
                    .selectable_value(&mut self.diff_method, ByIndex, "By Index")
                    .clicked()
                {
                    self.update_diffs();
                }

                if ui
                    .selectable_value(&mut self.diff_method, BpeGreedy00, "BPE Greedy 00")
                    .clicked()
                {
                    self.update_diffs();
                }

                if ui
                    .selectable_value(&mut self.diff_method, RePairGreedy00, "RePair Greedy 00")
                    .clicked()
                {
                    self.update_diffs();
                }

                ui.menu_button("Load Test", |ui| {
                    type PatternFunc = fn() -> (Vec<u8>, Vec<u8>);

                    let buttons: &[(_, PatternFunc)] = &[
                        ("trivial", test_patterns::trivial),
                        ("random_1k", test_patterns::random_1k),
                        ("random_256", test_patterns::random_256),
                        ("random_minus_block", test_patterns::random_minus_block),
                        ("zeroes_minus_block", test_patterns::zeroes_minus_block),
                        (
                            "random_10k_minus_block",
                            test_patterns::random_10k_minus_block,
                        ),
                    ];

                    for (name, f) in buttons {
                        if ui.button(*name).clicked() {
                            let (pattern0, pattern1) = f();

                            if self.try_set_pattern0(pattern0) {
                                self.source_name0 = Some(format!("test pattern 0: {name}"));
                            }
                            if self.try_set_pattern1(pattern1) {
                                self.source_name1 = Some(format!("test pattern 1: {name}"));
                            }
                            self.update_diffs();
                            ui.close_menu();
                        }
                    }
                });

                if ui
                    .add_enabled(
                        self.job_running.load(Ordering::Acquire),
                        egui::Button::new("cancel"),
                    )
                    .clicked()
                {
                    self.cancel_job.store(true, Ordering::Release);
                }

                //display the new id
                ui.label(RichText::new(format!("new id: {new_id:?}")));
            });

            // Calculate total rows for document map
            let total_rows = {
                let diffs0 = if let Ok(diffs0) = self.diffs0.try_lock() { diffs0 } else { return; };
                let diffs1 = if let Ok(diffs1) = self.diffs1.try_lock() { diffs1 } else { return; };
                let hex_grid_width = 16;
                1 + std::cmp::max(diffs0.len(), diffs1.len()) / hex_grid_width
            };

            ui.horizontal(|ui| {
                // Table on the left
                let table_output = TableBuilder::new(ui)
                    .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                    .striped(true)
                    .column(Column::auto().resizable(true))
                    .column(Column::auto().resizable(true))
                    .column(Column::auto().resizable(true))
                    .column(Column::auto().resizable(true))
                    .column(Column::remainder())
                    .vertical_scroll_offset(self.table_scroll_top)
                    .header(20.0, |header| self.add_header_row(header))
                    .body(|body| {
                        self.add_body_contents(body);
                    });

                // Update scroll position if table was scrolled by user
                if !self.document_map_dragging {
                    let current_scroll = table_output.state.offset.y;
                    if current_scroll.is_finite() {
                        self.table_scroll_top = current_scroll;
                    }
                }

                // Calculate visible rows based on available height
                let row_height = 18.0;
                let header_height = 20.0;
                let available_height = ui.available_height() - header_height;
                let visible_rows = (available_height / row_height).max(1.0) as usize;

                // Document map on the right
                ui.separator();
                ui.vertical(|ui| {
                    ui.label("Document Map");
                    self.draw_document_map(ui, table_output.inner_rect, visible_rows, total_rows);
                });
            });
        });
    }
}
