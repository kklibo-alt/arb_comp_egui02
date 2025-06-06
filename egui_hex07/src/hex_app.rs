use crate::diff::{self, HexCell};
use arb_comp06::{bpe::Bpe, matcher, re_pair::RePair, test_patterns, test_utils};
use egui::{
    Color32, ColorImage, Context, Pos2, Rect, Response, RichText, Sense, Stroke, StrokeKind,
    TextureHandle, TextureOptions, Ui, Vec2,
};
use egui_extras::{Column, TableBody, TableBuilder, TableRow};
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

#[derive(Debug, Default, Copy, Clone)]
/// Represents an active mousedrag on a scrolling object in the UI.
struct ScrollDrag {
    /// The mouse position when the drag started.
    start_pos: Pos2,
    /// The scroll value when the drag started.
    start_scroll: Ratio,
}

#[derive(Debug, Default, Copy, Clone)]
/// This value is a unitless ratio of something else.
struct Ratio(f32);

impl Ratio {
    fn valid_or_zero(ratio: f32) -> Self {
        Self(if ratio.is_finite() { ratio } else { 0.0 })
    }
}

#[derive(Debug, Default)]
/// Models a rectangular document area with a vertically-sliding view window.
struct DocumentViewState {
    /// The distance from the top of the document to the top of the
    /// view window, as a proportion of document height.
    scroll_from_top: Ratio,
    /// The height of the view window, as a proportion of document height.
    window_height: Ratio,
}

impl DocumentViewState {
    pub fn ratio_from_height(height: f32, document_rect: Rect) -> Ratio {
        Ratio::valid_or_zero(height / document_rect.height())
    }

    pub fn ratio_from_pos(pos: Pos2, document_rect: Rect) -> Ratio {
        Self::ratio_from_height(pos.y - document_rect.top(), document_rect)
    }

    pub fn scroll_from_top(&self) -> Ratio {
        self.scroll_from_top
    }

    pub fn window_height(&self) -> Ratio {
        self.window_height
    }

    /// The view window on a full document represented by `document_rect`.
    pub fn view_window(&self, document_rect: Rect) -> Rect {
        let top = document_rect.top() + document_rect.height() * self.scroll_from_top.0;
        let bottom = top + document_rect.height() * self.window_height.0;

        Rect::from_min_max(
            Pos2::new(document_rect.left(), top),
            Pos2::new(document_rect.right(), bottom),
        )
    }

    pub fn set_view_window_scroll(&mut self, scroll_from_top: Ratio) {
        self.set_view_window(scroll_from_top, self.window_height);
    }

    pub fn set_view_window(&mut self, scroll_from_top: Ratio, window_height: Ratio) {
        self.scroll_from_top = scroll_from_top;
        self.window_height = window_height;

        // Eventually: decide how to handle view windows that exceed document bounds.
    }

    pub fn is_in_view_window(&self, document_rect: Rect, pos: Pos2) -> bool {
        self.view_window(document_rect).contains(pos)
    }

    pub fn center_view_window(&mut self, center_on: Ratio) {
        self.scroll_from_top = Ratio(center_on.0 - 0.5 * self.window_height.0)
    }
}

pub struct HexApp {
    source_name0: Option<String>,
    source_name1: Option<String>,
    pattern0: Arc<Mutex<Option<Vec<u8>>>>,
    pattern1: Arc<Mutex<Option<Vec<u8>>>>,
    diffs0: Arc<Mutex<Vec<HexCell>>>,
    diffs1: Arc<Mutex<Vec<HexCell>>>,
    diffs_texture0: TextureHandle,
    diffs_texture1: TextureHandle,
    file_drop_target: WhichFile,
    diff_method: DiffMethod,
    update_new_id_rx: Option<mpsc::Receiver<usize>>,
    egui_context: Context,
    job_running: Arc<AtomicBool>,
    cancel_job: Arc<AtomicBool>,
    document_view_state: DocumentViewState,
    table_height: f32,
    document_map_drag: Option<ScrollDrag>,
}

fn random_pattern() -> Vec<u8> {
    let mut rng = rand::thread_rng();
    (0..4000).map(|_| rng.gen_range(0..=255)).collect()
}

impl HexApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let texture_handle0 =
            cc.egui_ctx
                .load_texture("document_map0", ColorImage::default(), Default::default());
        let texture_handle1 =
            cc.egui_ctx
                .load_texture("document_map1", ColorImage::default(), Default::default());

        let mut result = Self {
            source_name0: Some("zeroes0".to_string()),
            source_name1: Some("zeroes1".to_string()),
            pattern0: Arc::new(Mutex::new(Some(vec![0; 1000]))),
            pattern1: Arc::new(Mutex::new(Some(vec![0; 1000]))),
            diffs0: Arc::new(Mutex::new(vec![])),
            diffs1: Arc::new(Mutex::new(vec![])),
            diffs_texture0: texture_handle0,
            diffs_texture1: texture_handle1,
            file_drop_target: WhichFile::File0,
            diff_method: DiffMethod::ByIndex,
            update_new_id_rx: None,
            egui_context: cc.egui_ctx.clone(),
            job_running: Arc::new(AtomicBool::new(false)),
            cancel_job: Arc::new(AtomicBool::new(false)),
            document_view_state: DocumentViewState::default(),
            table_height: 0.0,
            document_map_drag: None,
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

        let mut diffs_texture0 = self.diffs_texture0.clone();
        let mut diffs_texture1 = self.diffs_texture1.clone();

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

            fn cells_to_image(cells: &[HexCell]) -> ColorImage {
                let columns = 16;
                let rows = cells.len().div_ceil(columns);

                let mut color_image = ColorImage::new([columns, rows], Color32::TRANSPARENT);

                for (&h, p) in cells.iter().zip(color_image.pixels.iter_mut()) {
                    match h {
                        HexCell::Same { value, source_id } => {
                            //*p = Color32::from_rgba_premultiplied(0, 0, 16 * (value % 16), 128);
                            *p = HexApp::color(source_id);
                        }
                        HexCell::Diff { value, source_id } => {
                            //*p = Color32::from_rgba_premultiplied(16 * (value % 16), 0, 0, 128);
                            let color = HexApp::color(source_id);
                            let contrast = HexApp::contrast(color);
                            *p = contrast;
                        }
                        HexCell::Blank => {
                            *p = Color32::from_rgba_premultiplied(0, 0, 0, 128);
                        }
                    }
                }
                color_image
            }

            let color_image0 = cells_to_image(&new_diffs0);
            let color_image1 = cells_to_image(&new_diffs1);

            // Can this block the main thread?
            diffs_texture0.set(
                color_image0,
                TextureOptions {
                    magnification: egui::TextureFilter::Nearest,
                    ..Default::default()
                },
            );
            diffs_texture1.set(
                color_image1,
                TextureOptions {
                    magnification: egui::TextureFilter::Nearest,
                    ..Default::default()
                },
            );

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

    fn add_body_contents(&self, body: TableBody<'_>) {
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
                                .color(Self::color(source_id))
                                .monospace(),
                        ),
                        Some(&HexCell::Diff { value, source_id }) => {
                            let color = Self::color(source_id);
                            let contrast = Self::contrast(color);
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

                    fn clean_ascii(value: u8) -> char {
                        let ch = value as char;
                        if ch.is_ascii_control() || ch.is_ascii_whitespace() {
                            ' '
                        } else {
                            ch
                        }
                    }

                    match cell {
                        Some(&HexCell::Same { value, source_id }) => ui.label(
                            RichText::new(format!("{}", clean_ascii(value)))
                                .color(Self::color(source_id))
                                .monospace(),
                        ),
                        Some(&HexCell::Diff { value, source_id }) => {
                            let color = Self::color(source_id);
                            let contrast = Self::contrast(color);

                            ui.label(
                                RichText::new(format!("{}", clean_ascii(value)))
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

        egui::SidePanel::right("document_map_panel")
            .exact_width(250.0)
            .show(ctx, |ui| {
                draw_document_map(
                    ui,
                    self.diffs_texture0.clone(),
                    self.diffs_texture1.clone(),
                    &mut self.document_view_state,
                    &mut self.document_map_drag,
                );
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

            egui::ScrollArea::horizontal().show(ui, |ui| {
                let scroll_area_output = TableBuilder::new(ui)
                    .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                    .striped(true)
                    .column(Column::auto().resizable(true))
                    .column(Column::auto().resizable(true))
                    .column(Column::auto().resizable(true))
                    .column(Column::auto().resizable(true))
                    .column(Column::remainder())
                    .vertical_scroll_offset(
                        self.table_height * self.document_view_state.scroll_from_top().0,
                    )
                    .header(20.0, |header| self.add_header_row(header))
                    .body(|body| self.add_body_contents(body));

                let table_height = scroll_area_output.content_size.y;
                self.table_height = table_height;

                let scroll_from_top =
                    Ratio::valid_or_zero(scroll_area_output.state.offset.y / table_height);
                let view_window_height =
                    Ratio::valid_or_zero(scroll_area_output.inner_rect.height() / table_height);

                self.document_view_state
                    .set_view_window(scroll_from_top, view_window_height);
            });
        });
    }
}

fn draw_document_map(
    ui: &mut Ui,
    texture_h0: TextureHandle,
    texture_h1: TextureHandle,
    document_view_state: &mut DocumentViewState,
    view_window_drag: &mut Option<ScrollDrag>,
) -> Response {
    let draw_rect = ui.max_rect();

    let (response, painter) = ui.allocate_painter(draw_rect.size(), Sense::click_and_drag());

    if response.clicked() {
        if let Some(pos) = response.interact_pointer_pos() {
            if !document_view_state.is_in_view_window(draw_rect, pos) {
                let center = DocumentViewState::ratio_from_pos(pos, draw_rect);
                document_view_state.center_view_window(center);
            }
        }
    }

    if response.drag_started() {
        if let Some(pos) = response.interact_pointer_pos() {
            if document_view_state.is_in_view_window(draw_rect, pos) {
                *view_window_drag = Some(ScrollDrag {
                    start_pos: pos,
                    start_scroll: document_view_state.scroll_from_top(),
                });
            }
        }
    }
    if response.dragged() {
        if let Some(pos) = response.interact_pointer_pos() {
            if let Some(ScrollDrag {
                start_pos,
                start_scroll,
            }) = view_window_drag
            {
                let drag_scroll =
                    DocumentViewState::ratio_from_height(pos.y - start_pos.y, draw_rect);

                document_view_state.set_view_window_scroll(Ratio(drag_scroll.0 + start_scroll.0));
            }
        }
    }
    if response.drag_stopped() {
        *view_window_drag = None;
    }

    painter.debug_rect(draw_rect, Color32::RED, "document_map");

    painter.rect_stroke(
        draw_rect,
        10.0,
        Stroke::new(1.0, Color32::ORANGE),
        StrokeKind::Inside,
    );

    let (mut left, mut right) = draw_rect.split_left_right_at_fraction(0.5);
    *left.right_mut() -= 1.0;
    *right.left_mut() += 1.0;

    egui::Image::new(&texture_h0).paint_at(ui, left);
    egui::Image::new(&texture_h1).paint_at(ui, right);

    let bar_rect = document_view_state.view_window(draw_rect);
    painter.rect_filled(bar_rect, 10.0, Color32::from_white_alpha(32));

    response
}
