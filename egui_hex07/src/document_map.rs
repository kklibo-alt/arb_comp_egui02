use egui::{Color32, Pos2, Rect, Response, Sense, Stroke, StrokeKind, TextureHandle, Ui};

#[derive(Debug, Default, Copy, Clone)]
/// This value is a unitless ratio of something else.
pub struct Ratio(pub f32);

impl Ratio {
    pub fn valid_or_zero(ratio: f32) -> Self {
        Self(if ratio.is_finite() { ratio } else { 0.0 })
    }
}

#[derive(Debug, Default)]
/// Models a rectangular document area with a vertically-sliding view window.
pub struct DocumentViewState {
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

    #[allow(dead_code)]
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

#[derive(Debug, Default, Copy, Clone)]
/// Represents an active mousedrag on a scrolling object in the UI.
pub struct ScrollDrag {
    /// The mouse position when the drag started.
    start_pos: Pos2,
    /// The scroll value when the drag started.
    start_scroll: Ratio,
}

pub struct DocumentMap {
    texture_handle0: TextureHandle,
    texture_handle1: TextureHandle,
    pub document_view_state: DocumentViewState,
    view_window_drag: Option<ScrollDrag>,
}

impl DocumentMap {
    pub fn new(texture_handle0: TextureHandle, texture_handle1: TextureHandle) -> Self {
        Self {
            texture_handle0,
            texture_handle1,
            document_view_state: Default::default(),
            view_window_drag: Default::default(),
        }
    }

    pub fn draw(&mut self, ui: &mut Ui, draw_rect_height_pts: &mut f32) -> Response {
        let draw_rect = ui.max_rect();
        *draw_rect_height_pts = draw_rect.height();

        let (response, painter) = ui.allocate_painter(draw_rect.size(), Sense::click_and_drag());

        if let Some(pos) = response.interact_pointer_pos() {
            if (response.clicked() || response.drag_started())
                && !self.document_view_state.is_in_view_window(draw_rect, pos)
            {
                let center = DocumentViewState::ratio_from_pos(pos, draw_rect);
                self.document_view_state.center_view_window(center);
            }
            if response.drag_started() {
                self.view_window_drag = Some(ScrollDrag {
                    start_pos: pos,
                    start_scroll: self.document_view_state.scroll_from_top(),
                });
            }
            if response.dragged() {
                if let Some(ScrollDrag {
                    start_pos,
                    start_scroll,
                }) = self.view_window_drag
                {
                    let drag_scroll =
                        DocumentViewState::ratio_from_height(pos.y - start_pos.y, draw_rect);

                    self.document_view_state
                        .set_view_window_scroll(Ratio(drag_scroll.0 + start_scroll.0));
                }
            }
        }
        if response.drag_stopped() {
            self.view_window_drag = None;
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

        egui::Image::new(&self.texture_handle0).paint_at(ui, left);
        egui::Image::new(&self.texture_handle1).paint_at(ui, right);

        let bar_rect = self.document_view_state.view_window(draw_rect);
        painter.rect_filled(bar_rect, 10.0, Color32::from_white_alpha(32));

        response
    }
}
