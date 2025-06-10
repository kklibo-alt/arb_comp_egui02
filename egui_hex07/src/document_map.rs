use egui::{Pos2, Rect};

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
