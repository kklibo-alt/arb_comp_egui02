pub trait ToPairs<T, U>
where
    T: Iterator<Item = U>,
    U: Copy,
{
    fn pairs(self) -> Pairs<T, U>;
}

impl<T, U> ToPairs<T, U> for T
where
    T: Iterator<Item = U>,
    U: Copy,
{
    fn pairs(self) -> Pairs<T, U> {
        Pairs::new(self)
    }
}
pub struct Pairs<T, U>
where
    T: Iterator<Item = U>,
    U: Copy,
{
    iter: std::iter::Peekable<T>,
    r#final: Option<U>,
}

impl<T, U> Pairs<T, U>
where
    T: Iterator<Item = U>,
    U: Copy,
{
    pub fn new(iter: T) -> Self {
        Self {
            iter: iter.peekable(),
            r#final: None,
        }
    }

    pub fn final_item(&self) -> Option<U> {
        self.r#final
    }
}

impl<T, U> Iterator for Pairs<T, U>
where
    T: Iterator<Item = U>,
    U: Copy,
{
    type Item = (U, U);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(id0) = self.iter.next() {
            if let Some(&id1) = self.iter.peek() {
                Some((id0, id1))
            } else {
                self.r#final = Some(id0);
                None
            }
        } else {
            self.r#final = None;
            None
        }
    }
}
