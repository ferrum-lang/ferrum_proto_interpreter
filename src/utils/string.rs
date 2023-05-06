pub trait StringUtils {
    fn char_at(&self, index: usize) -> Option<char>;
}

impl StringUtils for String {
    fn char_at(&self, index: usize) -> Option<char> {
        return self.as_str().char_at(index);
    }
}

impl<'a> StringUtils for &'a str {
    fn char_at(&self, index: usize) -> Option<char> {
        if index >= self.len() {
            return None;
        }

        return self.chars().skip(index).next();
    }
}
