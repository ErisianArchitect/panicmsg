use std::fmt::Display;

/// A panic message that can be initialized at compile time and used for panics with
/// specific error messages that are reuseable.
pub struct PanicMsg<M: Display = &'static str> {
    message: M,
}

impl<M: Display> PanicMsg<M> {
    pub const fn new(message: M) -> Self {
        Self { message }
    }

    /// Panic with inner message.
    #[track_caller]
    pub fn panic(&self) -> ! {
        panic!("{}", self.message);
    }

    #[track_caller]
    pub fn debug_panic(&self) {
        if cfg!(debug_assertions) {
            panic!("{}", self.message);
        }
    }

    /// Panic if the condition is met.
    #[track_caller]
    pub fn panic_if(&self, condition: bool) {
        if condition {
            panic!("{}", self.message);
        }
    }

    #[track_caller]
    pub fn debug_panic_if(&self, condition: bool) {
        #[cfg(debug_assertions)]
        if condition {
            panic!("{}", self.message);
        }
    }

    /// Assert the condition.
    #[track_caller]
    pub fn assert(&self, condition: bool) {
        assert!(condition, "{}", self.message);
    }

    /// Assert if two expressions are equal.
    #[track_caller]
    pub fn assert_eq<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug
    {
        assert_eq!(lhs, rhs, "{}", self.message);
    }

    /// Assert if two expressions are not equal.
    #[track_caller]
    pub fn assert_ne<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug
    {
        assert_ne!(lhs, rhs, "{}", self.message);
    }

    /// Assert condition in debug mode.
    #[track_caller]
    pub fn debug_assert(&self, condition: bool) {
        debug_assert!(condition, "{}", self.message);
    }

    /// Assert two expressions are equal in debug mode.
    #[track_caller]
    pub fn debug_assert_eq<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug
    {
        debug_assert_eq!(lhs, rhs, "{}", self.message);
    }

    /// Assert two expressions are not equal in debug mode.
    #[track_caller]
    pub fn debug_assert_ne<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug,
    {
        debug_assert_ne!(lhs, rhs, "{}", self.message);
    }

    /// Expect an [Option<T>] value.
    #[track_caller]
    pub fn expect_opt<T>(&self, value: Option<T>) -> T {
        let Some(value) = value else {
            panic!("(Expect failed): {}", self.message);
        };
        value
    }

    /// Expect a [Result<T, E>] value.
    #[track_caller]
    pub fn expect_result<T, E>(&self, value: Result<T, E>) -> T {
        let Ok(value) = value else {
            panic!("(Expect failed): {}", self.message);
        };
        value
    }

    /// Resturn a reference to the contained message.
    pub fn msg(&self) -> &M {
        &self.message
    }

}

// Trait implementations.

impl<M: Display + Clone> Clone for PanicMsg<M> {
    fn clone(&self) -> Self {
        Self {
            message: self.message.clone()
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.message = source.message.clone();
    }
}

impl<M: Display + Copy> Copy for PanicMsg<M> {}

impl<M: Display + PartialEq> PartialEq<M> for PanicMsg<M> {
    fn eq(&self, other: &M) -> bool {
        self.message == *other
    }

    fn ne(&self, other: &M) -> bool {
        self.message != *other
    }
}

impl<M: Display + PartialEq> PartialEq<PanicMsg<M>> for PanicMsg<M> {
    fn eq(&self, other: &PanicMsg<M>) -> bool {
        self.message == other.message
    }

    fn ne(&self, other: &PanicMsg<M>) -> bool {
        self.message != other.message
    }
}

impl<M: Display + Eq> Eq for PanicMsg<M> {}

impl<M: Display + PartialOrd> PartialOrd<M> for PanicMsg<M> {
    fn partial_cmp(&self, other: &M) -> Option<std::cmp::Ordering> {
        self.message.partial_cmp(other)
    }
}

impl<M: Display + PartialOrd> PartialOrd<PanicMsg<M>> for PanicMsg<M> {
    fn partial_cmp(&self, other: &PanicMsg<M>) -> Option<std::cmp::Ordering> {
        self.message.partial_cmp(&other.message)
    }
}

impl<M: Display + Ord> Ord for PanicMsg<M> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.message.cmp(&other.message)
    }
}

impl<M: Display + std::hash::Hash> std::hash::Hash for PanicMsg<M> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.message.hash(state);
    }

    fn hash_slice<H: std::hash::Hasher>(data: &[Self], state: &mut H)
        where
            Self: Sized, {
        unsafe {
            let trans_data: &[M] = std::mem::transmute(data);
            M::hash_slice(trans_data, state);
        }
    }
}

#[macro_export]
macro_rules! const_panic_msg {
    ($visibility:vis $name:ident = $msg:literal) => {
        $visibility const $name: PanicMsg<&'static str> = PanicMsg::new($msg);
    };
}

#[cfg(test)]
mod tests {
    #![allow(unused)]
    const_panic_msg!(TEST_PANIC = "Test panic.");
    use super::*;

    #[should_panic]
    #[test]
    fn panic_test() {
        
        TEST_PANIC.panic();
    }
}
