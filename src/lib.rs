//! This crate provides reusable error messages ([PanicMsg]) for use with 
//! panics, assertions (`assert`, `assert_eq`, `assert_ne`), and `expect`.
//! It also includes debug versions of each of these methods.
//! 
//! Simply declare a [PanicMsg] in your code:
//! ```rust, no_run
//! const EXAMPLE_PANIC: PanicMsg = PanicMsg::new("This is an example panic message.");
//! ```
//! Then you can use it like this:
//! ```rust, no_run
// ...
//! EXAMPLE_PANIC.panic();
//! // ...
//! EXAMPLE_PANIC.panic_if(left >= right);
//! // ...
//! EXAMPLE_PANIC.assert(left < right);
//! // ...
//! EXAMPLE_PANIC.assert_eq(left, right);
//! // ...
//! EXAMPLE_PANIC.assert_ne(left, right);
//! // ...
//! EXAMPLE_PANIC.expect_opt(option_value);
//! // ...
//! EXAMPLE_PANIC.expect_result(result);
//! // ...
//! EXAMPLE_PANIC.debug_panic();
//! // ...
//! EXAMPLE_PANIC.debug_panic_if(left >= right);
//! // ...
//! EXAMPLE_PANIC.debug_assert(left < right);
//! // ...
//! EXAMPLE_PANIC.debug_assert_eq(left, right);
//! // ...
//! EXAMPLE_PANIC.debug_assert_ne(left, right);
//! // ...
//! EXAMPLE_PANIC.debug_expect_opt(option_value);
//! // ...
//! EXAMPLE_PANIC.debug_expect_result(result);
//! ```

/// A compile-time initialized panic message that can be reused with specific error messages
/// for panics, allowing for consistent and reusable error reporting.
/// 
/// # Example
/// ```rust, no_run
/// const EXAMPLE_PANIC: PanicMsg = PanicMsg::new("This is an example panic message.");
/// // ...
/// EXAMPLE_PANIC.panic();
/// // ...
/// EXAMPLE_PANIC.panic_if(left >= right);
/// // ...
/// EXAMPLE_PANIC.assert(left < right);
/// // ...
/// EXAMPLE_PANIC.assert_eq(left, right);
/// // ...
/// EXAMPLE_PANIC.assert_ne(left, right);
/// // ...
/// EXAMPLE_PANIC.expect_opt(option_value);
/// // ...
/// EXAMPLE_PANIC.expect_result(result);
/// ```
/// There are also `debug` variants of each of these methods.
pub struct PanicMsg<M: std::fmt::Display = &'static str> {
    message: M,
}

impl<M: std::fmt::Display> PanicMsg<M> {
    pub const fn new(message: M) -> Self {
        Self { message }
    }

    /// Panic at runtime.
    /// 
    /// See [panic].
    #[track_caller]
    pub fn panic(&self) -> ! {
        panic!("{}", self.message);
    }

    /// Panic if the condition is `true` at runtime.
    /// 
    /// See [panic].
    #[track_caller]
    pub fn panic_if(&self, condition: bool) {
        if condition {
            panic!("{}", self.message);
        }
    }

    /// Asserts that a boolean expression is `true` at runtime.
    /// 
    /// see [assert].
    #[track_caller]
    pub fn assert(&self, condition: bool) {
        assert!(condition, "{}", self.message);
    }

    /// Assert that two expressions are equal to each other (using [PartialEq]).
    /// 
    /// See [assert_eq].
    #[track_caller]
    pub fn assert_eq<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug
    {
        assert_eq!(lhs, rhs, "{}", self.message);
    }

    /// Assert that two expressions are not equal to each other (using [PartialEq]).
    /// 
    /// See [assert_ne].
    #[track_caller]
    pub fn assert_ne<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug
    {
        assert_ne!(lhs, rhs, "{}", self.message);
    }

    /// Panic at runtime with `debug_assertions`.
    /// 
    /// See [panic].
    #[track_caller]
    pub fn debug_panic(&self) {
        if cfg!(debug_assertions) {
            panic!("{}", self.message);
        }
    }

    /// Panic if the condition is `true` at runtime with `debug_assertions`.
    /// 
    /// See [panic].
    #[track_caller]
    pub fn debug_panic_if(&self, condition: bool) {
        #[cfg(debug_assertions)]
        if condition {
            panic!("{}", self.message);
        }
    }

    /// Asserts that a boolean expression is `true` at runtime with `debug_assertions`.
    /// 
    /// see [assert].
    #[track_caller]
    pub fn debug_assert(&self, condition: bool) {
        debug_assert!(condition, "{}", self.message);
    }

    /// Assert that two expressions are equal to each other (using [PartialEq])
    /// with `debug_assertions`.
    /// 
    /// See [assert_eq].
    #[track_caller]
    pub fn debug_assert_eq<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug
    {
        debug_assert_eq!(lhs, rhs, "{}", self.message);
    }

    /// Assert that two expressions are not equal to each other (using [PartialEq])
    /// with `debug_assertions`.
    /// 
    /// See [assert_ne].
    #[track_caller]
    pub fn debug_assert_ne<L, R>(&self, lhs: L, rhs: R)
    where
        L: PartialEq<R>,
        L: std::fmt::Debug,
        R: std::fmt::Debug,
    {
        debug_assert_ne!(lhs, rhs, "{}", self.message);
    }

    /// Returns the contained [Some] value, consuming the `self` value
    #[track_caller]
    pub fn expect_opt<T>(&self, value: Option<T>) -> T {
        let Some(value) = value else {
            panic!("(Expect failed): {}", self.message);
        };
        value
    }

    /// Returns the contained [Ok] value, consuming the `self` value.
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

impl<M: std::fmt::Display + Clone> Clone for PanicMsg<M> {
    fn clone(&self) -> Self {
        Self {
            message: self.message.clone()
        }
    }

    fn clone_from(&mut self, source: &Self) {
        self.message = source.message.clone();
    }
}

impl<M: std::fmt::Display + Copy> Copy for PanicMsg<M> {}

impl<M: std::fmt::Display + PartialEq> PartialEq<M> for PanicMsg<M> {
    fn eq(&self, other: &M) -> bool {
        self.message == *other
    }

    fn ne(&self, other: &M) -> bool {
        self.message != *other
    }
}

impl<M: std::fmt::Display + PartialEq> PartialEq<PanicMsg<M>> for PanicMsg<M> {
    fn eq(&self, other: &PanicMsg<M>) -> bool {
        self.message == other.message
    }

    fn ne(&self, other: &PanicMsg<M>) -> bool {
        self.message != other.message
    }
}

impl<M: std::fmt::Display + Eq> Eq for PanicMsg<M> {}

impl<M: std::fmt::Display + PartialOrd> PartialOrd<M> for PanicMsg<M> {
    fn partial_cmp(&self, other: &M) -> Option<std::cmp::Ordering> {
        self.message.partial_cmp(other)
    }
}

impl<M: std::fmt::Display + PartialOrd> PartialOrd<PanicMsg<M>> for PanicMsg<M> {
    fn partial_cmp(&self, other: &PanicMsg<M>) -> Option<std::cmp::Ordering> {
        self.message.partial_cmp(&other.message)
    }
}

impl<M: std::fmt::Display + Ord> Ord for PanicMsg<M> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.message.cmp(&other.message)
    }
}

impl<M: std::fmt::Display + std::hash::Hash> std::hash::Hash for PanicMsg<M> {
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

/// Declare a const [PanicMsg] with an `&'static str` message.
/// 
/// # Example
/// ```rust, no_run
/// const_panic_msg!(PRIVATE_ERROR = "This is declared with private visibility.");
/// const_panic_msg!(pub PUBLIC_ERROR = "This is declared with public visiblity.");
/// // ...
/// PRIVATE_ERROR.panic();
/// // ...
/// PUBLIC_ERROR.debug_assert(left < right);
/// ```
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
