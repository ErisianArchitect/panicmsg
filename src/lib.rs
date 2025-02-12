//! This crate provides reusable error messages ([PanicMsg]) for use with 
//! panics, assertions (`assert`, `assert_eq`, `assert_ne`), and `expect`.
//! It also includes debug versions of each of these methods (except for `expect`).
//! 
//! Simply declare a [PanicMsg]:
//! ```rust, no_run
//! const EXAMPLE_PANIC: PanicMsg = PanicMsg::new("This is an example panic message.");
//! ```
//! Then use it like this:
//! ```rust, no_run
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
//! EXAMPLE_PANIC.expect(option);
//! // ...
//! EXAMPLE_PANIC.expect(result);
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
//! ```

mod private {
    pub trait Sealed {}
    impl<T> Sealed for Option<T> {}
    impl<T, E> Sealed for Result<T, E> {}
}

/// Used to convert [Result] into [Option], or keep [Option] as it is.
pub trait IntoOption: private::Sealed {
    type OptionT;
    fn into_option(self) -> Option<Self::OptionT>;
}

impl<T> IntoOption for Option<T> {
    type OptionT = T;
    fn into_option(self) -> Option<Self::OptionT> {
        self
    }
}

impl<T, E> IntoOption for Result<T, E> {
    type OptionT = T;
    fn into_option(self) -> Option<Self::OptionT> {
        self.ok()
    }
}

/// A message for runtime panics. When one of the methods is called on it, the given message
/// is used as the output for the panic.
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
/// EXAMPLE_PANIC.expect(option);
/// // ...
/// EXAMPLE_PANIC.expect(result);
/// // ...
/// EXAMPLE_PANIC.debug_panic();
/// // ...
/// EXAMPLE_PANIC.debug_panic_if(left >= right);
/// // ...
/// EXAMPLE_PANIC.debug_assert(left < right);
/// // ...
/// EXAMPLE_PANIC.debug_assert_eq(left, right);
/// // ...
/// EXAMPLE_PANIC.debug_assert_ne(left, right);
/// ```
pub struct PanicMsg<M: std::fmt::Display = &'static str> {
    message: M,
}

impl<M: std::fmt::Display> PanicMsg<M> {
    /// Create a new [PanicMsg].
    pub const fn new(message: M) -> Self {
        Self { message }
    }

    /// Panic at runtime.
    /// 
    /// See [panic].
    #[cold]
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
    #[cold]
    #[track_caller]
    pub fn debug_panic(&self) {
        #[cfg(debug_assertions)]
        {
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

    #[track_caller]
    pub fn expect<T: IntoOption>(&self, value: T) -> T::OptionT {
        let Some(value) = value.into_option() else {
            panic!("{}", self.message);
        };
        value
    }

    /// Resturn a reference to the contained message.
    pub const fn msg(&self) -> &M {
        &self.message
    }

}

// Trait implementations.

impl<M: std::fmt::Display> std::fmt::Display for PanicMsg<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl<M: std::fmt::Debug + std::fmt::Display> std::fmt::Debug for PanicMsg<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.message)
    }
}

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

/// An assert function with a message for runtime assertions.
/// 
/// # Example
/// ```rust, no_run
/// let asserter = Assertion::new("lhs is not equal to rhs.", |(lhs, rhs)| lhs == rhs);
/// asserter.assert((4, 4));
/// // Will panic here.
/// asserter.assert((4, 5));
/// 
/// asserter.debug_assert((4, 4));
/// // Will panic here in dev.
/// asserter.debug_assert((4, 5));
/// ```
pub struct Assertion<I, F: Fn(I) -> bool = fn(I) -> bool, M: std::fmt::Display = &'static str> {
    function: F,
    message: M,
    _phantom: std::marker::PhantomData<I>,
}

impl<I, F: Fn(I) -> bool, M: std::fmt::Display> Assertion<I, F, M> {
    /// Create a new [Assertion] with the provided `message` that calls the provided `function`.
    pub const fn new(message: M, function: F) -> Self {
        Self {
            message,
            function,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Assert the function with the given `input`.
    #[track_caller]
    pub fn assert(&self, input: I) {
        assert!((self.function)(input), "{}", self.message);
    }

    /// Assert the function with the given `input` in dev.
    #[track_caller]
    pub fn debug_assert(&self, input: I) {
        debug_assert!((self.function)(input), "{}", self.message);
    }
}