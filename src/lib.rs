/// A type that may represent a list
pub trait MaybeList {
    /// Type that represents the first element of a list
    type Head;

    /// Type that represents the tail of a list
    type Tail: MaybeList + ?Sized;

    /// Return true if this is a list
    fn is_list(&self) -> bool;

    /// Return true if this is an empty list
    fn is_empty(&self) -> bool;

    /// Return the first item if this is a non-empty list
    fn head(&self) -> Option<&Self::Head>;

    /// Return the tail (all *but* the first item) if this is a non-empty list
    fn tail(&self) -> Option<&Self::Tail>;

    /// Return a slice view of this non-empty list. May not be possible for all types
    fn as_slice(&self) -> Option<&[Self::Head]>;

    /// Deconstruct the list: return head and tail as a tuple
    fn decons(&self) -> Option<(&Self::Head, &Self::Tail)> {
        self.head().and_then(|h| self.tail().map(|t| (h, t)))
    }
}

impl<T: MaybeList> MaybeList for &T {
    type Head = T::Head;
    type Tail = T::Tail;

    fn is_list(&self) -> bool {
        (*self).is_list()
    }

    fn is_empty(&self) -> bool {
        (*self).is_empty()
    }

    fn head(&self) -> Option<&Self::Head> {
        (*self).head()
    }

    fn tail(&self) -> Option<&Self::Tail> {
        (*self).tail()
    }

    fn as_slice(&self) -> Option<&[Self::Head]> {
        (*self).as_slice()
    }
}

impl<T> MaybeList for [T] {
    type Head = T;
    type Tail = [T];

    fn is_list(&self) -> bool {
        true
    }

    fn is_empty(&self) -> bool {
        (*self).is_empty()
    }

    fn head(&self) -> Option<&Self::Head> {
        self.first()
    }

    fn tail(&self) -> Option<&Self::Tail> {
        match self {
            [] => None,
            [_, tail @ ..] => Some(tail),
        }
    }

    fn as_slice(&self) -> Option<&[Self::Head]> {
        Some(self)
    }
}

impl<T> MaybeList for Vec<T> {
    type Head = T;
    type Tail = [T];

    fn is_list(&self) -> bool {
        true
    }

    fn is_empty(&self) -> bool {
        (*self).is_empty()
    }

    fn head(&self) -> Option<&Self::Head> {
        self[..].head()
    }

    fn tail(&self) -> Option<&Self::Tail> {
        self[..].tail()
    }

    fn as_slice(&self) -> Option<&[Self::Head]> {
        Some(&self[..])
    }
}

#[macro_export]
macro_rules! match_sexpr {
    ($expr:expr, else => $then:expr,) => {$then};

    ($expr:expr, case _ => $then:expr, $($rest:tt)*) => {$then};

    ($expr:expr, case $var:ident => $then:expr, $($rest:tt)*) => {{
        let $var = $expr;
        $then
    }};

    ($expr:expr, case () => $then:expr, $($rest:tt)*) => {
        if $crate::MaybeList::is_empty($expr) {
            $then
        } else {
            match_sexpr! { $expr, $($rest)* }
        }
    };

    ($expr:expr, case ($item:tt) => $then:expr, $($rest:tt)*) => {
        match_sexpr! { $expr, case ($item :: ()) => $then, $($rest)* }
    };

    ($expr:expr, case ($item:tt :: $tail:tt) => $then:expr, $($rest:tt)*) => {{
        let result = if let Some((_h, _t)) = $crate::MaybeList::decons($expr) {
            match_sexpr!(
                _h,
                case $item => match_sexpr!(
                    _t,
                    case $tail => #[allow(unreachable_code)]Some($then),
                    else => None,
                ),
                else => None,
            )
        } else {
            None
        };
        match result {
            Some(r) => r,
            None => match_sexpr! { $expr, $($rest)* },
        }
    }};

    ($expr:expr, case ($item:tt, $($more:tt)*) => $then:expr, $($rest:tt)*) => {{
        let result = if let Some((_h, _t)) = $crate::MaybeList::decons($expr) {
            match_sexpr!(
                _h,
                case $item => match_sexpr!(
                    _t,
                    case ($($more)*) => #[allow(unreachable_code)]Some($then),
                    else => None,
                ),
                else => None,
            )
        } else {
            None
        };
        match result {
            Some(r) => r,
            None => match_sexpr! { $expr, $($rest)* },
        }
    }};

    ($expr:expr, case [$pat:pat] => $then:expr, $($rest:tt)*) => {
        if let $pat = $expr {
            $then
        }else {
            match_sexpr! { $expr, $($rest)* }
        }
    };

    ($expr:expr, case $literal:expr => $then:expr, $($rest:tt)*) => {
        if $expr == &$literal {
            $then
        } else {
            match_sexpr! { $expr, $($rest)* }
        }
    };
}
#[cfg(test)]
mod tests {
    use super::MaybeList;

    #[derive(PartialEq)]
    enum Sexpr {
        Nil,
        Atom(&'static str),
        Pair(Box<(Sexpr, Sexpr)>),
    }

    impl Sexpr {
        fn list(items: Vec<Self>) -> Self {
            let mut this = Sexpr::Nil;
            for x in items.into_iter().rev() {
                this = Sexpr::Pair(Box::new((x, this)));
            }
            this
        }
    }

    impl PartialEq<&str> for Sexpr {
        fn eq(&self, other: &&str) -> bool {
            match self {
                Sexpr::Atom(s) => s == other,
                _ => false,
            }
        }
    }

    impl MaybeList for Sexpr {
        type Head = Sexpr;
        type Tail = Sexpr;

        fn is_list(&self) -> bool {
            match self {
                Sexpr::Nil => true,
                Sexpr::Pair(_) => true,
                _ => false,
            }
        }

        fn is_empty(&self) -> bool {
            match self {
                Sexpr::Nil => true,
                _ => false,
            }
        }

        fn head(&self) -> Option<&Self::Head> {
            match self {
                Sexpr::Pair(p) => Some(&p.0),
                _ => None,
            }
        }

        fn tail(&self) -> Option<&Self::Tail> {
            match self {
                Sexpr::Pair(p) => Some(&p.1),
                _ => None,
            }
        }

        fn as_slice(&self) -> Option<&[Self::Head]> {
            unimplemented!()
        }
    }

    #[test]
    fn match_anything() {
        assert!(match_sexpr! {
            unreachable!(),  // the expression is never evaluated here
            case _ => true,
        })
    }

    #[test]
    fn match_pattern() {
        assert!(match_sexpr! {
            Sexpr::Atom("foo"),
            case [Sexpr::Atom(_)] => true,
            else => false,
        })
    }

    #[test]
    fn match_literal() {
        assert!(match_sexpr! {
            &Sexpr::Atom("foo"),
            case Sexpr::Atom("foo") => true,
            else => false,
        });

        assert!(match_sexpr! {
            &Sexpr::Atom("bar"),
            case Sexpr::Atom("foo") => false,
            else => true,
        });
    }

    #[test]
    fn match_anything_that_implements_partialeq() {
        assert!(match_sexpr! {
            &Sexpr::Atom("foo"),
            case "foo" => true,
            else => false,
        });

        assert!(match_sexpr! {
            &Sexpr::Atom("foo"),
            case "bar" => false,
            else => true,
        });
    }

    #[test]
    fn match_binds_identifier() {
        assert!(match_sexpr! {
            &Sexpr::Atom("foo"),
            case x => x == &"foo",
        })
    }

    #[test]
    fn match_empty_list() {
        assert!(match_sexpr! {
            &Vec::<()>::new(),
            case () => true,
            else => false,
        });
        assert!(match_sexpr! {
            &Sexpr::list(vec![]),
            case () => true,
            else => false,
        });

        assert!(match_sexpr! {
            &Sexpr::Atom("foo"),
            case () => false,
            else => true,
        });

        assert!(match_sexpr! {
            &Sexpr::list(vec![Sexpr::Atom("foo")]),
            case () => false,
            else => true,
        });
    }

    #[test]
    fn match_exact_list() {
        assert!(match_sexpr! {
            &vec![1],
            case (1) => true,
            else => false,
        });

        assert!(match_sexpr! {
            &Sexpr::list(vec![Sexpr::Atom("foo"), Sexpr::Atom("bar")]),
            case ("foo", "bar") => true,
            else => false,
        });
    }

    #[test]
    fn match_bind_list_items() {
        let expr = vec![1, 2, 3];

        assert!(match_sexpr! {
            &expr,
            case (_) => panic!("unexpected match"),
            else => true,
        });

        assert_eq!(
            match_sexpr! {
                &expr,
                case (_, 2, y) => y,
                else => panic!(""),
            },
            &3
        );
    }

    #[test]
    fn match_tail() {
        let expr = vec![1, 2, 3];
        assert!(match_sexpr! {
            &expr,
            case (1, 2 :: x) => x == &[3],
            else => false,
        });

        assert!(match_sexpr! {
            &expr,
            case (1 :: [[x@.., _]]) => x == &[2],
            else => false,
        })
    }
}
