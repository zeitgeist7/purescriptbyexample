#### Either type
Either is like the `Result` type in Swift (from the community). The implementatie might look like the following:
```purescript
data Either a b = Left a | Right b
```
The code on page 88 needs breaking down each line. Before that let's write down the type signature of all the functions and types involved:

```haskell
data Either a b = Left a | Right b

-- Functor
map :: (a -> b) -> f a -> f b

-- Apply
apply :: f (a -> b) -> f a -> f b

-- fullname
fullname :: String -> String -> String -> String

-- withError (note that e is on the left as stated in the book)
withError :: Maybe a -> e -> Either e a
```

It is useful to add parentheses to the signature of `fullname` and observe what happens when `map` is applied to it.
```haskell
fullname :: String -> (String -> String -> String)
```

So applying `map` on `fullname` will results in a function with the following type signature:
```haskell
map fullname :: f string -> f (String -> String -> String)
```
*Note the functor to functor morphism*

But the book goes a bit further than just `map fullname`. It applies the second argument to the map (ie an `f a`, where `f` is the `Either` functor).

```first `withError` "First name was missing"``` (which `:: Either String String` because `withError :: (Maybe String) -> String -> Either String String`) thus defines the functor for the rest of the chain.

```map fullname (first `withError` "First name was missing")``` <br/> therefore will have type <br/> `Either a (String -> String -> String)`

We cannot yet determine the `a` that will be determined as we *reduce* the `Right` part of the Either (using `apply` or its operator, `<*>`).
Observe that consecutive application of an `Either String String` is reducing the initial `Either a (String -> String -> String)` until we are left with an `Either String String` and we cannot apply any further *arguments* since the `Right` of the `Either` has been fully **reduced**.

## Update
`Either` as said before might have a type constructor of the form: `Either a b`. But the confusing part is that the `withError` function takes in a `Maybe a` and a `err` (`String`) and returns a `Either err a`. There is not only a **flip** but also there is an *unwrapping* of the `Maybe a`:

```haskell
withError :: Maybe a -> b ->  Either b a
                   ^    ^            ^ ^
```
**So how do we get the type signature of `fullnameEither`?**

Let's again break that into parts:
- Applying `map` on `fullname`
  ```haskell
  map :: (a -> b) -> f a -> f b

  fullname :: String -> String -> String -> String

  map fullname :: f String -> f (String -> String -> String)
  ```
- passing in the next argument, ``(first `withError` "First name was missing")``

  Something that I did not catch on the first (ok nth | n > 5) read is that the author is lifting over the `Either err` functor and **not** simply over the `Either` functor.

  Therefore the expression ``(first `withError` "First name was missing")`` has type `Either err a` **but** based on the signature of `withError`, `first` should have type `Maybe a` and `err :: String`.

  But `(map fullname)` has a first argument whose type is `f String` and since we are passing a `Either err a` to `(map fullname)` our `f` is the functor `Either err` and `a` is `String`. Therefore the expression
  ```haskell
  map fullname (first `withError` "First name was missing")
  ```
  has type `f (String -> String -> String)`.

  And since we said (above) that `first` has type `Maybe a`, and `a :: String`, `first` has type `Maybe String`.
- `apply` (or its operator `<*>`) has the following signature:
  ```haskell
  apply :: f (a -> b) -> f a -> f b
  ```
  The previous line ``map fullname (first `withError` "First name was missing")`` has type `f (String -> String -> String)`, which is of the form `a -> b` where,

  `a = String` and `b = String -> String`. Let's call that first line expression `exp`.

  So, `apply exp :: f a -> f b` or if we expand, `apply exp :: f String -> f (String -> String)`, which is again a function that takes a first argument of type `f String`.

  Applying the same logic for this type and the ``...`writeError`... `` we can deduce that middle will have type `Maybe String`.

- Lastly we are left with an expression of type `f (String -> String)` which is much simpler than the above and that will result in a type inference of `Maybe String` also for `last`.

# 🎉