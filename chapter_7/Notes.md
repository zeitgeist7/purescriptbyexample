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