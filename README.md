# service-collections
Elm library for custom dictionaries and sets.

# the problem
In elm, the only valid `Dict` keys and `Set` values are `comparable`--that is to say, the `Float`, `Int`, `String`, and `Tuple` types.
This list crucially leaves out records and custom types, like a user-defined `Id` or `Height`.

There are a littany of libraries to address this issue, but they all do so by violating a core tenet of Elm--functions are not data.
While it is _possible_ to make a dictionary type that works for all key types, you have to keep a sorter function alongside the data.
This leads to some nasty issues.
1. crashing your program with `==`
  * Using `==` on a type that stores a function is one of the few ways you can actually crash an elm program
  * So in using these dictionary types, you lose some of the guarantees of Elm
2. broken equality
  * Libraries that create this kind of dictionary type typically replace `==` with their own implementation, say `eq`
  * However, this `eq` function only compares the value of the _items_; it does not indicate true structural equality
    ```elm
    type alias Person = { name : String, occupation : String }

    nameSorter person1 person2 =
	compare person1.name person2.name

    occupationSorter person1 person2 =
	compare person1.occupation person2.occupation

    Dict.eq (Dict.empty nameSorter) (Dict.empty occupationSorter)
    -- True
    ```
  * While both `Dict`s share the same type and same values (they are empty), they are *not* the same
  * These `Dict`s will behave _very_ differently as we start adding entries, as they have completely separate notions of equality
  * In using this replacement equality, we lose even more of Elm's guarantees 

# other solutions
The library [miniBill/elm-generic-dict](https://package.elm-lang.org/packages/miniBill/elm-generic-dict/latest/) uses `elm-codegen` to generate custom dictionary types.
This approach seems more inline with the general Elm approach of keeping things simple and decoupled.

However, you do have to deal with the added infrastructure of code generation, which might not be something you want to spend time learning and configuring.
# our solution
This library exposes two service collections--`Service.Dict` and `Service.Set`.
A service collection is not meant to be used directly in your main application code.
Rather, a service collection provides machinery to make your custom types easy to implement.

Say you have a type `Ids`, which represents a set of unique `Id`s.
You might represent it with a service-set like this:
```elm
type Ids = Ids (Service.Set Id)
```

Maybe you want to check if an `Id` is present:
```elm
member : Id -> Ids -> Bool
member id (Ids ids) =
  Service.Set.member sorter id ids
```

Hold on now! What is `sorter`?
Well, if you are creating a set of unique `Id`s, that means they are comparable in some way.
When you define a sorter, you are explicitly outlining how your `Id`s should be compared.
```elm
sorter : Sorter Id
sorter =
  Sort.by Id.code Sort.int
```
So we grab the `Id` code value, then sort that integer value. Sounds good!

As you need new functionality, you can just grab whatever function you need from `Service.Dict`.
You can safely use your new custom type with `==`!
