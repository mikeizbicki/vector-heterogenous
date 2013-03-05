# HVector

This is a package for type safe heterogenous vectors, or `HVector`s.  This library was developed to allow the HLearn library to handle multivariate distributions---each data point is an HVector, and the trained multivariate distribution will also be an HVector.  This might be of more general interest, however, and so has been separated into its own library.  It is called [vector-heterogenous](http://hackage.haskell.org/package/vector-heterogenous) on hackage.

## Construction

The simplest way to construct an `HVector` is from an `HList` with the `vec` function.  For example, we can run:
    
    ghci> import Data.Vector.Heterogenous
    ghci> let hvec = vec ShowBox $ "test":::Nothing:::([4,5,6],()):::HNil

This declaration contains two parts.  To the right of the `$` we have the `HList` of our data.  Notice that any type is allowed.  The first argument of `vec` is the constructor for an "existential box" that we want to put these elements in.  We will call this constructor on each element in the `HList` in order to make them all of one type.  We then store these homogenous types into a standard vector from `Data.Vector`.

Since we used `ShowBox` in our `hvec` variable, we can print `hvec` to the screen:

    ghci> hvec
    vec ShowBox $ "test":::Nothing:::([4,5,6],()):::HNil
    
The type of our `HVector` has two components as well.  First is the type of box we use, and second is the list of which type corresponds to which value in the vector.
    
    ghci> :t hvec
    (Num b) => HVector ShowBox '[String,Maybe a,([b],())]

## Advantages over HLists

The advantage of an `HVector` over an `HList` is that we get O(1) indexing anywhere in the list.  (Technically the type checker still takes time O(n), but run time takes only O(1).)  We use the `view` function to do this:

    ghci> hvec `view` (undefined :: Sing 0)
    "test"

    ghci> hvec `view` (undefined :: Sing 1)
    Nothing

    ghci> hvec `view` (undefined :: Sing 2)
    ([4,5,6],())

Unfortunately, this is slightly awkward because we must make our accessor function polymorphic on the index.  Maybe someone with a better knowledge of a lens library's internals could come up with a prettier interface.

## Advantages over straight ExistentialQuantification

There are two advantages.  First, as we have seen, we can recover the original type for each index with our `view` function.  This would not be possible if we used a type of `V.Vector ShowBox`.

Second, `HVector` also has a `Monoid` instance.  That means we can do:

    ghci> hvec<>hvec
    vec ShowBox $ "testtest":::Nothing:::([4,5,6,4,5,6],())::HNil

With straight existential quantification, it would not be possible to merge the corresponding positions in each vector because they are not guaranteed to be the same type.

## Performance

Use of the `view` function above is not ideal for performance critical applications because it prevents fusion.  The easiest way to work around this is to directly access the underlying vector of existential boxes.  We do this with the `getvec` function:

    ghci> :t getvec
    getvec :: HVector box xs -> Vector box
    
    ghci> getvec hvec
    fromList [([4,5,6],()),Nothing,"test"]

Notice that elements will now be accessed in reverse order.  

Now the compiler can use fusion and everything runs quite zippy.  The trick to making this work well is creating a good existential box for your specific application.  In the HLearn library, for example, we would use a DatapointBox and a DistributionBox to represent our data points and multivariate distributions.

Based on my tests, a variable of type `HVector ShowBox '[Int,Int,Int,Int,Int,...]` performs the same as the standard `Data.Vector.Vector Int`.  The `HVector` has an extra layer of boxing to deal with, but using BangPatterns and `-funbox-strict-fields` the compiler can remove this from the generated code.
