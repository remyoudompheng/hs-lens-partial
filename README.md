# lens-partial

The module Control.Lens.Traversal.Update is meant to provide a way
to apply transformation over a large data structure through Traversals
while minimizing allocations.

For this purpose we replace

    f :: a -> a

by the notion of `Update` which is conditional to a predicate.

    newtype Update a = Update { getUpdate :: (a -> a, a -> Bool) }

The semantics of an update are defined by:

    apply (Update (f, pred)) x = if pred x then f x else x

Then a new operator `overIf` is provided with the following semantics:

    overIf t update == over t (apply update)

It satisfies the following laws:

    overIf (t.t') update == overIf t (overIf t' update)
    overIf t up <> overIf t up' == overIf t (up <> up')

A specialized form of vector traversal is provided for demo purposes:

    overIf traverse up :: Update V.Vector a
    overVector up :: Update V.Vector a

are the same operation.

Weigh benchmarks
----------------

    Case                 Allocated  GCs
    update naive lens  127,185,576  122
    update traverse     37,849,088   36
    update special       1,903,392    1
    update fused        44,507,272   42

