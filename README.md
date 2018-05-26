# unpacked-maybe

This module is intended to be a drop-in replacement for the Maybe type provided by base. To shave off pointer chasing, it uses -XUnboxedSums to represent the Maybe type as two machine words that are contiguous in memory, without loss of expressiveness that Maybe provides.

This library provides pattern synonyms Just and Nothing that allow users to pattern match on an Unpacked Maybe in the familiar way.

Functions are also provided for converting an Unpacked Maybe to the base library's Maybe, and vice versa.

This library is in alpha, and the internals are likely to change.
