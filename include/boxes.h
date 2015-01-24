/*
 * Common macros for boxes
 */

#ifndef HASKELL_BOXES_H
#define HASKELL_BOXES_H

#ifdef __GLASGOW_HASKELL__ 
#define OVERLOADED_STRINGS 1 
#endif

/*
 * We use cabal-generated MIN_VERSION_base to adapt to changes of base.
 * Nevertheless, as a convenience, we also allow compiling without cabal by
 * defining an approximate MIN_VERSION_base if needed. The alternative version
 * guesses the version of base using the version of GHC. This is usually
 * sufficiently accurate. However, it completely ignores minor version numbers,
 * and it makes the assumption that a pre-release version of GHC will ship with
 * base libraries with the same version numbers as the final release. This
 * assumption is violated in certain stages of GHC development, but in practice
 * this should very rarely matter, and will not affect any released version.
 */
#ifndef MIN_VERSION_base
#if __GLASGOW_HASKELL__ >= 709
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=8)))
#elif __GLASGOW_HASKELL__ >= 707
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=7)))
#elif __GLASGOW_HASKELL__ >= 705
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=6)))
#elif __GLASGOW_HASKELL__ >= 703
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=5)))
#elif __GLASGOW_HASKELL__ >= 701
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=4)))
#elif __GLASGOW_HASKELL__ >= 700
#define MIN_VERSION_base(major1,major2,minor) (((major1)<4)||(((major1) == 4)&&((major2)<=3)))
#else
#define MIN_VERSION_base(major1,major2,minor) (0)
#endif
#endif /* ifndef MIN_VERSION_base */
#endif /* ifndef HASKELL_BOXES_H */
