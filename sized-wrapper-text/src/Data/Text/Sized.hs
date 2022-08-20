{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module        : Data.Text.Sized
-- Copyright     : Gautier DI FOLCO
-- License       : BSD2
--
-- Maintainer    : Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability     : Unstable
-- Portability   : GHC
--
-- 'Sized' wrappers around `Data.Text`
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Text.Sized as NET
module Data.Text.Sized
  ( -- * Types
    SizedText,
    SizedStrictText,

    -- * Creation and elimination
    pack,
    unpack,
    singleton,

    -- * Basic interface
    cons,
    snoc,
    uncons,
    unsnoc,
    head,
    last,
    tail,
    init,
    length,
    compareLength,

    -- * Transformations
    map,
    intercalate,
    intersperse,
    transpose,
    reverse,
    replace,

    -- ** Case conversion
    -- $case
    toCaseFold,
    toLower,
    toUpper,
    toTitle,

    -- ** Justification
    justifyLeft,
    justifyRight,
    center,

    -- * Folds
    foldl,
    foldl',
    foldl1,
    foldl1',
    foldr,
    foldr1,

    -- ** Special folds
    concat,
    concatMap,
    any,
    all,
    maximum,
    minimum,

    -- * Construction

    -- ** Scans
    scanl,
    scanl1,
    scanr,
    scanr1,

    -- ** Accumulating maps
    mapAccumL,
    mapAccumR,

    -- ** Generation and unfolding
    replicate,

    -- * Substrings

    -- ** Breaking strings
    take,
    takeEnd,
    drop,
    dropEnd,
    takeWhile,
    takeWhileEnd,
    dropWhile,
    dropWhileEnd,
    dropAround,
    strip,
    stripStart,
    stripEnd,
    splitAt,
    breakOn,
    breakOnEnd,
    break,
    span,
    group,
    groupBy,
    inits,
    tails,

    -- ** Breaking into many substrings
    -- $split
    splitOn,
    split,
    chunksOf,

    -- ** Breaking into lines and words
    lines,
    words,
    unlines,
    unwords,

    -- * Predicates
    isPrefixOf,
    isSuffixOf,
    isInfixOf,

    -- ** View patterns
    stripPrefix,
    stripSuffix,
    commonPrefixes,

    -- * Searching
    filter,
    breakOnAll,
    find,
#if MIN_VERSION_text(1,2,5)
    elem,
#endif
    partition,

    -- * Indexing
    index,
    findIndex,
    count,

    -- * Zipping
    zip,
    zipWith,
  )
where

import Data.Bifunctor(bimap, first)
import Data.Proxy
import Data.Sized
import qualified Data.Text as T
import GHC.Stack
import GHC.TypeNats
import Prelude (Bool (..), Char, Int, Maybe (..),  Ordering, String, fmap, fst, snd, uncurry, ($), (.), fromInteger)
import GHC.Natural (naturalToInteger)

type SizedStrictText s = Sized s T.Text

type SizedText s = SizedStrictText s

instance SizedSingleton T.Text where
  type SizedSingletonElement T.Text = Char
  sizedSingleton _ = T.singleton

instance SizedFromContainer T.Text where
  calculateSize = T.length

-- -----------------------------------------------------------------------------

-- * Conversion to/from 'SizedStrictText'

-- | /O(n)/ Convert a 'Sized s String' into a 'SizedStrictText s'.
-- Performs replacement on invalid scalar values.
pack :: Sized s String -> SizedStrictText s
pack = trustedChangeOverSized T.pack
{-# INLINE pack #-}

-- | /O(n)/ Convert a 'SizedStrictText s' into a 'Sized s String'.
unpack :: HasCallStack => SizedStrictText s -> Sized s String
unpack = trustedChangeOverSized T.unpack
{-# INLINE unpack #-}

-- -----------------------------------------------------------------------------

-- * Basic functions

-- | /O(n)/ Adds a character to the front of a 'SizedStrictText'.  This function
-- is more costly than its 'List' counterpart because it requires
-- copying a new array.  Performs replacement on
-- invalid scalar values.
cons :: Char -> SizedStrictText s -> SizedStrictText (Exactly 1 <+> s)
cons c = trustedChangeOverSized $ T.cons c
{-# INLINE cons #-}

infixr 5 `cons`

-- | /O(n)/ Adds a character to the end of a 'SizedStrictText'.  This copies the
-- entire array in the process.
-- Performs replacement on invalid scalar values.
snoc :: SizedStrictText s -> Char -> SizedStrictText (s <+> Exactly 1)
snoc t c = trustedChangeOverSized (`T.snoc` c) t
{-# INLINE snoc #-}

-- | /O(1)/ Returns the first character of a 'SizedStrictText'.
head :: IsNotEmpty s => SizedStrictText s -> Char
head = fst . uncons
{-# INLINE head #-}

-- | /O(1)/ Returns the first character and rest of a 'SizedStrictText'.
uncons :: (IsNotEmpty s, (Exactly 1 <+> s) ~ s') => SizedStrictText s -> (Char, SizedStrictText s')
uncons = fmap trustedSized . withSized T.uncons
{-# INLINE uncons #-}

-- | /O(1)/ Returns the last character of a 'SizedStrictText'.
last :: (IsNotEmpty s, (Exactly 1 <+> s) ~ s') => SizedStrictText s -> Char
last = snd . unsnoc
{-# INLINE last #-}

-- | /O(1)/ Returns all characters after the head of a 'SizedStrictText'.
tail :: (IsNotEmpty s, (Exactly 1 <+> s) ~ s') => SizedStrictText s -> SizedStrictText s'
tail = snd . uncons
{-# INLINE tail #-}

-- | /O(1)/ Returns all but the last character of a 'SizedStrictText'.
init :: (IsNotEmpty s, (s <+> Exactly 1) ~ s') => SizedStrictText s -> SizedStrictText s'
init = fst . unsnoc
{-# INLINE init #-}

-- | /O(1)/ Returns all but the last character and the last character of a
-- 'SizedStrictText'.
unsnoc :: (IsNotEmpty s, (s <+> Exactly 1) ~ s') => SizedStrictText s -> (SizedStrictText s', Char)
unsnoc = first trustedSized . withSized T.unsnoc
{-# INLINE unsnoc #-}

-- | /O(n)/ Returns the number of characters in a 'SizedStrictText'.
length :: SizedStrictText s -> Int
length = T.length . getSized
{-# INLINE length #-}

-- length needs to be phased after the compareN/length rules otherwise
-- it may inline before the rules have an opportunity to fire.

-- | /O(min(n,c))/ Compare the count of characters in a 'SizedStrictText' to a number.
--
-- @
-- 'compareLength' t c = 'P.compare' ('length' t) c
-- @
--
-- This function gives the same answer as comparing against the result
-- of 'length', but can short circuit if the count of characters is
-- greater than the number, and hence be more efficient.
compareLength :: (KnownNat n, Includes s n) => SizedStrictText s -> Proxy n -> Ordering
compareLength x = T.compareLength (getSized x) . getN
{-# INLINE compareLength #-}

-- -----------------------------------------------------------------------------

-- * Transformations

-- | /O(n)/ 'map' @f@ @t@ is the 'SizedStrictText' obtained by applying @f@ to
-- each element of @t@.
--
-- Example:
--
-- >>> let message = pack "I am not angry. Not at all."
-- >>> T.map (\c -> if c == '.' then '!' else c) message
-- "I am not angry! Not at all!"
--
-- Performs replacement on invalid scalar values.
map :: (Char -> Char) -> SizedStrictText s -> SizedStrictText s
map f = trustedChangeOverSized $ T.map f
{-# INLINE map #-}

-- | /O(n)/ The 'intercalate' function takes a 'SizedStrictText' and a list of
-- 'SizedStrictText's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Example:
--
-- >>> T.intercalate "NI!" ["We", "seek", "the", "Holy", "Grail"]
-- "WeNI!seekNI!theNI!HolyNI!Grail"
intercalate :: T.Text -> Sized s [SizedStrictText s'] -> SizedStrictText (s <*> s')
intercalate e = trustedSized . T.intercalate e . fmap getSized . getSized
{-# INLINE intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'SizedStrictText'.
--
-- Example:
--
-- >>> T.intersperse '.' "SHIELD"
-- "S.H.I.E.L.D"
--
-- Performs replacement on invalid scalar values.
intersperse :: Char -> SizedStrictText s -> SizedStrictText (s <*> Exactly 2 <-> Exactly 1)
intersperse c = trustedChangeOverSized $ T.intersperse c
{-# INLINE intersperse #-}

-- | /O(n)/ Reverse the characters of a string.
--
-- Example:
--
-- >>> T.reverse "desrever"
-- "reversed"
reverse :: HasCallStack => SizedStrictText s -> SizedStrictText s
reverse = trustedChangeOverSized T.reverse
{-# INLINE reverse #-}

-- | /O(m+n)/ Replace every non-trustedChangeOverlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
--
-- This function behaves as though it was defined as follows:
--
-- @
-- replace needle replacement haystack =
--   'intercalate' replacement ('splitOn' needle haystack)
-- @
--
-- As this suggests, each occurrence is replaced exactly once.  So if
-- @needle@ occurs in @replacement@, that occurrence will /not/ itself
-- be replaced recursively:
--
-- >>> replace "oo" "foo" "oo"
-- "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- >>> replace "ofo" "bar" "ofofo"
-- "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
replace ::
  -- | @needle@ to search for.  If this string is empty, an
  -- error will occur.
  SizedStrictText s0 ->
  -- | @replacement@ to replace @needle@ with.
  SizedStrictText s1 ->
  -- | @haystack@ in which to search.
  SizedStrictText s2 ->
  SizedStrictText Unknown
replace = trustedChangeOverSized3 T.replace

-- ----------------------------------------------------------------------------

-- ** Case conversions (folds)

-- $case
--
-- When case converting 'SizedStrictText' values, do not use combinators like
-- @map toUpper@ to case convert each character of a string
-- individually, as this gives incorrect results according to the
-- rules of some writing systems.  The whole-string case conversion
-- functions from this module, such as @toUpper@, obey the correct
-- case conversion rules.  As a result, these functions may map one
-- input character to two or three output characters. For examples,
-- see the documentation of each function.
--
-- /Note/: In some languages, case conversion is a locale- and
-- context-dependent operation. The case conversion functions in this
-- module are /not/ locale sensitive. Programs that require locale
-- sensitivity should use appropriate versions of the
-- <http://hackage.haskell.org/package/text-icu-0.6.3.7/docs/Data-Text-ICU.html#g:4 case mapping functions from the text-icu package >.

-- | /O(n)/ Convert a string to folded case.
--
-- This function is mainly useful for performing caseless (also known
-- as case insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature \"&#xfb13;\" (men now, U+FB13) is case
-- folded to the sequence \"&#x574;\" (men, U+0574) followed by
-- \"&#x576;\" (now, U+0576), while the Greek \"&#xb5;\" (micro sign,
-- U+00B5) is case folded to \"&#x3bc;\" (small letter mu, U+03BC)
-- instead of itself.
toCaseFold :: SizedStrictText s -> SizedStrictText s
toCaseFold = trustedChangeOverSized T.toCaseFold
{-# INLINE toCaseFold #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, \"&#x130;\" (Latin capital letter I with dot above,
-- U+0130) maps to the sequence \"i\" (Latin small letter i, U+0069)
-- followed by \" &#x307;\" (combining dot above, U+0307).
toLower :: SizedStrictText s -> SizedStrictText s
toLower = trustedChangeOverSized T.toLower
{-# INLINE toLower #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the German \"&#xdf;\" (eszett, U+00DF) maps to the
-- two-letter sequence \"SS\".
toUpper :: SizedStrictText s -> SizedStrictText s
toUpper = trustedChangeOverSized T.toUpper
{-# INLINE toUpper #-}

-- | /O(n)/ Convert a string to title case, using simple case
-- conversion.
--
-- The first letter of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
toTitle :: SizedStrictText s -> SizedStrictText s
toTitle = trustedChangeOverSized T.toTitle
{-# INLINE toTitle #-}

-- | /O(n)/ Left-justify a string to the given length, using the
-- specified fill character on the right.
-- Performs replacement on invalid scalar values.
--
-- Examples:
--
-- >>> justifyLeft 7 'x' "foo"
-- "fooxxxx"
--
-- >>> justifyLeft 3 'x' "foobar"
-- "foobar"
justifyLeft :: KnownNat n => Proxy n -> Char -> SizedStrictText s -> SizedStrictText (AtLeast n) 
justifyLeft p c = trustedChangeOverSized $ T.justifyLeft (getN p) c
{-# INLINE justifyLeft #-}

-- | /O(n)/ Right-justify a string to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- >>> justifyRight 7 'x' "bar"
-- "xxxxbar"
--
-- >>> justifyRight 3 'x' "foobar"
-- "foobar"
justifyRight :: KnownNat n => Proxy n -> Char -> SizedStrictText s -> SizedStrictText (AtLeast n) 
justifyRight p c = trustedChangeOverSized $ T.justifyRight (getN p) c
{-# INLINE justifyRight #-}

-- | /O(n)/ Center a string to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- >>> center 8 'x' "HS"
-- "xxxHSxxx"
center :: KnownNat n => Proxy n -> Char -> SizedStrictText s -> SizedStrictText (AtLeast n) 
center p c = trustedChangeOverSized $ T.center (getN p) c
{-# INLINE center #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'SizedStrictText' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
--
-- Examples:
--
-- >>> transpose ["green","orange"]
-- ["go","rr","ea","en","ng","e"]
--
-- >>> transpose ["blue","red"]
-- ["br","le","ud","e"]
transpose :: Sized s [SizedStrictText s'] -> Sized s' [SizedStrictText s]
transpose = trustedChangeOverSized $ fmap trustedSized . T.transpose . fmap getSized

-- -----------------------------------------------------------------------------

-- * Reducing 'SizedStrictText's (folds)

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'SizedStrictText',
-- reduces the 'SizedStrictText' using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> SizedStrictText s -> a
foldl f a = T.foldl f a . getSized
{-# INLINE foldl #-}

-- | /O(n)/ A strict version of 'foldl'.
foldl' :: (a -> Char -> a) -> a -> SizedStrictText s -> a
foldl' f a = T.foldl' f a . getSized
{-# INLINE foldl' #-}

-- | /O(n)/ A variant of 'foldl' that has no starting value argument.
foldl1 :: IsNotEmpty s => (Char -> Char -> Char) -> SizedStrictText s -> Char
foldl1 f = uncurry (T.foldl f) . fmap getSized . uncons
{-# INLINE foldl1 #-}

-- | /O(n)/ A strict version of 'foldl1'.
foldl1' :: IsNotEmpty s => (Char -> Char -> Char) -> SizedStrictText s -> Char
foldl1' f = uncurry (T.foldl' f) . fmap getSized . uncons
{-# INLINE foldl1' #-}

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'SizedStrictText',
-- reduces the 'SizedStrictText' using the binary operator, from right to left.
--
-- If the binary operator is strict in its second argument, use 'foldr''
-- instead.
--
-- 'foldr' is lazy like 'Data.List.foldr' for lists: evaluation actually
-- traverses the 'SizedStrictText' from left to right, only as far as it needs to.
-- @
--
-- Searches from left to right with short-circuiting behavior can
-- also be defined using 'foldr' (/e.g./, 'any', 'all', 'find', 'elem').
foldr :: (Char -> a -> a) -> a -> SizedStrictText s -> a
foldr f a = T.foldr f a . getSized
{-# INLINE foldr #-}

-- | /O(n)/ A variant of 'foldr' that has no starting value argument.
foldr1 :: IsNotEmpty s => (Char -> Char -> Char) -> SizedStrictText s -> Char
foldr1 f = uncurry (T.foldr f) . fmap getSized . uncons
{-# INLINE foldr1 #-}

-- -----------------------------------------------------------------------------

-- ** Special folds

-- | /O(n)/ Concatenate a list of 'SizedStrictText's.
concat :: Sized s [SizedStrictText s'] -> SizedStrictText (s <*> s')
concat = trustedChangeOverSized $ T.concat . fmap getSized

-- | /O(n)/ Map a function over a 'SizedStrictText' that results in a 'SizedStrictText', and
-- concatenate the results.
concatMap :: (Char -> SizedStrictText s') -> SizedStrictText s -> SizedStrictText (s <*> s')
concatMap f = trustedChangeOverSized $ T.concatMap $ getSized . f
{-# INLINE concatMap #-}

-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'SizedStrictText' @t@ satisfies the predicate @p@.
any :: (Char -> Bool) -> SizedStrictText s -> Bool
any p = T.any p . getSized
{-# INLINE any #-}

-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'SizedStrictText' @t@ satisfy the predicate @p@.
all :: (Char -> Bool) -> SizedStrictText s -> Bool
all p = T.all p . getSized
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'SizedStrictText'.
maximum :: IsNotEmpty s => SizedStrictText s -> Char
maximum = T.maximum . getSized
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'SizedStrictText'.
minimum :: IsNotEmpty s => SizedStrictText s -> Char
minimum = T.minimum . getSized
{-# INLINE minimum #-}

-- -----------------------------------------------------------------------------

-- * Building 'SizedStrictText's

-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- __Properties__
--
-- @'head' ('scanl' f z xs) = z@
--
-- @'last' ('scanl' f z xs) = 'foldl' f z xs@
scanl :: (Char -> Char -> Char) -> Char -> SizedStrictText s -> SizedStrictText s
scanl f s = trustedChangeOverSized $ T.scanl f s
{-# INLINE scanl #-}

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 ::  IsNotEmpty s =>(Char -> Char -> Char) -> SizedStrictText s -> SizedStrictText s
scanl1 f = trustedChangeOverSized $ T.scanl1 f
{-# INLINE scanl1 #-}

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
scanr :: (Char -> Char -> Char) -> Char -> SizedStrictText s -> SizedStrictText s
scanr f s = trustedChangeOverSized $ T.scanr f s
{-# INLINE scanr #-}

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument. Performs replacement on invalid scalar values.
scanr1 :: IsNotEmpty s => (Char -> Char -> Char) -> SizedStrictText s -> SizedStrictText s
scanr1 f = trustedChangeOverSized $ T.scanr1 f
{-# INLINE scanr1 #-}

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'SizedStrictText', passing an accumulating
-- parameter from left to right, and returns a final 'SizedStrictText'.  Performs
-- replacement on invalid scalar values.
mapAccumL :: forall a s. (a -> Char -> (a, Char)) -> a -> SizedStrictText s -> (a, SizedStrictText s)
mapAccumL f s = fmap trustedSized . T.mapAccumL f s . getSized
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'SizedStrictText', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'SizedStrictText'.
-- Performs replacement on invalid scalar values.
mapAccumR :: forall a s. (a -> Char -> (a, Char)) -> a -> SizedStrictText s -> (a, SizedStrictText s)
mapAccumR f s = fmap trustedSized . T.mapAccumR f s . getSized
{-# INLINE mapAccumR #-}

-- -----------------------------------------------------------------------------

-- ** Generating and unfolding 'SizedStrictText's

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'SizedStrictText' consisting of the input
-- @t@ repeated @n@ times, @n@ should be strictly positive.
replicate :: KnownNat n => Proxy n -> SizedStrictText s -> SizedStrictText (s <*> Exactly n)
replicate n = trustedChangeOverSized $ T.replicate $ getN n
{-# INLINE replicate #-}

-- -----------------------------------------------------------------------------

-- * Substrings

-- | /O(n)/ 'take' @n@, applied to a 'SizedStrictText', returns the prefix of the
-- 'Text' of length @n@, or the 'Text' itself if @n@ is greater than
-- the length of the SizedStrictText.
take :: (KnownNat n, Includes s n)=> Proxy n -> SizedStrictText s -> SizedStrictText (AtMost n) 
take n = trustedChangeOverSized $ T.take (getN n)
{-# INLINE take #-}

-- | /O(n)/ 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> takeEnd 3 "foobar"
-- "bar"
takeEnd :: (KnownNat n, Includes s n)=> Proxy n -> SizedStrictText s -> SizedStrictText (AtMost n) 
takeEnd n = trustedChangeOverSized $ T.takeEnd (getN n)
{-# INLINE takeEnd #-}

-- | /O(n)/ 'drop' @n@, applied to a 'SizedStrictText', returns the suffix of the
-- 'Text' after the first @n@ characters, or the empty 'Text' if @n@
-- is greater than the length of the 'SizedStrictText'.
drop :: (KnownNat n, Includes s n) => Proxy n -> SizedStrictText s -> SizedStrictText (s <-> Exactly n) 
drop n = trustedChangeOverSized $ T.drop (getN n)
{-# INLINE drop #-}

-- | /O(n)/ 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> dropEnd 3 "foobar"
-- "foo"
dropEnd :: (KnownNat n, Includes s n) => Proxy n -> SizedStrictText s -> SizedStrictText (s <-> Exactly n) 
dropEnd n = trustedChangeOverSized $ T.dropEnd (getN n)
{-# INLINE dropEnd #-}

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'SizedStrictText',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> SizedStrictText s -> SizedStrictText (RestrictAtMost s)
takeWhile p = trustedChangeOverSized $ T.takeWhile p
{-# INLINE takeWhile #-}

-- | /O(n)/ 'takeWhileEnd', applied to a predicate @p@ and a 'SizedStrictText',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- >>> takeWhileEnd (=='o') "foo"
-- "oo"
takeWhileEnd :: (Char -> Bool) -> SizedStrictText s -> SizedStrictText (RestrictAtMost s)
takeWhileEnd p = trustedChangeOverSized $ T.takeWhileEnd p
{-# INLINE takeWhileEnd #-}

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.
dropWhile :: (Char -> Bool) -> SizedStrictText s -> SizedStrictText (RestrictAtMost s)
dropWhile p = trustedChangeOverSized $ T.dropWhile p
{-# INLINE dropWhile #-}

-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- >>> dropWhileEnd (=='.') "foo..."
-- "foo"
dropWhileEnd :: (Char -> Bool) -> SizedStrictText s -> SizedStrictText (RestrictAtMost s)
dropWhileEnd p = trustedChangeOverSized $ T.dropWhileEnd p
{-# INLINE dropWhileEnd #-}

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.
dropAround :: (Char -> Bool) -> SizedStrictText s -> SizedStrictText (RestrictAtMost s)
dropAround p = trustedChangeOverSized $ T.dropAround p
{-# INLINE dropAround #-}

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
stripStart :: SizedStrictText s -> SizedStrictText (RestrictAtMost s)
stripStart = trustedChangeOverSized $ T.stripStart
{-# INLINE stripStart #-}

-- | /O(n)/ Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
stripEnd :: SizedStrictText s -> SizedStrictText (RestrictAtMost s)
stripEnd = trustedChangeOverSized $ T.stripEnd
{-# INLINE stripEnd #-}

-- | /O(n)/ Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
strip :: SizedStrictText s -> SizedStrictText (RestrictAtMost s)
strip = trustedChangeOverSized $ T.strip
{-# INLINE strip #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: (KnownNat n, Includes s n) => Proxy n -> SizedStrictText s -> (SizedStrictText (RestrictAtMost s), SizedStrictText (RestrictAtMost s)) 
splitAt n = bimap trustedSized trustedSized . T.splitAt (getN n) . getSized
{-# INLINE splitAt #-}

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the text.
--
-- >>> T.span (=='0') "000AB"
-- ("000","AB")
span :: (Char -> Bool) -> SizedStrictText s -> (SizedStrictText (RestrictAtMost s), SizedStrictText (RestrictAtMost s)) 
span p = bimap trustedSized trustedSized . T.span p . getSized
{-# INLINE span #-}

-- | /O(n)/ 'break' is like 'span', but the prefix returned is
-- over elements that fail the predicate @p@.
--
-- >>> T.break (=='c') "180cm"
-- ("180","cm")
break :: (Char -> Bool) -> SizedStrictText s -> (SizedStrictText (RestrictAtMost s), SizedStrictText (RestrictAtMost s)) 
break p = bimap trustedSized trustedSized . T.break p . getSized
{-# INLINE break #-}

-- | /O(n)/ Group characters in a string according to a predicate.
groupBy :: (Char -> Char -> Bool) -> SizedStrictText s -> Sized (RestrictAtMost s) [SizedStrictText (RestrictAtMost s)]
groupBy p = trustedChangeOverSized $ fmap trustedSized  . T.groupBy p
{-# INLINE groupBy #-}

-- | /O(n)/ Group characters in a string by equality.
group :: SizedStrictText s -> Sized (RestrictAtMost s) [SizedStrictText (RestrictAtMost s)]
group = trustedChangeOverSized $ fmap trustedSized  . T.group
{-# INLINE group #-}

-- | /O(n)/ Return all initial segments of the given 'SizedStrictText', shortest
-- first.
inits :: SizedStrictText s -> Sized (s <+> Exactly 1) [SizedStrictText (RestrictAtMost s)]
inits = trustedChangeOverSized $ fmap trustedSized  . T.inits
{-# INLINE inits #-}

-- | /O(n)/ Return all final segments of the given 'SizedStrictText', longest
-- first.
tails :: SizedStrictText s -> Sized (s <+> Exactly 1) [SizedStrictText (RestrictAtMost s)]
tails = trustedChangeOverSized $ fmap trustedSized   . T.tails
{-# INLINE tails #-}

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Text's that
-- are slices of the original.

-- | /O(m+n)/ Break a 'SizedStrictText' into pieces separated by the first 'Text'
-- argument (which cannot be empty), consuming the delimiter. An empty
-- delimiter is invalid, and will cause an error to be raised.
--
-- Examples:
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >>> splitOn "x"    "x"
-- ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
--
-- (Note: the string @s@ to split on above cannot be empty.)
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
splitOn ::
  -- | String to split on. If this string is empty, an error
  -- will occur.
  SizedStrictText s ->
  -- | Input text.
  SizedStrictText s' ->
  Sized (RestrictAtMost s') [SizedStrictText (RestrictAtMost s')]
splitOn = trustedChangeOverSized2 $ \x y -> fmap trustedSized $ T.splitOn x y
{-# INLINE splitOn #-}

-- | /O(n)/ Splits a 'SizedStrictText' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- >>> split (=='a') "aabbaca"
-- ["","","bb","c",""]
--
-- >>> split (=='a') ""
-- [""]
split :: (Char -> Bool) -> SizedStrictText s -> Sized (RestrictAtMost s) [SizedStrictText (RestrictAtMost s)]
split p = trustedChangeOverSized  $ fmap trustedSized   . T.split p
{-# INLINE split #-}

-- | /O(n)/ Splits a 'SizedStrictText' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- >>> chunksOf 3 "foobarbaz"
-- ["foo","bar","baz"]
--
-- >>> chunksOf 4 "haskell.org"
-- ["hask","ell.","org"]
chunksOf :: (KnownNat n, Includes s n) => Proxy n -> SizedStrictText s -> Sized (AtMost d) [SizedStrictText (RestrictAtMost s)]
chunksOf p = trustedChangeOverSized  $ fmap trustedSized  . T.chunksOf (getN p)
{-# INLINE chunksOf #-}

-- ----------------------------------------------------------------------------

-- * Searching

-------------------------------------------------------------------------------

-- ** Searching with a predicate

-- | /O(n)/ The 'elem' function takes a character and a 'SizedStrictText', and
-- returns 'True' if the element is found in the given 'SizedStrictText', or
-- 'False' otherwise.
#if MIN_VERSION_text(1,2,5)
elem :: Char -> SizedStrictText s -> Bool
elem p = T.elem p . getSized
{-# INLINE elem #-}
#endif

-- | /O(n)/ The 'find' function takes a predicate and a 'SizedStrictText', and
-- returns the first element matching the predicate, or 'Nothing' if
-- there is no such element.
find :: (Char -> Bool) -> SizedStrictText s -> Maybe Char
find p = T.find p . getSized
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate and a 'SizedStrictText',
-- and returns the pair of 'Text's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
partition :: (Char -> Bool) -> SizedStrictText s -> (SizedStrictText (RestrictAtMost s), SizedStrictText (RestrictAtMost s))
partition p = bimap trustedSized trustedSized . T.partition p . getSized
{-# INLINE partition #-}

-- | /O(n)/ 'filter', applied to a predicate and a 'SizedStrictText',
-- returns a 'Text' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> SizedStrictText s -> SizedStrictText (RestrictAtMost s)
filter p = trustedChangeOverSized $ T.filter p
{-# INLINE filter #-}

-- | /O(n+m)/ Find the first instance of @needle@ (which must be
-- non-'null') in @haystack@.  The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- >>> breakOn "::" "a::b::c"
-- ("a","::b::c")
--
-- >>> breakOn "/" "foobar"
-- ("foobar","")
--
-- Laws:
--
-- > append prefix match == haystack
-- >   where (prefix, match) = breakOn needle haystack
--
-- If you need to break a string by a substring repeatedly (e.g. you
-- want to break on every instance of a substring), use 'breakOnAll'
-- instead, as it has lower startup overhead.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
breakOn :: SizedStrictText s -> SizedStrictText s' -> (SizedStrictText (RestrictAtMost s), SizedStrictText (RestrictAtMost s))
breakOn t = bimap trustedSized trustedSized . T.breakOn (getSized t) . getSized 
{-# INLINE breakOn #-}

-- | /O(n+m)/ Similar to 'breakOn', but searches from the end of the
-- string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- >>> breakOnEnd "::" "a::b::c"
-- ("a::b::","c")
breakOnEnd :: SizedStrictText s -> SizedStrictText s' -> (SizedStrictText (RestrictAtMost s), SizedStrictText (RestrictAtMost s))
breakOnEnd t = bimap trustedSized trustedSized . T.breakOnEnd (getSized t) . getSized
{-# INLINE breakOnEnd #-}

-- | /O(n+m)/ Find all non-overlapping instances of @needle@ in
-- @haystack@.  Each element of the returned list consists of a pair:
--
-- * The entire string prior to the /k/th match (i.e. the prefix)
--
-- * The /k/th match, followed by the remainder of the string
--
-- Examples:
--
-- >>> breakOnAll "::" ""
-- []
--
-- >>> breakOnAll "/" "a/b/c/"
-- [("a","/b/c/"),("a/b","/c/"),("a/b/c","/")]
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- The @needle@ parameter may not be empty.
breakOnAll ::
  -- | @needle@ to search for
  SizedStrictText s ->
  -- | @haystack@ in which to search
  SizedStrictText s' ->
  Sized (RestrictAtMost s) [(SizedStrictText (RestrictAtMost s), SizedStrictText (RestrictAtMost s))]
breakOnAll x y = trustedSized  $ fmap (bimap trustedSized trustedSized) $ T.breakOnAll (getSized x) (getSized y)
{-# INLINE breakOnAll #-}

-------------------------------------------------------------------------------

-- ** Indexing 'SizedStrictText's

-- $index
--
-- If you think of a 'SizedStrictText' value as an array of 'Char' values (which
-- it is not), you run the risk of writing inefficient code.
--
-- An idiom that is common in some languages is to find the numeric
-- offset of a character or substring, then use that number to split
-- or trim the searched string.  With a 'SizedStrictText' value, this approach
-- would require two /O(n)/ operations: one to perform the search, and
-- one to operate from wherever the search ended.
--
-- For example, suppose you have a string that you want to split on
-- the substring @\"::\"@, such as @\"foo::bar::quux\"@. Instead of
-- searching for the index of @\"::\"@ and taking the substrings
-- before and after that index, you would instead use @breakOnAll \"::\"@.

-- | /O(n)/ 'SizedStrictText' index (subscript) operator, starting from 0.
index :: (KnownNat n, Includes s n, HasCallStack) => SizedStrictText s -> Proxy n -> Char
index x = T.index (getSized x) . getN
{-# INLINE index #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'SizedStrictText'
-- and returns the index of the first element in the 'SizedStrictText' satisfying
-- the predicate.
findIndex :: (Char -> Bool) -> SizedStrictText s -> Maybe Int
findIndex p = T.findIndex p . getSized
{-# INLINE findIndex #-}

-- | /O(n+m)/ The 'count' function returns the number of times the
-- query string appears in the given 'SizedStrictText'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
count :: SizedStrictText s -> SizedStrictText s' -> Int
count x y = T.count (getSized x) (getSized y)
{-# INLINE count #-}

-------------------------------------------------------------------------------

-- * Zipping

-- | /O(n)/ 'zip' takes two 'SizedStrictText's and returns a list of
-- corresponding pairs of bytes. If one input 'SizedStrictText' is short,
-- excess elements of the longer 'SizedStrictText' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: SizedStrictText s -> SizedStrictText s -> Sized s [(Char, Char)]
zip = trustedChangeOverSized2 T.zip
{-# INLINE zip #-}

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
zipWith :: (Char -> Char -> Char) -> SizedStrictText s -> SizedStrictText s -> SizedStrictText s
zipWith p = trustedChangeOverSized2 $ T.zipWith p
{-# INLINE zipWith #-}

-- | /O(n)/ Breaks a 'SizedStrictText' up into a list of words, delimited by 'Char's
-- representing white space.
words :: SizedStrictText s -> Sized (RestrictAtMost s) [SizedStrictText (RestrictAtMost s)]
words = trustedChangeOverSized $ fmap trustedSized . T.words
{-# INLINE words #-}

-- | /O(n)/ Breaks a 'SizedStrictText' up into a list of 'SizedStrictText's at newline characters
-- @'\\n'@ (LF, line feed). The resulting strings do not contain newlines.
--
-- 'lines' __does not__ treat @'\\r'@ (CR, carriage return) as a newline character.
lines :: SizedStrictText s -> Sized (RestrictAtMost s) [SizedStrictText (RestrictAtMost s)]
lines = trustedChangeOverSized $ fmap trustedSized . T.lines
{-# INLINE lines #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: Sized s [SizedStrictText s'] -> SizedStrictText (s <*> (s' <+> Exactly 1))
unlines = trustedChangeOverSized $ T.unlines . fmap getSized
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: Sized s [SizedStrictText s'] -> SizedStrictText (s <*> (s' <+> Exactly 1))
unwords = trustedChangeOverSized $ T.unwords . fmap getSized
{-# INLINE unwords #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'SizedStrictText's and returns
-- 'True' if and only if the first is a prefix of the second.
isPrefixOf :: IsMoreGeneral s' s => SizedStrictText s -> SizedStrictText s' -> Bool
isPrefixOf x y = T.isPrefixOf (getSized x) (getSized y)
{-# INLINE isPrefixOf #-}

-- | /O(n)/ The 'isSuffixOf' function takes two 'SizedStrictText's and returns
-- 'True' if and only if the first is a suffix of the second.
isSuffixOf :: IsMoreGeneral s' s => SizedStrictText s -> SizedStrictText s' -> Bool
isSuffixOf x y = T.isSuffixOf (getSized x) (getSized y)
{-# INLINE isSuffixOf #-}

-- | /O(n+m)/ The 'isInfixOf' function takes two 'SizedStrictText's and returns
-- 'True' if and only if the first is contained, wholly and intact, anywhere
-- within the second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
isInfixOf :: (IsMoreGeneral s' s, HasCallStack) => SizedStrictText s -> SizedStrictText s' -> Bool
isInfixOf x y = T.isInfixOf (getSized x) (getSized y)
{-# INLINE isInfixOf #-}

-------------------------------------------------------------------------------

-- * View patterns

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripPrefix "foo" "foobar"
-- Just "bar"
--
-- >>> stripPrefix ""    "baz"
-- Just "baz"
--
-- >>> stripPrefix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text.Sized as T
-- >
-- > fnordLength :: SizedStrictText -> Int
-- > fnordLength (stripPrefix "fnord" -> Just suf) = T.length suf
-- > fnordLength _                                 = -1
stripPrefix :: SizedStrictText s -> SizedStrictText s' -> Maybe (SizedStrictText (s <-> s'))
stripPrefix x y = fmap trustedSized $ T.stripPrefix (getSized x) (getSized y)
{-# INLINE stripPrefix #-}

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match.
--
-- If the strings do not have a common prefix or either one is empty,
-- this function returns 'Nothing'.
--
-- Examples:
--
-- >>> commonPrefixes "foobar" "fooquux"
-- Just ("foo","bar","quux")
--
-- >>> commonPrefixes "veeble" "fetzer"
-- Nothing
--
-- >>> commonPrefixes "" "baz"
-- Nothing
commonPrefixes :: SizedStrictText s -> SizedStrictText s' -> Maybe (T.Text, T.Text, T.Text)
commonPrefixes x y = T.commonPrefixes (getSized x) (getSized y)
{-# INLINE commonPrefixes #-}

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripSuffix "bar" "foobar"
-- Just "foo"
--
-- >>> stripSuffix ""    "baz"
-- Just "baz"
--
-- >>> stripSuffix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text.Sized as T
-- >
-- > quuxLength :: SizedStrictText -> Int
-- > quuxLength (stripSuffix "quux" -> Just pre) = T.length pre
-- > quuxLength _                                = -1
stripSuffix :: SizedStrictText s -> SizedStrictText s' -> Maybe (SizedStrictText (s <-> s'))
stripSuffix x y = fmap trustedSized $ T.stripSuffix  (getSized x) (getSized y)
{-# INLINE stripSuffix #-}

getN :: KnownNat n => Proxy n -> Int
getN = fromInteger . naturalToInteger . natVal
