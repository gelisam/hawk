{-# LANGUAGE OverloadedStrings,FlexibleInstances,FlexibleContexts,DefaultSignatures #-}

module HSL.Types where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intersperse)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)


-- Type witnesses

s :: B.ByteString
s = undefined

i :: Int
i = undefined

f :: Float
f = undefined


class RenderableItem a where
    renderItem :: a -> B.ByteString

    default renderItem :: Show a => a -> B.ByteString
    renderItem = B.pack . show

class RenderableLine a where
    -- do not include the terminating newline
    renderLine :: a -> B.ByteString

    default renderLine :: RenderableItem a => a -> B.ByteString
    renderLine = renderItem

class RenderableFile a where
    -- include the terminating newline
    renderFile :: a -> B.ByteString

    default renderFile :: RenderableLine a => a -> B.ByteString
    renderFile = B.unlines . return . renderLine


class ParsableFromItem a where
    parseItem :: B.ByteString -> a

    default parseItem :: Read a => B.ByteString -> a
    parseItem = read . B.unpack

class ParsableFromLine a where
    -- does not include terminating newline
    parseLine :: B.ByteString -> a

    default parseLine :: ParsableFromItem a => B.ByteString -> a
    parseLine = parseItem

class ParsableFromFile a where
    -- includes the terminating newline
    parseFile :: B.ByteString -> a


-- Nothing has a different meaning depending on where it's used.
-- * top-level: failure
-- * item-level: empty cell

instance RenderableFile a => RenderableFile (Maybe a) where
    renderFile (Just a) = renderFile a
    renderFile Nothing = error "Nothing"

instance RenderableItem a => RenderableItem (Maybe a) where
    renderItem (Just a) = renderItem a
    renderItem Nothing = ""

instance ParsableFromItem a => ParsableFromItem (Maybe a) where
    parseItem "" = Nothing
    parseItem xs = Just $ parseItem xs


-- instantiate the default instances

instance ParsableFromItem Int
instance ParsableFromItem Int64
instance ParsableFromItem Char
instance ParsableFromItem String where parseItem = B.unpack
instance ParsableFromItem B.ByteString where parseItem = id

instance ParsableFromLine Int
instance ParsableFromLine Int64
instance ParsableFromLine Char
instance ParsableFromLine String
instance ParsableFromLine B.ByteString

-- no default instance for ParsableFromFile, we want to parse "1" as [1]

instance RenderableItem Int
instance RenderableItem Int64
instance RenderableItem Char
instance RenderableItem String where renderItem = B.pack
instance RenderableItem B.ByteString where renderItem = id

instance RenderableLine Int
instance RenderableLine Int64
instance RenderableLine Char
instance RenderableLine String
instance RenderableLine B.ByteString

instance RenderableFile Int
instance RenderableFile Int64
instance RenderableFile Char
-- instance RenderableFile String  -- ambiguous, fixed by hack below
instance RenderableFile B.ByteString


instance (ParsableFromItem a, ParsableFromItem b) => ParsableFromLine (a, b) where
    parseLine xs = let [a, b] = B.split '\t' xs in (parseItem a, parseItem b)
instance (ParsableFromItem a, ParsableFromItem b, ParsableFromItem c) => ParsableFromLine (a, b, c) where
    parseLine xs = let [a, b, c] = B.split '\t' xs in (parseItem a, parseItem b, parseItem c)
instance (ParsableFromItem a, ParsableFromItem b, ParsableFromItem c, ParsableFromItem d) => ParsableFromLine (a, b, c, d) where
    parseLine xs = let [a, b, c, d] = B.split '\t' xs in (parseItem a, parseItem b, parseItem c, parseItem d)
instance (ParsableFromItem a, ParsableFromItem b, ParsableFromItem c, ParsableFromItem d, ParsableFromItem e) => ParsableFromLine (a, b, c, d, e) where
    parseLine xs = let [a, b, c, d, e] = B.split '\t' xs in (parseItem a, parseItem b, parseItem c, parseItem d, parseItem e)

instance (RenderableItem a, RenderableItem b) => RenderableLine (a, b) where
    renderLine (a, b) = B.concat $ intersperse "\t" [renderItem a, renderItem b]
instance (RenderableItem a, RenderableItem b, RenderableItem c) => RenderableLine (a, b, c) where
    renderLine (a, b, c) = B.concat $ intersperse "\t" [renderItem a, renderItem b, renderItem c]
instance (RenderableItem a, RenderableItem b, RenderableItem c, RenderableItem d) => RenderableLine (a, b, c, d) where
    renderLine (a, b, c, d) = B.concat $ intersperse "\t" [renderItem a, renderItem b, renderItem c, renderItem d]
instance (RenderableItem a, RenderableItem b, RenderableItem c, RenderableItem d, RenderableItem e) => RenderableLine (a, b, c, d, e) where
    renderLine (a, b, c, d, e) = B.concat $ intersperse "\t" [renderItem a, renderItem b, renderItem c, renderItem d, renderItem e]


-- hack:
--   We could not instantiate `RenderableFile String` above, but we don't want
--   to display one character per line. Luckily, all `RenderableLine` instances
--   happen to have `Show` instances, so we can use `show` to distinguish "..."
--   from [...] and treat strings specially.
instance (Show a, RenderableLine a) => RenderableFile [a] where
    renderFile xs = if isString xs then renderString xs
                                   else renderFile' xs
                    where
      isString = (=='"') . head . show
      toString = read . show
      renderString = B.unlines . return . B.pack . toString
      renderFile' = B.unlines . fmap renderLine

instance ParsableFromLine a => ParsableFromFile [a] where
    parseFile = fmap parseLine . B.lines
