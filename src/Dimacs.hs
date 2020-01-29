{-# LANGUAGE OverloadedStrings #-}
module Dimacs where

import Control.Monad
import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Suma.Types

-- | Parse a text according to
-- https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
--
-- Note that we don't currently handle multiple clauses on the same line, or
-- omitting the 0 on the final clause, behaviour which is exhibited in some
-- examples.
parseDimacsCnf :: Text -> Maybe Formula
parseDimacsCnf = go . T.lines
  where
    go :: [Text] -> Maybe Formula
    go [] = Nothing
    go (l:ls) = case comment l of
      Nothing -> problem l *> go2 ls
      Just () -> go ls

    go2 :: [Text] -> Maybe Formula
    go2 [] = Just V.empty
    go2 (l:ls) = case comment l of
      Nothing -> V.cons <$> clause l <*> go2 ls
      Just () -> go2 ls

    int :: Text -> Maybe Int
    int t = case T.unsnoc t of
      Nothing -> Just 0
      Just (ds, d)
        | isDigit d -> do
          n <- int ds
          pure $ 10 * n + digitToInt d
        | otherwise -> Nothing

    comment :: Text -> Maybe ()
    comment t = case T.head t of
      'c' -> Just ()
      _   -> Nothing

    problem :: Text -> Maybe (Int,Int)
    problem t = case T.words t of
      ["p","cnf",v,n] -> (,) <$> int v <*> int n
      _ -> Nothing

    clause :: Text -> Maybe Clause
    clause t = do
      let ns = T.words t
      0 <- int (last ns)
      forM (init ns) $ \n -> do
        (n', p) <- case T.uncons n of
          Nothing -> Nothing
          Just ('-',n') -> Just (n', False)
          _ -> Just (n, True)
        n'' <- int n'
        pure (n'', p)
