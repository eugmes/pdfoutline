module OutlinesParser (outlinesFile) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Maybe
import PdfOutlines

number :: Parser Int
number = read <$> many1 digit

entry :: Parser (Int, OutlineEntry)
entry = do
    level <- number
    void $ char ' '
    page <- number
    void $ char ' '
    title <- many $ noneOf "\r\n"
    return (level, OutlineEntry page title [])

comment :: Parser ()
comment = do
    void $ char '#'
    void $ many $ noneOf "\r\n"
    return ()

maybeOutlineEntry :: Parser (Maybe (Int, OutlineEntry))
maybeOutlineEntry = (comment >> return Nothing) <|> fmap Just entry

unflattenOutlines :: Int -> [OutlineEntry] -> [(Int, OutlineEntry)] -> ([OutlineEntry], [(Int, OutlineEntry)])
unflattenOutlines _ tree [] = (tree, [])

unflattenOutlines level tree (flat@((level', x) : xs))
  | level' < level = (tree, flat)
  | level' == level = unflattenOutlines level (x : tree) xs
  | otherwise =
    let (tree', flat') = unflattenOutlines level' [x] xs in
        case tree of
        [] -> unflattenOutlines level' tree' flat'
        OutlineEntry page title _ : tRest -> unflattenOutlines level' (OutlineEntry page title tree' : tRest) flat'

flipOutlines :: [OutlineEntry] -> [OutlineEntry]
flipOutlines [] = []
flipOutlines (OutlineEntry page title children : xs) =
    flipOutlines xs ++ [OutlineEntry page title (flipOutlines children)]

outlinesFile :: Parser [OutlineEntry]
outlinesFile = do
    spaces
    l <- sepEndBy maybeOutlineEntry spaces
    eof
    let (tree, []) = unflattenOutlines 0 [] (catMaybes l)
    return $ flipOutlines tree
