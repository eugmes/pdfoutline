{-# LANGUAGE OverloadedStrings #-}
module PdfOutlines ( OutlineEntry(..)
                   , PdfOutlineTreeEntry(..)
                   , convertEntries
                   , buildOutlinesDict
                   , writeOutlineEntry
                   ) where

import Data.String
import Pdf.Toolbox.Core

data OutlineEntry = OutlineEntry Int String [OutlineEntry] 
    deriving Show

data PdfOutlineTreeEntry = PdfOutlineTreeEntry Ref Dict [PdfOutlineTreeEntry]
    deriving Show

data RawPdfOutlineTreeEntry = RawPdfOutlineTreeEntry Dict [RawPdfOutlineTreeEntry]
    deriving Show

buildEntry :: [Ref] -> OutlineEntry -> RawPdfOutlineTreeEntry
buildEntry pages (OutlineEntry page title subents) =
    RawPdfOutlineTreeEntry dict' children
  where
    dict = Dict [("Dest", OArray $ Array [ORef $ pages !! (page - 1), OName "XYZ", ONull, ONull, ONull]),
                 ("Title", OStr $ Str $ fromString title)]
    (dict', children) = case subents of
        [] -> (dict, [])
        _ -> (setValueForKey "Count" (ONumber $ NumInt $ length subents) dict, map (buildEntry pages) subents)

-- NOTE the oder of entries is inversed
numberTree :: Int -> [RawPdfOutlineTreeEntry] -> (Int, [PdfOutlineTreeEntry])
numberTree firstIndex ents =
    numberTreeRec firstIndex ents []
  where
    numberTreeRec :: Int -> [RawPdfOutlineTreeEntry] -> [PdfOutlineTreeEntry] -> (Int, [PdfOutlineTreeEntry])
    numberTreeRec idx [] result = (idx, result)
    numberTreeRec idx (RawPdfOutlineTreeEntry dict children : xs) result =
        numberTreeRec idx' xs $ PdfOutlineTreeEntry (Ref idx 0) dict children' : result
      where
        (idx', children') = numberTreeRec (idx + 1) children []
                
linkDepth :: Ref -> PdfOutlineTreeEntry -> PdfOutlineTreeEntry
linkDepth parent (PdfOutlineTreeEntry ref dict children) =
    PdfOutlineTreeEntry ref (setValueForKey "Parent" (ORef parent) dict') (map (linkDepth ref) children)
  where
    -- NOTE this assumes inverse order
    dict' = case children of
        [] -> dict
        _ -> let (PdfOutlineTreeEntry lastRef _ _) = head children
                 (PdfOutlineTreeEntry firstRef _ _) = last children
             in setValueForKey "Last" (ORef lastRef) $ setValueForKey "First" (ORef firstRef) dict

-- NOTE the order is inversed once more
linkWidth :: [PdfOutlineTreeEntry] -> [PdfOutlineTreeEntry]
linkWidth ents =
    linkWidthRec ents []
  where
    linkWidthRec [] result = result
    linkWidthRec (PdfOutlineTreeEntry ref dict children : xs) [] =
        linkWidthRec xs [PdfOutlineTreeEntry ref dict children']
      where
        children' = linkWidth children
    linkWidthRec (PdfOutlineTreeEntry ref dict children : xs) (PdfOutlineTreeEntry xref xdict xchildren : ys) =
        linkWidthRec xs $ PdfOutlineTreeEntry ref dict' children' : PdfOutlineTreeEntry xref xdict' xchildren : ys
      where
        children' = linkWidth children
        dict' = setValueForKey "Next" (ORef xref) dict
        xdict' = setValueForKey "Prev" (ORef ref) xdict

convertEntries :: Int -> Ref -> [Ref] -> [OutlineEntry] -> (Int, [PdfOutlineTreeEntry])
convertEntries idx parent pages xs =
    let raw = map (buildEntry pages) xs
        (idx', numbered) = numberTree idx raw
        linked = linkWidth (map (linkDepth parent) numbered)
    in (idx', linked)

buildOutlinesDict :: [PdfOutlineTreeEntry] -> Dict
buildOutlinesDict [] = Dict [("Type", OName "Outlines")]

buildOutlinesDict xs =
    Dict [("Type", OName "Outlines"),
          ("First", ORef firstRef),
          ("Last", ORef lastRef),
          ("Count", ONumber $ NumInt $ length xs)]
  where
    ref (PdfOutlineTreeEntry r _ _) = r
    firstRef = ref $ head xs
    lastRef = ref $ last xs

writeOutlineEntry :: MonadIO m => PdfOutlineTreeEntry -> PdfWriter m ()
writeOutlineEntry (PdfOutlineTreeEntry ref dict children) = do
    writeObject ref (ODict dict)
    mapM_ writeOutlineEntry children
