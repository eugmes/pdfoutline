module PdfUtils (nextFreeRefIndex) where

import Control.Monad
import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Core.Parsers.XRef

nextFreeRefIndex :: MonadIO m => Int -> XRef -> Pdf m Int
nextFreeRefIndex idx xref = do
    idx' <- nextFreeRefIndex' idx xref
    ris <- getRIS
    prev <- prevXRef ris xref
    case prev of
        Just xref' -> nextFreeRefIndex idx' xref'
        Nothing -> return idx'

nextFreeRefIndex' :: MonadIO m => Int -> XRef -> Pdf m Int
nextFreeRefIndex' idx (XRefTable off) = do
    ris <- getRIS
    seek ris off
    _ <- inputStream ris >>= isTable
    nextFreeRefIndexInTable idx ris

-- TODO implement support for XRef streams
nextFreeRefIndex' _ (XRefStream _ _) = throwE $ UnexpectedError "XRef streams are not supported"

subsectionHeader :: MonadIO m => IS -> PdfE m (Int, Int)
subsectionHeader = parse parseSubsectionHeader

nextSubsectionHeader :: MonadIO m => IS -> Int -> PdfE m (Maybe (Int, Int))
nextSubsectionHeader is count = do
    skipSubsection is count
    hush `liftM` runExceptT (subsectionHeader is)

skipSubsection :: MonadIO m => IS -> Int -> PdfE m ()
skipSubsection is count = dropExactly (count * 20) is

nextFreeRefIndexInTable :: MonadIO m => Int -> RIS -> Pdf m Int
nextFreeRefIndexInTable firstIndex ris =
    inputStream ris >>= subsectionHeader >>= go firstIndex
  where
    go idx (start, count) = do
        let idx' = max idx (start + count)
        is <- inputStream ris
        nextSubsectionHeader is count >>= maybe (return idx') (go idx')
