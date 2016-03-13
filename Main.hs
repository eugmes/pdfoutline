{-# LANGUAGE OverloadedStrings #-}
{-
Author: Eugeniy Meshcheryakov <eugen@debian.org>

This program adds outlines to pdf files.
Usage: pdfoutline input.pdf [outline.txt] out.pdf

File given as second argument should contain outline information in
form:

<level> <page> Some text

where <level> and <page> are integers. Values for <level> should be greater
or equal than that of first line. Page numeration starts with 1.

Outlines file can contain comments that start with # in first column. Comments
and empty lines are ignored.

Example file:
0 1 Document title
1 1 Chapter 1
2 1 Chapter 1.1
2 2 Chapter 1.2
1 3 Chapter 2

This file will result in outline like the following:

Document title
+-Chapter 1
| +-Chapter 1.1
| +-Chapter 1.2
+-Chapter 2

The program removes existing outlines from file if run with only two arguments.
-}
import System.IO
import qualified System.IO.Streams as Streams
import System.Environment
import System.Exit
import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types
import Text.Parsec.String
import PdfUtils
import PdfOutlines
import OutlinesParser

getInputStream :: ExceptT PdfError (Pdf' IO) IS
getInputStream = do
    ris <- getRIS
    seek ris 0
    inputStream ris

addOutlines :: [OutlineEntry] -> FilePath -> FilePath -> IO (Either PdfError ())
addOutlines [] input output =
    withBinaryFile input ReadMode $ \handle ->
        runPdfWithHandle handle knownFilters $ do
            pdf <- document
            catalog <- documentCatalog pdf
            let Catalog catalogRef catalogDict = catalog

            case lookupDict' "Outlines" catalogDict of
                Nothing -> do
                    istream <- getInputStream
                    liftIO $ Streams.withFileAsOutput output $ Streams.supply istream
                Just _ -> do
                    let Document xref tr = pdf
                    let startxref =
                            case xref of
                                XRefTable off -> off
                                XRefStream off _ -> off
                    let newTr = setValueForKey "Prev" (ONumber $ NumInt $ fromIntegral startxref) tr

                    let newCatalogDict = deleteValueForKey "Outlines" catalogDict

                    fileSize <- getRIS >>= size
                    istream <- getInputStream

                    liftIO $ Streams.withFileAsOutput output $ \ostream -> do
                        Streams.supply istream ostream
                        runPdfWriter ostream $ do
                            writeObject catalogRef $ ODict newCatalogDict
                            writeXRefTable fileSize newTr

addOutlines ents input output =
    withBinaryFile input ReadMode $ \handle ->
        runPdfWithHandle handle knownFilters $ do
            pdf <- document
            catalog <- documentCatalog pdf
            let Document xref tr = pdf
                Catalog catalogRef catalogDict = catalog

            pageNode <- catalogPageNode catalog
            pageRefs <- pageNodeKids pageNode

            let startxref =
                    case xref of
                        XRefTable off -> off
                        XRefStream off _ -> off
            let newTr = setValueForKey "Prev" (ONumber $ NumInt $ fromIntegral startxref) tr

            freeIdx <- nextFreeRefIndex 1 xref
            let outlinesRef = Ref freeIdx 0
            let pdfOutlines = snd $ convertEntries (freeIdx + 1) outlinesRef pageRefs ents
            let outlines = buildOutlinesDict pdfOutlines
            let newCatalogDict = setValueForKey "Outlines" (ORef outlinesRef) catalogDict

            fileSize <- getRIS >>= size
            istream <- getInputStream

            liftIO $ Streams.withFileAsOutput output $ \ostream -> do
                Streams.supply istream ostream
                runPdfWriter ostream $ do
                    writeObject catalogRef $ ODict newCatalogDict
                    writeObject outlinesRef $ ODict outlines
                    mapM_ writeOutlineEntry pdfOutlines
                    writeXRefTable fileSize newTr

printUsage :: IO ()
printUsage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " input.pdf [outlines.txt] output.pdf"

main :: IO ()
main = do
    (input, output, outlineEntries) <- do
        args <- getArgs
        case args of
            [input', output'] -> return (input', output', [])
            [input', outlinesFileName, output'] -> do
                result <- parseFromFile outlinesFile outlinesFileName
                ents <- case result of
                    Left e -> print e >> exitFailure
                    Right ents' -> return ents'
                return (input', output', ents)
            _ -> printUsage >> exitFailure

    res <- addOutlines outlineEntries input output
    case res of
        Left e -> print e >> exitFailure
        Right _ -> return ()
