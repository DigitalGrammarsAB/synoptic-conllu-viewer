module SynopticView where

import Data.Functor
import Data.List.Extra (splitOn, lower, intercalate, (\\), transpose, isInfixOf)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import UDConcepts hiding (prReducedUDSentence)

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "static"
        } setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Synoptic CoNNL-U viewer"
    UI.addStyleSheet window "style.css"
    -- create input field
    searchInput <- UI.input
    element searchInput # set 
        (UI.attr "placeholder") 
        "function name or pipe-separated strings (one per language)"
    element searchInput # set (UI.attr "size") "60"
    -- create search button
    searchButton <- UI.button
    element searchButton # set UI.text "search"
    -- create checkboxes (& their labels) for ud fields
    checkBoxes <- mapM mkLabelledCheckbox udFieldNames
    -- add graphical elements to window body
    getBody window #+ ([element searchInput, element searchButton] 
        ++ [element e | 
                e <- map fst checkBoxes `interleave` map snd checkBoxes])
    -- handle button click event
    on UI.click searchButton $ search searchInput checkBoxes
    where 
        mkLabelledCheckbox label = do
            checkBox <- UI.input
            element checkBox # set UI.type_ "checkbox"
            element checkBox # set UI.id_ label
            element checkBox # if label `elem` checkedByDefault
                then set UI.checked True
                else set UI.checked False
            idLabel <- UI.label
            element idLabel # set UI.for label
            element idLabel # set UI.text label
            return (checkBox,idLabel)
        search searchInput checkBoxes = const $ do
            -- get text input -> lemmas/arbitrary strings to search
            searchString <- get value searchInput
            -- get checkbox values -> pattern for prReducedUDSentence
            checkboxVals <- mapM (get UI.checked . fst) checkBoxes
            let pattern = map (\v -> if v then 'x' else '_') checkboxVals
            -- parse conllu files to get sentences
            sentences <- liftIO $ mapM parseUDFile conllus <&> transpose
            -- find sentences matching the search string
            let matchingSentences = filter (match searchString) sentences
            -- update and show tables
            updateTable window matchingSentences pattern
        checkedByDefault = udFieldNames \\ ["XPOS", "FEATS", "DEPS", "MISC"]

udFieldNames :: [String]
udFieldNames = [
    "ID", "FORM", "LEMMA", "UPOS", "XPOS", 
    "FEATS", "HEAD", "DEPREL", "DEPS", "MISC"
    ]

updateTable :: Window -> [[UDSentence]] -> String -> UI Element
updateTable window sentences pattern = do
    -- destroy existing tables
    destroyTable window
    -- build and populate new table
    table <- buildTable window sentences pattern
    -- add table to window body
    getBody window #+ [element table]
    where 
        destroyTable window = do
            existingTables <- getElementsByClassName window "table"
            mapM_ delete existingTables
        
        buildTable window sentences pattern = do
            cells <- mapM 
                (mapM (return . string . prReducedUDSentence pattern)) 
                sentences
            table <- UI.grid cells
            return table

match :: String -> [UDSentence] -> Bool
searchString `match` sents =  if '|' `elem` searchString 
    then all 
            (\(l,s) -> l `isInfixOf` (prUDSentence 0) s) 
            (strings `zip` sents)
    else all 
            (\(l,s) -> l `elem` map udLEMMA (udWordLines s)) 
            (lemmas `zip` sents)
    where 
        lemmas = init $ splitOn "_" searchString
        strings = splitOn " | " searchString

interleave :: [a] -> [a] -> [a]
interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
interleave _        _        = []

-- | alternative to gf-ud's prReducedUDSentence (does not print comments)
prReducedUDSentence :: String -> UDSentence -> String
prReducedUDSentence parts s = unlines (map prReducedUDWord (udWordLines s))
  where
    prReducedUDWord w = 
        intercalate "\t" [p | (p,b) <- zip (prUDWordParts w) pattern, b]
    pattern = map (/='_') parts

conllus = [
    "../data/descriptionsEng.conllu", 
    "../data/descriptionsFin.conllu", 
    "../data/descriptionsGer.conllu", 
    "../data/descriptionsSwe.conllu"
    ]
