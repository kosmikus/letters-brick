{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Brick as B
import Brick.BChan
import Brick.Widgets.Border as B
import Brick.Widgets.Center as B
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Coerce
import Data.List as L
import Data.Text as T
import Data.Text.IO as T
import Data.Vector as V
import Graphics.Vty
import Optics
import System.Environment
import System.Random

data Pos =
  MkPos
    { _posX :: !Int -- ^ from the left
    , _posY :: !Int -- ^ from above
    }
  deriving (Eq)

asPair :: Iso' Pos (Int, Int)
asPair = iso (\ (MkPos x y) -> (x, y)) (uncurry MkPos)

newtype Time =
  MkTime Int
  deriving newtype (Num, Eq, Ord)

newtype Score =
  MkScore Int
  deriving newtype (Num, Show)

newtype Lives =
  MkLives Int
  deriving newtype (Num, Eq, Ord, Show)

data Letters =
    MkLetters
      { _lettersSize        :: !Pos
      , _lettersChooseWord  :: IO Text
      , _lettersTime        :: !Time
      , _lettersSpeed       :: !Time -- ^ number of ticks until words are stepped
      , _lettersGenSpeed    :: !Time -- ^ average number of ticks until new word is generated
      , _lettersActiveWords :: [ActiveWord] -- ^ always sorted top to bottom
      , _lettersScore       :: !Score
      , _lettersLives       :: !Lives
      }

data ActiveWord =
  MkActiveWord
    { _wordWord  :: !Text
    , _wordPos   :: !Pos  -- ^ word starting position on grid
    , _wordIndex :: !Int  -- ^ current horizontal position in word
    , _wordAge   :: !Time -- ^ ticks at current position
    }

makeLenses ''Pos
makeLenses ''Letters
makeLenses ''ActiveWord

gameOver :: Letters -> Bool
gameOver letters =
  letters ^. lettersLives <= MkLives 0

newActiveWord :: Text -> Pos -> ActiveWord
newActiveWord w pos =
  MkActiveWord w pos 0 0

ageWord :: Time -> ActiveWord -> ActiveWord
ageWord speed aw =
  let
    t' = aw ^. wordAge + 1
  in
    if t' >= speed
      then aw
        & wordPos % posY %~ (+1)
        & wordAge .~ 0
      else aw
        & wordAge .~ t'

cleanupWord :: Pos -> ActiveWord -> Either Lives ActiveWord
cleanupWord size aw =
  if aw ^. wordPos % posY >= size ^. posY
    then Left (-1)
    else Right aw

advanceActiveWord :: Char -> ActiveWord -> Either Score ActiveWord
advanceActiveWord c aw
  | c == index w idx = go (idx + 1)
  | c == index w 0   = go 1
  | otherwise        =
    Right $ aw & wordIndex .~ 0
  where
    idx = aw ^. wordIndex
    w   = aw ^. wordWord
    go :: Int -> Either Score ActiveWord
    go idx' =
      if idx' >= T.length w
        then Left $ fromIntegral (T.length w)
        else Right $ aw & wordIndex .~ idx'

partitionEither :: [Either a b] -> ([a], [b])
partitionEither =
  L.foldr
    (\ x -> case x of
      Left  b -> first  (b :)
      Right c -> second (c :)
    )
    ([], [])

advanceActiveWords :: Char -> [ActiveWord] -> (Score, [ActiveWord])
advanceActiveWords c =
  first L.sum . partitionEither . L.map (advanceActiveWord c)

advanceLetters :: Char -> Letters -> Letters
advanceLetters c letters =
  let
    (score, aws') = advanceActiveWords c (letters ^. lettersActiveWords)
  in
    letters
      & lettersActiveWords .~ aws'
      & lettersScore       %~ (+score)

cleanupWords :: Pos -> [ActiveWord] -> (Lives, [ActiveWord])
cleanupWords size =
  first L.sum . partitionEither . L.map (cleanupWord size)

advanceLevel :: Letters -> Letters
advanceLevel letters
  | levelScore >= 20000 = letters & lettersSpeed .~  4 & lettersGenSpeed .~ 12
  | levelScore >= 19000 = letters & lettersSpeed .~  4 & lettersGenSpeed .~ 13
  | levelScore >= 18000 = letters & lettersSpeed .~  4 & lettersGenSpeed .~ 14
  | levelScore >= 17000 = letters & lettersSpeed .~  4 & lettersGenSpeed .~ 15
  | levelScore >= 16000 = letters & lettersSpeed .~  5 & lettersGenSpeed .~ 15
  | levelScore >= 15000 = letters & lettersSpeed .~  5 & lettersGenSpeed .~ 16
  | levelScore >= 14000 = letters & lettersSpeed .~  6 & lettersGenSpeed .~ 16
  | levelScore >= 13000 = letters & lettersSpeed .~  6 & lettersGenSpeed .~ 17
  | levelScore >= 12000 = letters & lettersSpeed .~  7 & lettersGenSpeed .~ 17
  | levelScore >= 11000 = letters & lettersSpeed .~  7 & lettersGenSpeed .~ 18
  | levelScore >= 10000 = letters & lettersSpeed .~  8 & lettersGenSpeed .~ 18
  | levelScore >=  9000 = letters & lettersSpeed .~  8 & lettersGenSpeed .~ 19
  | levelScore >=  8000 = letters & lettersSpeed .~  9 & lettersGenSpeed .~ 19
  | levelScore >=  7000 = letters & lettersSpeed .~  9 & lettersGenSpeed .~ 20
  | levelScore >=  6000 = letters & lettersSpeed .~ 10 & lettersGenSpeed .~ 20
  | levelScore >=  5000 = letters & lettersSpeed .~ 11 & lettersGenSpeed .~ 21
  | levelScore >=  4000 = letters & lettersSpeed .~ 12 & lettersGenSpeed .~ 23
  | levelScore >=  3000 = letters & lettersSpeed .~ 14 & lettersGenSpeed .~ 25
  | levelScore >=  2000 = letters & lettersSpeed .~ 16 & lettersGenSpeed .~ 30
  | levelScore >=  1000 = letters & lettersSpeed .~ 18 & lettersGenSpeed .~ 40
  | otherwise           = letters & lettersSpeed .~ 20 & lettersGenSpeed .~ 50
  where
    levelScore :: Int
    levelScore =
        coerce (letters ^. lettersScore) * 20
      + coerce (letters ^. lettersTime)

ageLetters :: Letters -> IO Letters
ageLetters letters = do
  let
    size = letters ^. lettersSize
    (lives, aws') =
          cleanupWords size
      $   ageWord (letters ^. lettersSpeed)
      <$> letters ^. lettersActiveWords
    shouldGenerate :: IO Bool
    shouldGenerate
      | L.null aws' = pure True -- always generate when board is empty
      | L.any ((== 0) . (^. wordPos % posY)) aws' = pure False -- don't generate two words on the same line
      | otherwise = checkSpeed (letters ^. lettersGenSpeed)
  g <- shouldGenerate
  newAws <- if g
    then do
      w <- letters ^. lettersChooseWord
      p <- genPositionFor w size
      pure [newActiveWord w p]
    else pure []
  pure $ advanceLevel $ letters
    & lettersTime %~ (+1)
    & lettersActiveWords .~ newAws L.++ aws'
    & lettersLives %~ (+lives)

genPositionFor :: Text -> Pos -> IO Pos
genPositionFor w size = do
  let maxX = size ^. posX - T.length w
  x <- randomRIO (0, maxX - 1)
  pure (MkPos x 0)

checkSpeed :: Time -> IO Bool
checkSpeed (MkTime t) = do
  i <- randomRIO (0, t - 1)
  pure (i == 0)

setupDictionary :: FilePath -> IO (IO Text)
setupDictionary file = do
  dict <- V.fromList . T.lines <$> T.readFile file
  let
    dictSize = V.length dict
  pure $ (dict V.!) <$> randomRIO (0, dictSize - 1)

drawUI :: Letters -> Widget n
drawUI letters =
    withAttr "board"
  $ B.centerWith (Just '=')
  $ joinBorders $ border
    ( ( setAvailableSize
          (letters ^. lettersSize % asPair)
      $ vBox
      $ boardLine 0 (letters ^. lettersActiveWords)
      )
      <=>
      ( hLimit (letters ^. lettersSize % posX)
      $ withAttr "scoreline"
      $ vBox
        [ hBorder
        , vLimit 1 $ hBox
          [ txt "Score: "
          , str (show (letters ^. lettersScore))
          , fill ' '
          , txt "Lives: "
          , str (show (letters ^. lettersLives))
          ]
        ]
      )
    )

boardLine :: Int -> [ActiveWord] -> [Widget n]
boardLine _ [] = [fill ' ']
boardLine y (aw : aws) =
  case compare y (aw ^. wordPos % posY) of
    LT -> vLimit 1 (fill ' ') : boardLine (y + 1) (aw : aws)
    EQ -> (txt (T.replicate (aw ^. wordPos % posX) " ") <+> drawActiveWord aw) : boardLine (y + 1) aws
    GT -> [] -- error; should not occur

drawActiveWord :: ActiveWord -> Widget n
drawActiveWord aw =
  let
    (covered, uncovered) = T.splitAt (aw ^. wordIndex) (aw ^. wordWord)
  in
    withAttr "covered" (txt covered) <+> withAttr "uncovered" (txt uncovered)

handler :: Letters -> BrickEvent () () -> EventM () (Next Letters)
handler letters (VtyEvent (EvKey KEsc [])) =
  halt letters
handler letters _
  | gameOver letters = continue letters
handler letters (VtyEvent (EvKey (KChar c) [])) =
  continue $ advanceLetters c letters
handler letters (AppEvent ()) =
  liftIO (ageLetters letters) >>= continue
handler letters _             =
  continue letters

lettersApp :: App Letters () ()
lettersApp = App
  { appDraw = pure . drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handler
  , appStartEvent = pure
  , appAttrMap = \ letters ->
    lettersAttrMap (not (gameOver letters))
  }

gray :: Color
gray = rgbColor @Int 20 20 20

purple :: Color
purple = rgbColor @Int 196 0 196

lettersAttrMap :: Bool -> AttrMap
lettersAttrMap alive =
  attrMap (white' `on` black)
    [ ("board", purple' `on` black)
    , ("covered", black `on` white')
    , ("uncovered", white' `on` black)
    , ("scoreline", yellow `on` black)
    ]
  where
    white'  = if alive then white else gray
    purple' = if alive then purple else gray


initialLetters :: IO Text -> Letters
initialLetters dict = MkLetters
  { _lettersSize        = MkPos 80 25
  , _lettersChooseWord  = dict
  , _lettersTime        = MkTime 0
  , _lettersSpeed       = MkTime 7
  , _lettersGenSpeed    = MkTime 20
  , _lettersActiveWords = []
  , _lettersScore       = MkScore 0
  , _lettersLives       = MkLives 3
  }

main :: IO ()
main = do
  [file] <- getArgs
  dict <- setupDictionary file
  let vtyCfg = mkVty defaultConfig
  vty <- vtyCfg
  chan <- newBChan 5
  _ <- forkIO $ forever $ do
    writeBChan chan ()
    threadDelay 100000
  _ <- customMain
    vty
    vtyCfg
    (Just chan)
    lettersApp
    (initialLetters dict)
  pure ()
