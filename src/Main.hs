import Graphics.Gloss (play, white, red, green, Display(InWindow))
import Graphics.Gloss.Data.Picture
    (Picture, rectangleSolid, thickCircle, translate, pictures, color, rotate,
     blank)
import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), KeyState (Up), MouseButton (LeftButton),
     Key (MouseButton))
import qualified Data.Map as M
import Data.List (maximumBy)
import Data.Ord (comparing)

type Board = M.Map (Int, Int) Marker

data Marker = X | O | Blank deriving Eq

main :: IO ()
main = play
    (InWindow "Tic-Tac-Toe" (300, 300) (100, 100))
    white
    20
    M.empty
    renderBoard
    handleInput
    step

renderMarker :: Marker -> Picture
renderMarker X = color red $ rotate 45 $
                     pictures [rectangleSolid 90 10, rectangleSolid 10 90]
renderMarker O = color green $ thickCircle 35 10
renderMarker _ = blank

getMarker :: Board -> (Int, Int) -> Marker
getMarker = flip $ M.findWithDefault Blank

renderBoard :: Board -> Picture
renderBoard b =
    pictures $ [ translate    0   50  $ rectangleSolid 300   3
               , translate    0 (-50) $ rectangleSolid 300   3
               , translate   50    0  $ rectangleSolid   3 300
               , translate (-50)   0  $ rectangleSolid   3 300
               ] ++
               [ translate
                    ((fromIntegral x - 1) * 100)
                    ((fromIntegral y - 1) * 100) $
                    renderMarker $ getMarker b (x, y)
               | x <- [0..2]
               , y <- [0..2] ]

currentPlayer :: Board -> Marker
currentPlayer b | odd $ length $ availableMoves b = X
                | otherwise                       = O

availableMoves :: Board -> [(Int, Int)]
availableMoves b = case winner b of Nothing -> [ (x, y)
                                               | x <- [0..2]
                                               , y <- [0..2]
                                               , getMarker b (x, y) == Blank]
                                    _       -> []

handleInput :: Event -> Board -> Board
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) b =
    if getMarker b (snap x, snap y) == Blank && currentPlayer b == X
    then M.insert (snap x, snap y) X b
    else b
    where
        snap = (+1) . max (-1) . min 1 . floor . (/100) . (+50)
handleInput _ b = b

step :: Float -> Board -> Board
step _ b =
    if currentPlayer b == O && (not . null . availableMoves $ b)
    then aiMove b
    else b

aiMove :: Board -> Board
aiMove b = fst .
           maximumBy (comparing snd) .
           map (\m -> (m, aiMinMax m False)) $
           map (\k -> M.insert k (currentPlayer b) b) (availableMoves b)

aiMinMax :: Board -> Bool -> Int
aiMinMax board maximize
    | null $ availableMoves board = scoreBoard board
    | otherwise =
        let moves = map
                (\k -> aiMinMax
                    (M.insert k (currentPlayer board) board)
                    (not maximize))
                (availableMoves board)
        in if maximize then maximum moves else minimum moves

allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (==x) xs

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _      = Nothing

winner :: Board -> Maybe Marker
winner b = maybeHead . map head .
               filter (\xs -> head xs /= Blank && allSame xs) .
               map (map $ getMarker b) $ rows (3, 3) 3

scoreBoard :: Board -> Int
scoreBoard b = case winner b of Just X  -> -1
                                Just O  ->  1
                                _       ->  0

directions :: [(Int, Int) -> (Int, Int)]
directions = [ (\(x, y) -> (x+1, y  ))
             , (\(x, y) -> (x  , y+1))
             , (\(x, y) -> (x+1, y+1))
             , (\(x, y) -> (x+1, y-1))
             ]

rows :: (Int, Int) -> Int -> [[(Int, Int)]]
rows (x, y) l =
    filter (all (\(a, b) -> a >= 0 && b >= 0 && a < x && b < y))
    [take l (iterate d (a, b)) | a <- [0..x-1], b <- [0..y-1], d <- directions]
