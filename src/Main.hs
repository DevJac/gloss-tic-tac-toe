import Graphics.Gloss (play, white, red, green, Display(InWindow))
import Graphics.Gloss.Data.Picture
    (Picture, rectangleSolid, thickCircle, translate, pictures, color, rotate, blank)
import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), KeyState (Up), MouseButton (LeftButton), Key (MouseButton))
import qualified Data.Map as M

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
renderMarker X = color red $ rotate 45 $ pictures [rectangleSolid 90 10, rectangleSolid 10 90]
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
               [ translate ((fromIntegral x - 1) * 100) ((fromIntegral y - 1) * 100) $ renderMarker $ getMarker b (x, y)
               | x <- [0..2]
               , y <- [0..2] ]

currentPlayer :: Board -> Marker
currentPlayer b | even markCount = X
                | otherwise      = O
    where
        markCount = length $ filter (/=Blank) [getMarker b (x, y) | x <- [0..2], y <- [0..2]]

handleInput :: Event -> Board -> Board
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) b =
    if getMarker b (snap x, snap y) == Blank
    then M.insert (snap x, snap y) (currentPlayer b) b
    else b
    where
        snap = (+1) . max (-1) . min 1 . fromIntegral . floor . (/100) . (+50)
handleInput _ b = b

step :: Float -> Board -> Board
step _ b = b
