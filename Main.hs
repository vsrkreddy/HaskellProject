{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.Text              as T
import           Graphics.Blank
import Control.Monad

data Color= Red|Blue deriving (Show,Eq) --datatype of color
--Datatype used to define result after each move
data Outcome = Lose | Win Color | Active | Invalid


--Used to represent the state of the board and the result of each move
data MoveResult = MoveResult Outcome Board

coordinates sz= [ (-sz * 0.4, -sz * 0.4), (0, -sz * 0.4), (sz * 0.4,-sz * 0.4), (-sz * 0.4,0), (0,0), (sz * 0.4,0), (-sz * 0.4, sz * 0.4), (0,sz * 0.4), (sz * 0.4,sz * 0.4)]



data Coin = Coin { idc:: Int, player:: Color, pos:: Maybe Int} deriving (Show,Eq)


type Board = [Coin]

--initial board six coins with positions as nothing which indicate the coins have not been used
initBoard::Board
initBoard = [Coin{idc=0, player= Red, pos= Nothing}, Coin{idc=1, player= Red, pos= Nothing}, Coin{idc=2, player= Red, pos= Nothing},Coin{idc=3, player= Blue, pos= Nothing},
             Coin{idc=4, player= Blue, pos= Nothing}, Coin{idc=5, player= Blue, pos= Nothing} ]


main::IO()
main = do
    putStrLn ""

    let b = initBoard
    startBoard <- newTVarIO b --shared memory for board
    currTurn <- newTVarIO Red --shared memory for current player turn
    startOutcome <- newTVarIO Active --shared memory for outcome of the play b/w viewer and controller
    prev <-newTVarIO Nothing -- drag detection
    blankCanvas 3000 {events = ["mousedown" ,"mouseup"]} $ \ context -> do
      forkIO $ viewer context startBoard startOutcome currTurn --to display the board after each update
      controller context startBoard startOutcome currTurn prev --logic for the game to update the board after each event
              --  circle 10 10 5 "Red"
-- viewer is meant to display the board after each cycle, view in model veiw controller(MVC)
viewer :: DeviceContext -> TVar Board -> TVar Outcome->TVar Color-> IO ()
viewer context board_var outcome_var turn_var= do
  board <- readTVarIO board_var --list of six coin
  outcome <- readTVarIO outcome_var -- outcome , updated outcome by controller
  currturn <- readTVarIO turn_var
  send context $ do -- send logic to browser to draw
          let (w,h) =(width context, height context)
          clearRect(0,0,w,h)
          beginPath()

          let sz = min w h
          save()
          draw $ "Player: " ++ show currturn
          translate (w/2 , h/2)
          sequence_ [ do bigLine (-sz * 0.4,n) (sz * 0.4,n)
                         bigLine (n,-sz * 0.4) (n,sz * 0.4)
                    | n <- [-sz * 0.4,sz * 0.4,0]
                    ]
          bigLine (-sz * 0.4,-sz * 0.4 ) (sz * 0.4,sz * 0.4)
          bigLine (sz * 0.4,-sz * 0.4 ) (-sz * 0.4,sz * 0.4)
          -- blueCircle (0,0,50)
          let positions = coordinates sz
          display board positions -- placement of coins at specific positions
          restore()

  case outcome of
       Lose ->send context $ do
          save()
          translate ( width context /2, height context /2)
          draw "You Lost!!"
          restore()
       Win c ->send context $ do
          save()
          translate ( width context / 2, height context / 2)
          draw $ "You Win!! " ++ show c
          restore()
       _ -> viewer context board_var outcome_var turn_var


decideWinner :: Board -> [(Int,Int,Int)] -> Maybe Color --win positions
decideWinner _ [] = Nothing
decideWinner b ((f,s,t):ys) = do
        fc <- coinAtPos (Just f) b
        sc <- coinAtPos (Just s) b
        if player fc == player sc
          then do
            tc <- coinAtPos (Just t) b
            if player fc == player tc
              then Just $ player fc
              else decideWinner b ys
          else decideWinner b ys


form x y z =(x,y,z)

winners :: [(Int,Int,Int)]
winners = [form 0 1 2, form 3 4 5, form 6 7 8,  form 0 4 8, form 2 4 6,  form 0 3 6, form 1 4 7, form 2 5 8]

valid :: [(Int,Int,Int)] --check for valid moves during drag
valid = [form 1 3 4, form 0 2 4, form 1 4 5, form 0 4 6, form 1 1 1, form 2 4 8, form 3 4 7, form 4 6 8, form 4 5 7]

getsize :: DeviceContext -> Double -- grid size
getsize context = do
  let (w,h) =(width context, height context)
  min w h

-- changing the turn of players
changeturn :: Color -> Color
changeturn Red = Blue
changeturn Blue = Red

-- logic for updating the board after each event
controller :: DeviceContext -> TVar Board ->TVar Outcome->TVar Color -> TVar (Maybe Int)-> IO ()
controller context board_var outcome_var turn_var prev_var= do
    board <- readTVarIO board_var
    turn <- readTVarIO turn_var
    prev <- readTVarIO prev_var
    --putStrLn $ show board
    event <- wait context --waiting for mouse events
    --let extract (Just x) = x
      --  extract Nothing = (0,0)
    -- let positions  =  coordinates (getsize context)
    -- let (xe,ye) = extract (ePageXY event)
    case (eType event,ePageXY event >>= \position-> findArea context position) of
      ("mousedown", i) -> do
        let coinAtclick = coinAtPos i board --i corresponds to 0-8 positions
        case coinAtclick of -- gets i th position coin if exits
                Nothing ->do
                  let newboard = assignCoin board turn i-- places a coin of a particular player if it is not used
                  atomically $ do -- postbox example
                      writeTVar board_var newboard
                      writeTVar outcome_var $ case decideWinner newboard winners of
                          Just c -> Win c
                          Nothing -> Active
                      when (newboard /= board) $ -- board changes if it was a valid event hence update the turn accordingly
                        writeTVar turn_var $ changeturn turn
                  controller context board_var outcome_var turn_var prev_var
                Just c -> if player c /= turn then controller context board_var outcome_var turn_var prev_var -- checks if players red clicks red circle
                    else do
                      atomically $ writeTVar prev_var (pos c)-- if same it remembers
                      controller context board_var outcome_var turn_var prev_var
      ("mouseup", j) ->  -- already clicked works for dragging
        case prev of
          Nothing -> controller context board_var outcome_var turn_var prev_var
          _ -> do
            let prevcoin = coinAtPos prev board
            let coinAtJ = coinAtPos j board -- j refers to dragging to jth position
            case coinAtJ of
              Nothing -> do
                unless (isvalid valid prev j) $ do --if false ot works (invalid is true)
                  atomically $ writeTVar prev_var Nothing
                  controller context board_var outcome_var turn_var prev_var
                let newboard = updatePosition prevcoin j board-- places at jth position and update the board
                atomically $ do
                    writeTVar board_var newboard
                    when (newboard /= board) $
                      writeTVar turn_var $ changeturn turn
                    -- writeTVar turn_var $ changeturn turn
                    writeTVar outcome_var $ case decideWinner newboard winners of
                        Just col -> Win col
                        Nothing -> Active
                controller context board_var outcome_var turn_var prev_var
              _ -> do
                atomically $ writeTVar prev_var Nothing
                controller context board_var outcome_var turn_var prev_var
        -- print $ length newboard
        -- print newboard
        -- let out = decideWinner newboard winners
        -- print out

        -- controller context board_var outcome_var turn_var
      _ -> controller context board_var outcome_var turn_var prev_var


ispresent :: (Int,Int,Int)->Int -> Bool
ispresent (a,b,c) d = a == d || b == d || c==d

isvalid :: [(Int,Int,Int)] -> Maybe Int -> Maybe Int -> Bool
isvalid _ (Just 4) _ = True-- for 4th position center position
isvalid mp (Just prev) (Just curr) = ispresent (mp !! prev) curr
isvalid _ _ _ = False


updatePosition :: Maybe Coin -> Maybe Int -> Board -> Board
updatePosition Nothing _ b = b
updatePosition (Just c) j b = if coinAtPos j b == Nothing then assign b c j else b
  where assign :: Board -> Coin -> Maybe Int -> Board
        assign [] _ _ = []
        assign (y:ys) cr ind = if idc y == idc cr then Coin { idc = idc cr, player = player cr , pos=ind} : ys else y: assign ys cr ind



assignCoin :: Board -> Color -> Maybe Int -> Board
assignCoin [] _ _ = []
assignCoin (x:xs) pla place = if player x == pla && pos x == Nothing then Coin { idc = idc x, player = pla , pos=place} : xs else x :  assignCoin xs pla place


coinAtPos :: Maybe Int -> Board -> Maybe Coin --returns the coin at a particular position
coinAtPos Nothing _ = Nothing
coinAtPos _ [] = Nothing
coinAtPos ji (x:xs) = if pos x == ji then Just x else coinAtPos ji xs


radius::Double --radius of coin/circle
radius = 50

findArea :: DeviceContext->(Double,Double)-> Maybe Int
findArea context (x,y) = do
    let size = getsize context
    let (w,h) =(width context, height context)
    let transX = x - (w/2)
    let transY = y - (h/2)
    let positions = coordinates size
    find positions (transX, transY) radius 0

find :: [(Double,Double)]->(Double,Double)->Double -> Int -> Maybe Int --finding position for the click
find positions (tx,ty) rad index  =  if index < length positions
                                          then do
                                            let x = positions !! index
                                            if tx >= (fst x - radius) && ty >= (snd x - radius) && tx <= (fst x + radius) && ty <= (snd x + radius)
                                              then return index
                                              else
                                                find positions (tx, ty) rad (index + 1)
                                          else
                                            Nothing



display:: [Coin]->[(Double,Double)]-> Canvas() -- displays the board
display [] _ = return()
display (x:xs) positions= do
    let p= pos x
    case p of
      Just n ->do
        circle (player x) (positions !! n) 50
        display xs positions
      Nothing -> display xs positions


--function for displaying which player should make a move.
draw :: String -> Canvas()
draw message = do
  font (T.pack (show 30 ++ "pt Calibri"))
  -- textAlign "center"
  -- textBaseline "middle"
  lineWidth 1
  fillStyle "black"
  fillText(T.pack message,50,50)

circle::Color -> (Double,Double)->Double->Canvas()   --draw circle based on color
circle  color (x,y) r = do

                  beginPath()
                  arc(x,y , r, 0, 2 * pi,False)
                  lineWidth 0

                  let c = case color of
                            Red->"red"
                            Blue->"blue"
                  strokeStyle c
                  fillStyle c
                  fill()
                  stroke()

bigLine :: (Double, Double) -> (Double, Double) -> Canvas () -- used to draw the box structure
bigLine (x,y) (x',y') = do
                          beginPath()
                          moveTo(x,y)
                          lineTo(x',y')
                          lineWidth 15
                          strokeStyle "black"
                          lineCap "round"
                          stroke()
