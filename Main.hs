module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

type Radius = Float
type Position = (Float, Float)
data PlayerMovement = PlayerUp | PlayerStill | PlayerDown deriving Show
data Winner = Player1 | Player2 | NoOne

data PongGame = Game
    { ballLoc :: Position
    , ballDirection :: (Float, Float)
    , ballSpeed :: Float
    , player1 :: Position
    , player1Movement :: PlayerMovement
    , player2 :: Position
    , player2Movement :: PlayerMovement
    , paused :: Bool
    } deriving Show

width = 300
height = 300
offset = 100
fps = 60

playerSpeed = 4.0    
ballRadius = 10
paddleWidth = 13
paddleLength = 36
wallHeight = 10
wallWidth = 300

paddleYMax = 150 - paddleLength - wallHeight

initialPosition = (0.0, 0.0)
initialDirection = (1.5, 1.0)
initialSpeed = 60.0
speedIncrement = 10.0

ballColor = dark red

wallColor = greyN 0.5

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = play window background fps initialState draw handleKeys update

update :: Float -> PongGame -> PongGame
update seconds game = if (paused game)
                        then game
                        else ((moveBall seconds) . wallBounce . paddleBounce . movePlayers . handleBallOutOfBounds) game

initialState :: PongGame
initialState = Game initialPosition initialDirection initialSpeed
    (-120.0, 20.0) PlayerStill
    (120.0, 100.0) PlayerStill
    False

drawPaddle :: Color -> Position -> Picture
drawPaddle col pos = pictures
    [ translate x y $ color col $ rectangleSolid (2 * paddleWidth) (2 * paddleLength)]
    where (x, y) = pos

drawBall :: PongGame -> Picture
drawBall game = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius

    
drawWall :: Float -> Picture
drawWall offset =
    translate 0 offset $
    color wallColor $
        rectangleSolid wallWidth wallHeight

drawWalls :: Picture
drawWalls = pictures [drawWall 150, drawWall (-150)]

draw :: PongGame -> Picture
draw game = pictures [drawBall game, drawWalls, drawPaddle rose (player1 game), drawPaddle orange (player2 game)]

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
    where
    (x, y) = ballLoc game
    (vx, vy) = ballDirection game
    speed = ballSpeed game
    x' = x + vx * seconds * speed
    y' = y + vy * seconds * speed

checkWallCollision :: Position -> Bool
checkWallCollision (_, y) = topCollision || bottomCollision
    where
    topCollision = y - ballRadius <= -fromIntegral width / 2
    bottomCollision = y + ballRadius >= fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballDirection = (vx, vy') }
    where
    (vx, vy) = ballDirection game
    vy' = if checkWallCollision (ballLoc game) then -vy else vy


checkLeftPaddleCollision :: Position -> Position -> Bool
checkLeftPaddleCollision ballPosition player = xCollision && yCollision
    where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = (x - ballRadius <= px + paddleWidth) && (x - ballRadius >= px - paddleWidth)
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)

checkRightPaddleCollision :: Position -> Position -> Bool
checkRightPaddleCollision ballPosition player = xCollision && yCollision
    where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = (x + ballRadius >= px - paddleWidth) && (x + ballRadius <= px + paddleWidth)
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballDirection = (vx', vy), ballSpeed = speed' }
    where
    (vx, vy) = ballDirection game
    leftCollision = checkLeftPaddleCollision (ballLoc game) (player1 game)
    rightCollision = checkRightPaddleCollision (ballLoc game) (player2 game)
    vx' = if leftCollision then abs vx
            else if rightCollision then - (abs vx)
            else vx
    speed' = if leftCollision || rightCollision
                then (ballSpeed game) + speedIncrement
                else (ballSpeed game)

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

lockPaddleBounds :: Position -> Position
lockPaddleBounds (px, py) = (px, clamp (-paddleYMax) paddleYMax py)

movePlayer :: Position -> PlayerMovement -> Position
movePlayer (px, py) PlayerUp = (px, py + playerSpeed)
movePlayer (px, py) PlayerStill = (px, py)
movePlayer (px, py) PlayerDown = (px, py - playerSpeed)

movePlayers :: PongGame -> PongGame
movePlayers game = game { player1 = lockPaddleBounds $ movePlayer (player1 game) (player1Movement game)
                        , player2 = lockPaddleBounds $ movePlayer (player2 game) (player2Movement game)
                        }

winner :: Position -> Winner
winner (bx, _) | bx >= 150 = Player1
winner (bx, _) | bx <= (-150) = Player2
winner _ = NoOne

handleBallOutOfBounds :: PongGame -> PongGame
handleBallOutOfBounds game = case winner (ballLoc game) of
                                NoOne -> game
                                Player1 -> game { ballLoc = initialPosition, ballSpeed = initialSpeed }
                                Player2 -> game { ballLoc = initialPosition, ballSpeed = initialSpeed }


handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') Down _ _) game = game { ballLoc = initialPosition }
handleKeys (EventKey (Char 'p') Down _ _) game = game { paused = not (paused game) }

handleKeys (EventKey (Char 'w') Down _ _) game = game { player1Movement = PlayerUp }
handleKeys (EventKey (Char 'w') Up _ _) game = game { player1Movement = PlayerStill }
handleKeys (EventKey (Char 's') Down _ _) game = game { player1Movement = PlayerDown }
handleKeys (EventKey (Char 's') Up _ _) game = game { player1Movement = PlayerStill }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { player2Movement = PlayerUp }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { player2Movement = PlayerStill }
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { player2Movement = PlayerDown }
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { player2Movement = PlayerStill }
    
handleKeys _ game = game