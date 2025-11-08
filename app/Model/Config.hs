module Model.Config where

assetTilePixelSize :: Float
assetTilePixelSize = 18

scaleFactor :: Float
scaleFactor = 0.5

jumpImpulse :: Float
jumpImpulse = 8

jumpHoldAccelStart :: Float
jumpHoldAccelStart = 50

jumpHoldDuration :: Float
jumpHoldDuration = 0.56

-- For how many game frames a given sprite in an animation should last
-- a lower frametime means animations run quicker
frameTime :: Int
frameTime = 30

-- Zoom bounds for tile scaling
minTileZoom, maxTileZoom :: Float
minTileZoom = 0.5
maxTileZoom = 2.0

zoomStep :: Float
zoomStep = 0.1

gravityAcceleration :: Float
gravityAcceleration = -35

groundFrictionCoeff, airFrictionCoeff :: Float
groundFrictionCoeff = 12
airFrictionCoeff = 1.5

groundWalkAccel, groundSprintAccel, groundMoveDecel, airWalkAccel, airSprintAccel, airMoveDecel :: Float
groundWalkAccel = 50
groundSprintAccel = 100
groundMoveDecel = 125
airWalkAccel = 9
airSprintAccel = 16
airMoveDecel = 28

goombaWalkSpeed, goombaMoveAccel :: Float
goombaWalkSpeed = 3
goombaMoveAccel = 20

slideProbeDistance :: Float
slideProbeDistance = 0.02

wallProbeDistance :: Float
wallProbeDistance = 0.05

-- Maximum number of jumps before needing to touch the ground
maxJumps :: Int
maxJumps = 2

-- Vertical velocity applied to player when stomping an enemy
stompBounceVelocity :: Float
stompBounceVelocity = 8

-- Time window after stomping in which a jump gets boosted
stompJumpWindow :: Float
stompJumpWindow = 0.3

-- Jump impulse strength when jumping within stompJumpWindow after a stomp
stompBoostedJumpImpulse :: Float
stompBoostedJumpImpulse = 10

-- Health and lives configuration
maxHealth :: Int
maxHealth = 6

defaultLives :: Int
defaultLives = 3

-- Damage dealt by a Goomba on contact (non-stomp)
goombaContactDamage :: Int
goombaContactDamage = 3

-- Player invulnerability (after getting hit)
invulnDuration :: Float
invulnDuration = 1.5

flickerInterval :: Float
flickerInterval = 0.25

-- Time a goomba stays in shell before reviving (seconds)
goombaShellDuration :: Float
goombaShellDuration = 10
