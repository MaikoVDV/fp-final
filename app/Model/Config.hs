module Model.Config where

assetTilePixelSize :: Float
assetTilePixelSize = 18

scaleFactor :: Float
scaleFactor = 0.5

jumpImpulse :: Float
jumpImpulse = 8

jumpHoldAccelStart :: Float
jumpHoldAccelStart = 44

jumpHoldDuration :: Float
jumpHoldDuration = 0.5

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
groundWalkAccel = 40
groundSprintAccel = 80
groundMoveDecel = 125
airWalkAccel = 10
airSprintAccel = 17
airMoveDecel = 28

goombaWalkSpeed, goombaMoveAccel :: Float
goombaWalkSpeed = 3
goombaMoveAccel = 20

slideProbeDistance :: Float
slideProbeDistance = 0.02

wallProbeDistance :: Float
wallProbeDistance = 0.05