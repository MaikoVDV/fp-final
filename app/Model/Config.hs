module Model.Config where

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