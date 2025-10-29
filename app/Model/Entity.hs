module Model.Entity where

import Model.Types
import qualified Model.Types as Types
import Data.List
import Model.Config (maxJumps)

getEntity :: GameState -> Int -> Maybe Entity
getEntity GameState {entities = es} eId = 
  find (\case
    EGoomba         gId _ -> gId  == eId
    EKoopa          kId _ -> kId  == eId
    EPowerup        pId _ -> pId  == eId
    EPlatform mpId  -> mpId == eId
    ) es

-- Spawns a new entity and updates global id counter
spawnEntity :: GameState -> Entity -> GameState
spawnEntity gs@GameState { entities = es, entityIdCounter = eIdCounter } e =
  let 
    e'  = setId e eIdCounter
    es' = e':es
  in gs { entities = es', entityIdCounter = eIdCounter + 1 }

-- Removes an entity from the entities list given an entity id.
-- Does not fail if no enemy with the given id was found
killEntity :: GameState -> Int -> GameState
killEntity gs@GameState { entities } eId = 
  gs { entities = filter (\e -> getEntityId e /= eId) entities
  }

setId :: Entity -> Int -> Entity
setId (EGoomba  _ g)  newId = EGoomba  newId g
setId (EKoopa   _ k)  newId = EKoopa   newId k
setId (EPowerup _ pu) newId = EPowerup newId pu
setId (EPlatform _  ) newId = EPlatform newId

getEntityType :: Entity -> EntityType
getEntityType (EGoomba   _ _) = TGoomba
getEntityType (EKoopa    _ _) = TKoopa
getEntityType (EPowerup  _ _) = TPowerup
getEntityType (EPlatform _  ) = TPlatform

defaultPlayer :: Player
defaultPlayer = Player
  { playerPos         = (1, 0)
  , playerVel         = (0, 0)
  , onGround          = False
  , health            = 1
  -- default player animation is unset, doing otherwise would require impurity. Just remember to always set playerAnim when spawning
  , playerAnim        = [] 
  , playerColSpec     = Just ColliderSpec
    { colliderWidth   = 0.85
    , colliderHeight  = 1.1
    , colliderOffset  = (0, 0)
    }
  , playerJumpTime    = 0
  , playerJumpDir     = (0, 1)
  , playerSlide       = Nothing
  , playerAccelTime   = 0
  , playerAccelDir    = 0
  , playerAccelSprint = False
  , playerCollisions  = []
  , moveLeftHeld      = False
  , moveRightHeld     = False
  , lastMoveDir       = 0
  , jumpsLeft         = maxJumps
  , stompJumpTimeLeft = 0
  }

-- Helper function for changing player's health
setPlayerHealth :: GameState -> Int -> GameState
setPlayerHealth gs@GameState{ player = p } h = 
  gs 
  { player = p { health = h }
  , nextState = if h <= 0 then NDeath else NPlaying 
  }

-- Shorthands for common health changes
damagePlayer :: GameState -> GameState
damagePlayer gs@GameState{ player } = setPlayerHealth gs $ health player - 1
healPlayer :: GameState -> GameState
healPlayer gs@GameState{ player } = setPlayerHealth gs $ health player + 1


defaultGoomba :: Goomba
defaultGoomba = Goomba 
  { goombaPos = (0, 0)
  , goombaVel = (0, 0)
  , goombaDir = Types.Left
  , goombaColSpec = Just ColliderSpec
      { colliderWidth = 0.9
      , colliderHeight = 0.6
      , colliderOffset = (0, -0.25)
      }
  , goombaOnGround = False
  , goombaCollisions = []
  }

defaultPowerup :: Powerup
defaultPowerup = Powerup
  { powerupPos = (0, 0)
  , powerupVel = (0, 0)
  , powerupDir = Types.Left
  , powerupColSpec = Just ColliderSpec
      { colliderWidth = 0.9
      , colliderHeight = 0.9
      , colliderOffset = (0, 0)
      }
  , powerupCollisions = []
  }
