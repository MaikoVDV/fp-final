module Model.Entity where

import Model.Types
import qualified Model.Types as Types
import Data.List
import Model.Config (maxJumps, maxHealth, invulnDuration)

getEntity :: GameState -> Int -> Maybe Entity
getEntity GameState {entities = es} eId = 
  find (\case
    EGoomba         gId _ -> gId  == eId
    EKoopa          kId _ -> kId  == eId
    EPowerup        pId _ -> pId  == eId
    ECoin           cId _ -> cId  == eId
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

-- Update a Goomba by entity id using a transformation function
updateGoombaById :: GameState -> Int -> (Goomba -> Goomba) -> GameState
updateGoombaById gs@GameState { entities } targetId f =
  let upd e = case e of
        EGoomba eid g | eid == targetId -> EGoomba eid (f g)
        _ -> e
  in gs { entities = map upd entities }

setId :: Entity -> Int -> Entity
setId (EGoomba  _ g)  newId = EGoomba  newId g
setId (EKoopa   _ k)  newId = EKoopa   newId k
setId (EPowerup _ pu) newId = EPowerup newId pu
setId (ECoin    _ c)  newId = ECoin    newId c
setId (EPlatform _  ) newId = EPlatform newId

getEntityType :: Entity -> EntityType
getEntityType (EGoomba   _ _) = TGoomba
getEntityType (EKoopa    _ _) = TKoopa
getEntityType (EPowerup  _ _) = TPowerup
getEntityType (ECoin     _ _) = TCoin
getEntityType (EPlatform _  ) = TPlatform

defaultPlayer :: Player
defaultPlayer = Player
  { playerPos         = (1, 0)
  , playerVel         = (0, 0)
  , onGround          = False
  , health            = maxHealth
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
  , playerAnimClock   = 0
  , invulnTimeLeft    = 0
  }

-- Helper function for changing player's health
setPlayerHealth :: GameState -> Int -> GameState
setPlayerHealth gs@GameState{ player = p } h = 
  let h' = max 0 h
  in if h' <= 0
      then -- lose a life and trigger death (leave level to world map handler)
        let livesLeft = max 0 (playerLives gs - 1)
        in gs { player = p { health = 0 }
              , playerLives = livesLeft
              , nextState = NDeath }
      else gs { player = p { health = min maxHealth h' }, nextState = NPlaying }

-- Shorthands for common health changes
damagePlayer :: GameState -> GameState
damagePlayer gs@GameState{ player = _p } = damagePlayerN 1 gs

damagePlayerN :: Int -> GameState -> GameState
damagePlayerN n gs@GameState{ player = p0 } =
  let n' = max 0 n
  in if invulnTimeLeft p0 > 0
        then gs
        else
          let gs' = setPlayerHealth gs (health p0 - n')
          in gs' { player = (Types.player gs') { invulnTimeLeft = invulnDuration } }

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
  , goombaMode = GWalking
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

defaultCoin :: Coin
defaultCoin = Coin
  { coinPos = (0, 0)
  , coinColSpec = Just ColliderSpec
      { colliderWidth = 0.6
      , colliderHeight = 0.6
      , colliderOffset = (0, 0)
      }
  }
