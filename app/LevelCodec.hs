module LevelCodec
  ( saveLevel
  , loadLevel
  , tileId
  , tileFromId
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Data.Word (Word8, Word16, Word32)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Maybe (mapMaybe)

import Graphics.Gloss (Picture)
import Model ( GameState(..), World(..), Tile(..), TileMap
             , Player(..), Entity(..), Goomba(..), Koopa(..), ColliderSpec(..)
             )
import qualified Model as M
import qualified Collision as Collisions

-- Tile <-> id mapping (supports up to 16 tiles)
tileId :: Tile -> Word8
tileId Air           = 0
tileId Grass         = 1
tileId Crate         = 2
tileId QuestionBlock = 3

tileFromId :: Word8 -> Maybe Tile
tileFromId 0 = Just Air
tileFromId 1 = Just Grass
tileFromId 2 = Just Crate
tileFromId 3 = Just QuestionBlock
tileFromId _ = Nothing

-- Fixed-point for positions: store tile units * 256 in Int16
packPos :: Float -> Int16
packPos f = fromIntegral (round (f * 256))

unpackPos :: Int16 -> Float
unpackPos i = fromIntegral i / 256

-- Header
magic :: Word32
magic = 0x31564C46 -- "FLV1" little-endian marker for this format

version :: Word8
version = 1

-- Serialize a GameState to a binary level file
saveLevel :: FilePath -> GameState -> IO ()
saveLevel path gs = do
  let World { grid } = world gs
      width :: Int
      width = case grid of
        []     -> 0
        (r:_)  -> length r
      tiles1D = concat grid
      tileCount = length tiles1D
      px = fst (playerPos (player gs))
      py = snd (playerPos (player gs))
      pxi, pyi :: Int16
      pxi = packPos px
      pyi = packPos py
      -- Only save known enemy spawns we can reconstruct
      encodeSpawn (EGoomba Goomba { goombaPos = (x,y) }) = Just (1 :: Word8, packPos x, packPos y)
      encodeSpawn (EKoopa  Koopa  { koopaPos  = (x,y) }) = Just (2 :: Word8, packPos x, packPos y)
      encodeSpawn _ = Nothing
      spawns = mapMaybe encodeSpawn (entities gs)

      -- RLE encode tiles with 4-bit tile id and 4-bit run; air supports extended 16-bit run
      encodeRuns :: [Tile] -> [Word8]
      encodeRuns [] = []
      encodeRuns (t:ts) =
        let tid = tileId t
            (same, rest) = span (== t) ts
            runLen0 = 1 + length same
        in emit tid runLen0 ++ encodeRuns rest

      emit :: Word8 -> Int -> [Word8]
      emit tid len
        | len <= 0   = []
        | tid == 0 && len > 15 =
            -- Extended run for Air: marker byte with low nibble 0, then 16-bit little-endian run length chunk
            let chunk = min len 65535
                lo = fromIntegral (chunk .&. 0xFF) :: Word8
                hi = fromIntegral ((chunk `shiftR` 8) .&. 0xFF) :: Word8
            in ((tid `shiftL` 4) .|. 0) : lo : hi : emit tid (len - chunk)
        | otherwise =
            let n = fromIntegral (min len 15) :: Word8
            in ((tid `shiftL` 4) .|. n) : emit tid (len - fromIntegral n)

      runsBytes = encodeRuns tiles1D

      b :: BB.Builder
      b =  BB.word32LE magic
        <> BB.word8    version
        <> BB.word16LE (fromIntegral width)
        <> BB.word32LE (fromIntegral tileCount)
        <> BB.int16LE  pxi
        <> BB.int16LE  pyi
        <> BB.word16LE (fromIntegral (length spawns))
        <> mconcat [ BB.word8 ty <> BB.int16LE x <> BB.int16LE y | (ty,x,y) <- spawns ]
        <> mconcat (map BB.word8 runsBytes)

  BL.writeFile path (BB.toLazyByteString b)

-- Load a level and construct a GameState using supplied resources and config
loadLevel :: FilePath -> Bool -> TileMap -> Picture -> (Int, Int) -> IO GameState
loadLevel path debugEnabled tileMap playerSpriteImage screenDims = do
  bs <- BS.readFile path
  case parseLevel bs of
    Left err -> ioError (userError ("Level load error: " ++ err))
    Right (width, tiles1D, (px,py), spawns) -> do
      let height :: Int
          height = if width == 0 then 0 else length tiles1D `div` width
          -- Rebuild grid row-major
          grid' = [ take width (drop (y*width) tiles1D) | y <- [0..height - 1] ]
          worldState = World
            { grid = grid'
            , colliders = Collisions.generateCollidersForWorld grid'
            , slopes = []
            }
          initialPlayer = Player
            { playerPos    = (px, py)
            , playerVel    = (0, 0)
            , onGround     = False
            , health       = 1
            , playerSprite = [playerSpriteImage]
            , playerColliderSpec = Just ColliderSpec { colliderWidth = 0.6, colliderHeight = 0.75, colliderOffset = (0,0) }
            , playerJumpTime = 0
            , playerJumpDir  = (0,1)
            , playerSlide    = Nothing
            , playerAccelTime = 0
            , playerAccelDir  = 0
            , playerAccelSprint = False
            }
          toEntity (1, x, y) = Just (EGoomba Goomba { goombaPos=(x,y), goombaVel=(0,0), goombaDir=M.Left, goombaColliderSpec=Just ColliderSpec { colliderWidth=0.9, colliderHeight=0.9, colliderOffset=(0,0) }, goombaOnGround=False })
          toEntity (2, x, y) = Just (EKoopa  Koopa  { koopaPos=(x,y),  koopaVel=(0,0), koopaDir=M.Left, koopaColliderSpec=Just ColliderSpec { colliderWidth=0.9, colliderHeight=0.9, colliderOffset=(0,0) } })
          toEntity _         = Nothing
          entities' = mapMaybe toEntity spawns
      return GameState
        { world = worldState
        , player = initialPlayer
        , entities = entities'
        , tileZoom = 1.0
        , screenSize = screenDims
        , tileMap = tileMap
        , frameCount = 0
        , frameTime = 30
        , paused = False
        , debugMode = debugEnabled
        , pendingJump = False
        , jumpHeld = False
        , sprintHeld = False
        , moveLeftHeld = False
        , moveRightHeld = False
        }

-- Parser for the level format
parseLevel :: BS.ByteString -> Either String (Int, [Tile], (Float,Float), [(Word8, Float, Float)])
parseLevel bs = do
  let need n msg s = if BS.length s >= n then Right () else Left ("unexpected EOF reading " ++ msg)
      getWord8 s = Right (BS.head s, BS.tail s)
      getWord16LE s = do
        _ <- need 2 "word16" s
        let b0 = fromIntegral (BS.index s 0) :: Word16
            b1 = fromIntegral (BS.index s 1) :: Word16
        Right (b0 .|. (b1 `shiftL` 8), BS.drop 2 s)
      getWord32LE s = do
        _ <- need 4 "word32" s
        let b0 = fromIntegral (BS.index s 0) :: Word32
            b1 = fromIntegral (BS.index s 1) :: Word32
            b2 = fromIntegral (BS.index s 2) :: Word32
            b3 = fromIntegral (BS.index s 3) :: Word32
        Right ( b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
              , BS.drop 4 s)
      getInt16LE s = do
        (w, s') <- getWord16LE s
        let i = fromIntegral (fromIntegral w :: Int16)
        Right (i, s')

  -- header
  _ <- if BS.length bs >= 4 then Right () else Left "unexpected EOF reading magic"
  let mg =  fromIntegral (BS.index bs 0)
         .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
         .|. (fromIntegral (BS.index bs 2) `shiftL` 16)
         .|. (fromIntegral (BS.index bs 3) `shiftL` 24)
  if mg /= magic then Left "bad magic (not a level file)" else Right ()
  let rest0 = BS.drop 4 bs
  (ver, rest1) <- getWord8 rest0
  if ver /= version then Left "unsupported version" else Right ()
  (w16, rest2) <- getWord16LE rest1
  (tileCount32, rest3) <- getWord32LE rest2
  (pxi, rest4) <- getInt16LE rest3
  (pyi, rest5) <- getInt16LE rest4
  (enemyCount16, rest6) <- getWord16LE rest5
  let px = unpackPos pxi
      py = unpackPos pyi
      enemyCount = fromIntegral enemyCount16 :: Int
  (spawns, rest7) <- parseEnemies enemyCount rest6
  tiles <- parseTiles (fromIntegral tileCount32) rest7
  return (fromIntegral w16, tiles, (px,py), spawns)
  where
    parseEnemies :: Int -> BS.ByteString -> Either String ([(Word8, Float, Float)], BS.ByteString)
    parseEnemies 0 s = Right ([], s)
    parseEnemies n s = do
      let need n' msg s' = if BS.length s' >= n' then Right () else Left ("unexpected EOF reading " ++ msg)
      _ <- need 5 "enemy" s
      let ty = BS.index s 0
      let w0 = fromIntegral (BS.index s 1) :: Word16
      let w1 = fromIntegral (BS.index s 2) :: Word16
      let w2 = fromIntegral (BS.index s 3) :: Word16
      let w3 = fromIntegral (BS.index s 4) :: Word16
      let xi = fromIntegral (fromIntegral (w0 .|. (w1 `shiftL` 8)) :: Int16)
      let yi = fromIntegral (fromIntegral (w2 .|. (w3 `shiftL` 8)) :: Int16)
      let x = fromIntegral xi / 256
      let y = fromIntegral yi / 256
      (rest, s') <- Right ((), BS.drop 5 s)
      (more, s'') <- parseEnemies (n-1) s'
      Right ((ty, x, y) : more, s'')

    parseTiles :: Int -> BS.ByteString -> Either String [Tile]
    parseTiles total s = go total s []
      where
        go 0 s' acc = Right (reverse acc)
        go n s' acc = do
          if BS.null s' then Left "unexpected EOF in tile data" else Right ()
          let b = BS.head s'
              s'' = BS.tail s'
              tid = (b `shiftR` 4) .&. 0x0F
              lenNib = b .&. 0x0F
          if lenNib /= 0 then do
            let run = fromIntegral lenNib
            tiles <- replicateTiles tid run
            go (n - run) s'' (tiles ++ acc)
          else do
            -- extended for Air (tid==0) 16-bit length; for others, read 1 byte and add 16
            if tid == 0 then do
              if BS.length s'' < 2 then Left "unexpected EOF in extended air run" else Right ()
              let lo = fromIntegral (BS.index s'' 0) :: Int
                  hi = fromIntegral (BS.index s'' 1) :: Int
                  run = lo .|. (hi `shiftL` 8)
                  s3 = BS.drop 2 s''
              tiles <- replicateTiles tid run
              go (n - run) s3 (tiles ++ acc)
            else do
              if BS.null s'' then Left "unexpected EOF in extended run" else Right ()
              let add = fromIntegral (BS.head s'') :: Int
                  run = 16 + add
                  s3 = BS.tail s''
              tiles <- replicateTiles tid run
              go (n - run) s3 (tiles ++ acc)

        replicateTiles :: Word8 -> Int -> Either String [Tile]
        replicateTiles tid n
          | n < 0 = Left "negative run"
          | otherwise = case tileFromId tid of
              Nothing -> Left "unknown tile id"
              Just t  -> Right (replicate n t)
