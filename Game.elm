module Game (Game, Tile, createGame, revealTile) where

import List
import Random
import Array


type alias Tile =
  {id: Int
  ,threatCount: Maybe Int
  ,isRevealed: Bool
  ,isMine: Bool}


type alias Game =
  {isDead: Bool
  ,isSafe: Bool
  ,rows: Int
  ,cols: Int
  ,tiles: List Tile}


type Direction = W | NW | N | NE | E | SE | S | SW



tileByIdx : Game -> Int -> Maybe Tile
tileByIdx game idx =
  if idx >= 0 then
    List.drop idx game.tiles |> List.head
  else
    Nothing


onWEdge : Game -> Tile -> Bool
onWEdge game tile =
  (tile.id % game.cols) == 0


onEEdge : Game -> Tile -> Bool
onEEdge game tile =
  (tile.id % game.cols) == game.cols - 1


neighbourByDir : Game -> Maybe Tile -> Direction -> Maybe Tile
neighbourByDir game tile dir =
  let
    tIdx = tileByIdx game
  in
    case (tile, dir) of
      (Just t, W)  -> if not <| onWEdge game t then tIdx <| t.id - 1 else Nothing
      (Just t, NW) -> if not <| onWEdge game t then tIdx <| t.id - game.cols - 1 else Nothing
      (Just t, N)  -> tIdx <| t.id - game.cols
      (Just t, NE) -> if not <| onEEdge game t then tIdx <| t.id - game.cols + 1 else Nothing
      (Just t, E)  -> if not <| onEEdge game t then tIdx <| t.id + 1 else Nothing
      (Just t, SE) -> if not <| onEEdge game t then tIdx <| t.id + game.cols + 1 else Nothing
      (Just t, S)  -> tIdx <| t.id + game.cols
      (Just t, SW) -> if not <| onWEdge game t then tIdx <| t.id + game.cols - 1 else Nothing
      (Nothing, _) -> tile


neighbours : Game -> Maybe Tile -> List Tile
neighbours game tile =
  let
    n = neighbourByDir game tile
  in
    List.filterMap identity <| List.map n [W, NW, N, NE, E, SE, S, SW]


mineCount : Game -> Maybe Tile -> Int
mineCount game tile =
  List.length <| List.filter (\t -> t.isMine) <| neighbours game tile


updateIn : Int -> (a -> a) -> List a -> List a
updateIn  idx f items =
  let
    ts = Array.fromList items
    t = Array.get idx ts
  in
    case t of
      Just v -> Array.toList <| Array.set idx (f v) ts
      Nothing -> items


revealMines : Game -> Game
revealMines game =
  {game | tiles  <- List.map (\t -> {t | isRevealed <- t.isRevealed || t.isMine}) game.tiles
        , isDead <- True}


revealThreatCount : Game -> Tile -> Tile
revealThreatCount game tile =
  {tile | threatCount <- Just (mineCount game <| Just tile)
        , isRevealed  <- True}


revealAdjacentSafeTiles :  Game -> Int -> Game
revealAdjacentSafeTiles game tileId =
  case tileByIdx game tileId of
    Nothing -> game
    Just t ->
      if t.isMine then game else
        let
          updT = revealThreatCount game t
          updG = {game | tiles <- updateIn tileId (\_ -> updT) game.tiles}
          f    = (\t g -> if not t.isRevealed then revealAdjacentSafeTiles g t.id else g)
        in
          if not (updT.threatCount == Just 0) then
            updG
          else
            List.foldl f updG <| neighbours updG <| Just updT


isSafe : Game -> Bool
isSafe game =
  (List.filter (\t -> t.isMine && t.isRevealed) game.tiles |> List.length) == 0
    && (List.filter (\t -> (not t.isMine) && (not t.isRevealed)) game.tiles |> List.length) == 0


attemptWinning : Game -> Game
attemptWinning game =
  {game | isSafe <- isSafe game }
--  if isSafe game then {game | isSafe <- True} else game



revealTile : Game -> Int -> Game
revealTile game tileId =
  let
    t = tileByIdx game tileId
  in
    case t of
      Nothing -> game
      Just v ->
        if v.isMine then
          revealMines game
        else
          attemptWinning <| revealAdjacentSafeTiles game tileId


randBools : Int -> Int -> List Bool
randBools count seedVal =
    let
      intList = Random.generate (Random.list count <| Random.int 0 100) (Random.initialSeed seedVal) |> fst
    in
      List.map (\n -> (n % 2) == 1 ) intList


createGame : Int -> Int -> Int -> Game
createGame cols rows seedVal =
  let
    tiles = List.indexedMap (\i isMine -> Tile i Nothing False isMine ) (randBools (rows*cols) seedVal)
  in
    Game False False rows cols tiles

-- testing
{- let
  g = (createGame 3 3 123545)
  --gUpd = {g | tiles <-  updateIn 1 (\t -> {t | isMine <- False}) g.tiles}
  gUpd = {g | tiles <- List.map (\t ->  if t.isMine then t else {t | isRevealed <- True}) g.tiles}
in
  --gUpd
  attemptWinning g -}
  --revealTile gUpd 0




