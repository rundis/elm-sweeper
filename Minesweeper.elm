module Minesweeper where


import Game exposing (Game)
import Utils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple
import Debug


type Action =
  RevealTile Int


init : Game
init =
  Game.createGame 15 10 123545


update : Action -> Game -> Game
update action game =
  case action of
    RevealTile id -> Debug.watch "game" (Game.revealTile game id)


threatCount : Maybe Int -> List Html
threatCount count =
  case count of
    Nothing -> []
    Just t  -> [text (if t > 0 then toString t else "")]


tileView : Signal.Address Action -> Game.Tile -> Html
tileView address tile =
  if tile.isRevealed then
    div [class ("tile" ++ (if tile.isMine then " mine" else ""))]
        <| threatCount tile.threatCount

  else
    div [class "tile", onClick address (RevealTile tile.id)]
        [div [class "lid"] []]


rowView : Signal.Address Action -> List Game.Tile -> Html
rowView address tiles =
  div [class "row"] (List.map (tileView address) tiles)


view : Signal.Address Action -> Game -> Html
view address game =
  let
    rows = Utils.partitionByN game.cols game.tiles
  in
    div [id "main"] [
      h1 [] [text "Minesweeper"],
      div [class "board"] (List.map (rowView address) rows)
    ]


main : Signal Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }
