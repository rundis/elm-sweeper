module Minesweeper where


import Game exposing (Game, GameStatus(..))
import Utils
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple
import Time
import Task
import Debug


type Action =
  NewGame
  | RevealTile Int



init : Game
init =
  Game.createGame 15 15 5787345


update : (Float, Action) -> Game -> Game
update (time, action) game =
  case action of
    NewGame -> Game.createGame 15 15 (truncate time)
    RevealTile id -> if not (game.status == IN_PROGRESS) then game else
                        Debug.watch "game" (Game.revealTile game id)



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



statusView: Game -> Html
statusView game =
  let
    (status, c) = case game.status of
                    SAFE          -> (" -  You won", "status-won")
                    DEAD          -> (" - You lost", "status-lost")
                    IN_PROGRESS   -> ("", "")
  in
    span [class c] [text status]


view : Signal.Address Action -> Game -> Html
view address game =
  let
    rows = Utils.partitionByN game.cols game.tiles
  in
    div [id "main"] [
      h1 [] [text "Minesweeper", statusView game],
      div [class "board"] (List.map (rowView address) rows),
      div [] [button [class "button", onClick address NewGame] [text "Reset"]]
    ]


{- main : Signal Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }
 -}

-- Without start-app
actions: Signal.Mailbox Action
actions =
  Signal.mailbox NewGame

model: Signal Game
model =
  Signal.foldp update init (Time.timestamp actions.signal)

main : Signal Html
main =
  Signal.map (view actions.address) model

port initGame : Task.Task x ()
port initGame =
  Signal.send actions.address NewGame
