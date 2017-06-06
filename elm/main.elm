module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Util exposing (pairs)



main : Program Never Model Msg
main =
    Html.program
        { init = resetBoard createModel
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- TODO: express this intensionally
blackPositions =
  [ (1, 1), (3, 1), (5, 1), (7, 1)
  , (2, 2), (4, 2), (6, 2), (8, 2)
  , (1, 3), (3, 3), (5, 3), (7, 3)
  , (2, 4), (4, 4), (6, 4), (8, 4)
  , (1, 5), (3, 5), (5, 5), (7, 5)
  , (2, 6), (4, 6), (6, 6), (8, 6)
  , (1, 7), (3, 7), (5, 7), (7, 7)
  , (2, 8), (4, 8), (6, 8), (8, 8)
  ]



-- TYPES

type GameState =
    ResetGame
  | StartingGame
  | StartedGame
  | EndedGame


type Msg =
  SelectCell Cell


type alias Model =
  { gameState: GameState
  , cells: List Cell
  , playerToMove: Player
  , allowedMoves: Moves
  }


type CellState =
    EmptyCell
  | BlackCell
  | WhiteCell


type alias Cell =
  { state: CellState
  , row: Int
  , col: Int
  , selected: Bool
  }


type Player =
    BlackPlayer
  | WhitePlayer


type alias Moves = List Jump

type alias Jump = List (Int, Int)


-- MISC

gameStateToString : GameState -> String
gameStateToString gs =
  case gs of
    ResetGame -> "reset"
    StartingGame -> "starting"
    StartedGame -> "started"
    EndedGame -> "ended"


cellPosToString : Cell -> String
cellPosToString cell =
  "(" ++ (toString cell.row) ++ "," ++ (toString cell.col) ++ ")"


cellStateToString : Cell -> String
cellStateToString cell =
  case cell.state of
    EmptyCell -> "empty"
    BlackCell -> "black"
    WhiteCell -> "white"


playerToString : Player -> String
playerToString player =
  case player of
    BlackPlayer -> "Black"
    WhitePlayer -> "White"


getSelectedCell : Model -> Maybe Cell
getSelectedCell model =
  List.head (List.filter (\cell -> cell.selected) model.cells)


maybeCellToString : Maybe Cell -> String
maybeCellToString maybeCell =
  case maybeCell of
    Nothing -> "N/A"
    Just cell -> (cellPosToString cell)


updateSelection : List Cell -> Cell -> List Cell
updateSelection cells currentCell =
  List.map (\cell -> if cell == currentCell then {cell | selected = True} else {cell | selected = False}) cells


getAllowedMoves : Model -> Moves
getAllowedMoves model =
  let
    player = model.playerToMove
    selectedCell = getSelectedCell model
  in
    case selectedCell of
      Nothing -> [[(1, 1), (4, 4), (8, 8)]]
      Just cell -> [[(1,2)]]


isMoveAllowed : Cell -> Model -> Bool
isMoveAllowed cell model =
  let
    allowedMoves = model.allowedMoves
  in
    ( List.member (cell.row, cell.col) model.allowedMoves )


resetBoard : (Model, Cmd Msg) -> (Model, Cmd Msg)
resetBoard (model, msg) =
  ( { gameState = ResetGame
    , cells = resetCells model.cells
    , playerToMove = BlackPlayer
    , allowedMoves = getAllowedMoves model
    }, msg )


resetCells : List Cell -> List Cell
resetCells cells =
  --List.map (\cell -> if cell.row == cell.col then {cell | state = BlackCell} else cell) cells
  List.map (\cell -> if List.member (cell.row, cell.col) blackPositions then {cell | state = BlackCell} else {cell | state = WhiteCell }) cells



-- MODEL


createModel : (Model, Cmd Msg )
createModel =
    ( { gameState = ResetGame
      , cells = List.map (\(r, c) -> createCell r c) (pairs 1 8)
      , playerToMove = BlackPlayer
      , allowedMoves = [[]]
      }, Cmd.none )


createCell : Int -> Int -> Cell
createCell row col =
  Cell EmptyCell row col False



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectCell cell ->
      ({ model | cells = updateSelection model.cells cell
       }, Cmd.none)



-- VIEW

createPiece : Cell -> Bool -> Html Msg
createPiece cell allowed =
    div [ classList [ ("piece", True)
                    , ("selected", cell.selected)
                    , ("allowed", allowed)
                    , (cellStateToString cell, True)
                    ]
        , (title (cellPosToString cell))
        , onClick (SelectCell cell)
        ] []


createPiecesAndMoves : Model -> List (Html Msg)
createPiecesAndMoves model =
  List.map (\cell -> createPiece cell (isMoveAllowed cell model) ) model.cells


view : Model -> Html Msg
view model =
    div [ (id "board") ]
        [ div [id "info"]
          [ div [] [ text ("Game state: " ++ (gameStateToString model.gameState)) ]
          , div [] [ text ("Selected cell: " ++ (maybeCellToString (getSelectedCell model))) ]
          , div [] [ text ("Player to move: " ++ (playerToString model.playerToMove)) ]
          , div [] [ text ("Allowed moves: " ++ (toString model.allowedMoves)) ]
          ]
        , div [id "pieces"] (createPiecesAndMoves model) ]
