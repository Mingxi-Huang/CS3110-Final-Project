(** Representation of dynamic state in Xiangqi.

    This module represents the state of a Xiangqi game as it is being
    played, including the player's current coordinates, and functions
    that cause the state to change. *)
open Board

open Piece

type t

type step

type result =
  | Legal of t
  | Illegal

(**[check_win t] is whether one side has win the game*)
val check_winner : t -> Piece.side option

(** [get_current_board t] is the current board configuration for state
    [t]*)
val get_current_board : t -> Board.t

(** [get current turn t] is the turn for state [t]*)
val get_current_turn : t -> Piece.side

val get_current_grave : t -> Board.graveyard

val get_current_score : t -> Board.score

val get_current_step : t -> step

(** [init_state] is the side that starts the game. We initializes it to
    be the [Red] side*)
val init_state : t

(** [move] evaluated if the input movement is legal. *)
val move : int * int -> int * int -> t -> result

val non_empty_coord : Piece.t option -> bool

val occupied_coord : Board.t -> int * int -> bool

(** [rules] is a bool, judging whether a move of piece [p] to
    destination coordinate [c] is legal, currently for player's red
    side, without eating pieces, before acrossing the river. *)
val rules : Piece.t -> Piece.coord -> t -> bool
