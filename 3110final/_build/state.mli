(** Representation of dynamic state in Xiangqi.

    This module represents the state of a Xiangqi game as it is being
    played, including the player's current coordinates, and functions
    that cause the state to change. *)
open Board

open Piece

type t

type result =
  | Legal of t
  | Illegal

(** [get_current_board t] is the current board configuration for state
    [t]*)
val get_current_board : t -> Board.t

(** [get current turn t] is the turn for state [t]*)
val get_current_turn : t -> Piece.side

(** [init_state] is the side that starts the game. We initializes it to
    be the [Red] side*)
val init_state : t

(** [create_state board turn] create a state with board config [board]
    and current turn [side]*)
val create_state : Board.t -> Piece.side -> t

(** [go] evaluated if the input movement is legal. *)
val move : int * int -> int * int -> t -> result
