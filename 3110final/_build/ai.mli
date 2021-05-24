(** A very dumb computer player that enables 1 player mode.

    This module represents the computer player of Xiangqi game. It
    enables 1 player mode. The very dumb ai randomly picks a move that
    is Legal at its turn, all else act the same as human player. *)

open State
open Piece
open Board

type piece = Piece.t

(** [available_piece] is a function that returns the list of all pieces
    on board that is of the computer's side *)
val available_piece : Board.t -> Piece.t list

(** [choose_piece] randomly picks a piece out of all available pieces of
    the computer side. We suppose the computer is always at the [Black]
    side for now *)
val choose_piece : Board.t -> Piece.t list -> Piece.t

(** [get_coordinate] takes in a coordinate and randomly generate a
    destiny coordinate of the piece on that coordinate according to its
    features following game rules*)
val get_coordinate : Piece.t -> Piece.coord * (int * int)

(** [make_command] takes the current state of the game and randomly
    generate a legal move for the computer side, converting the move
    into a string that is identical to user input*)
val make_command : State.t -> string -> string
