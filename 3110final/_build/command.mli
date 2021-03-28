(* *--->---> Footprint: Init pieces and init states to be done

   (**The type [piece_phrase] represents the piece phrase that can be
   part of a player command.*)

   (* type select_phrase = string

   type mov_phrase = string

   type piece_phrase = { select_phrase : select_phrase; mov_phrase :
   mov_phrase; }

   type command = | Go of piece_phrase | Quit

   exception Empty

   exception Malformed

   val parse : string -> command *) open State open Piece

   (**Returns the selected piece given the user input message*) val
   selected_piece : string -> piece list -> piece

   (**Returns the destination state given the user input message*) val
   destination_state : string -> state list -> state

   (**Update the piece information by updating the label of the piece*)
   val update_piece : piece -> state -> piece

   (**Update the state label information by updating the occupied field
   of it*) val update_state : state -> bool -> state

   val update_plist : piece list -> piece -> state -> piece list

   val update_slist : state list -> piece -> state -> state list *)
