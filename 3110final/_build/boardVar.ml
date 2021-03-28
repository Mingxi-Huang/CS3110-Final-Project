open Piece

let rank_array = [| Rook; Horse; Elephant; Advisor |]

let rev_rank_array = [| Advisor; Elephant; Horse; Rook |]

let bottom_row side =
  let general =
    create_piece General side ((if side = Red then 9 else 0), 4)
  in
  let row = Array.make 9 None in
  for i = 0 to 3 do
    let piece =
      create_piece rank_array.(i) side ((if side = Red then 9 else 0), i)
    in
    row.(i) <- Some piece
  done;
  row.(4) <- Some general;
  for i = 5 to 8 do
    let piece =
      create_piece
        rev_rank_array.(i - 5)
        side
        ((if side = Red then 9 else 0), i)
    in
    row.(i) <- Some piece
  done;
  row

let soldier_row side =
  let row = Array.make 9 None in
  for i = 0 to 8 do
    if i mod 2 = 0 then
      row.(i) <-
        Some
          (create_piece Soldier side ((if side = Red then 6 else 3), i))
  done;
  row

let cannon_row side =
  let row = Array.make 9 None in
  let cannon1 =
    create_piece Cannon side ((if side = Red then 7 else 2), 1)
  in
  let cannon2 =
    create_piece Cannon side ((if side = Red then 7 else 2), 7)
  in
  row.(1) <- Some cannon1;
  row.(7) <- Some cannon2;
  row
