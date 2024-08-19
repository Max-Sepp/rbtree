open Rbtree

let tree =
  ref (Node { value = 5; color = Black; left = Leaf Black; right = Leaf Black })

(* let () =
   while true do
     let value_to_insert = read_int () in
     tree := insert value_to_insert !tree;
     print_json !tree;
     print_endline ""
   done *)

let insert_list = [ 7; 6; 4; 5; 1; 9; 2; 3; 8 ]
let delete_list = [ 7; 2; 8; 1; 6; 9; 5; 3; 4 ]

let () =
  let f elem =
    tree := insert elem !tree;
    print_json !tree;
    print_endline "";
    print_endline "--------------------------"
  in
  List.iter f insert_list;
  let f elem =
    tree := delete elem !tree;
    print_json !tree;
    print_endline "";
    print_endline "--------------------------"
  in
  List.iter f delete_list
