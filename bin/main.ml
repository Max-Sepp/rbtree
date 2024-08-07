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

let list_of_ints = [ 5; 3; 6; 7; 8; 3; 6; 7 ]

let () =
  let f elem =
    tree := insert elem !tree;
    print_json !tree;
    print_endline "";
    print_endline "--------------------------"
  in
  List.iter f list_of_ints
