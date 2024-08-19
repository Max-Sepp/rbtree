type node_color = Red | Black | Double_Black

type 'a rbtree =
  | Leaf of node_color
  | Node of {
      value : 'a;
      color : node_color;
      left : 'a rbtree;
      right : 'a rbtree;
    }

let balance = function
  | Node
      {
        value = z;
        color = Black;
        left =
          Node
            {
              value = y;
              color = Red;
              left = Node { color = Red; value = x; left = a; right = b };
              right = c;
            };
        right = d;
      }
  | Node
      {
        value = z;
        color = Black;
        left =
          Node
            {
              value = x;
              color = Red;
              left = a;
              right = Node { color = Red; value = y; left = b; right = c };
            };
        right = d;
      }
  | Node
      {
        value = x;
        color = Black;
        left = a;
        right =
          Node
            {
              value = z;
              color = Red;
              left = Node { value = y; color = Red; left = b; right = c };
              right = d;
            };
      }
  | Node
      {
        value = x;
        color = Black;
        left = a;
        right =
          Node
            {
              value = y;
              color = Red;
              left = b;
              right = Node { value = z; color = Red; left = c; right = d };
            };
      } ->
      Node
        {
          color = Red;
          value = y;
          left = Node { color = Black; value = x; left = a; right = b };
          right = Node { color = Black; value = z; left = c; right = d };
        }
  | Node
      {
        value = z;
        color = Double_Black;
        left =
          Node
            {
              value = x;
              color = Red;
              left = a;
              right = Node { value = y; color = Red; left = b; right = c };
            };
        right = d;
      }
  | Node
      {
        value = x;
        color = Double_Black;
        left = a;
        right =
          Node
            {
              value = z;
              color = Red;
              left = Node { value = y; color = Red; left = b; right = c };
              right = d;
            };
      } ->
      Node
        {
          value = y;
          color = Black;
          left = Node { value = x; color = Black; left = a; right = b };
          right = Node { value = z; color = Black; left = c; right = d };
        }
  | Node { value; color; left; right } -> Node { value; color; left; right }
  | Leaf Black -> Leaf Black
  | _ -> failwith "Invalid tree entered"

let insert insert_value tree =
  let rec insert_helper = function
    | Leaf Black ->
        Node
          {
            value = insert_value;
            color = Red;
            left = Leaf Black;
            right = Leaf Black;
          }
    | Node { value; color; left; right } as n ->
        if insert_value < value then
          balance (Node { value; color; left = insert_helper left; right })
        else if insert_value > value then
          balance (Node { value; color; left; right = insert_helper right })
        else n
    | _ -> failwith "Invalid tree entered"
  in
  match insert_helper tree with
  | Node { value; color = _; left; right } ->
      Node { value; color = Black; left; right }
  | Leaf _ -> failwith "Helper function returned invalid value"

let rec print_json = function
  (* Case 1 *)
  | Node x ->
      print_string "{\"value\": ";
      print_int x.value;
      if x.color = Black then print_string ", \"color\": \"Black\", \"left\": "
      else if x.color = Red then print_string ", \"color\": \"Red\", \"left\": "
      else print_string ", \"color\": \"Double Black\", \"left\": ";
      print_json x.left;
      print_string ", \"right\": ";
      print_json x.right;
      print_string "}"
  | Leaf _ -> print_string "\"Leaf\" "

let rotate = function
  | Node
      {
        color = Red;
        value = y;
        left = Node { color = Double_Black; value = x; left = a; right = b };
        right = Node { color = Black; value = z; left = c; right = d };
      } ->
      balance
        (Node
           {
             color = Black;
             value = z;
             left =
               Node
                 {
                   color = Red;
                   value = y;
                   left = Node { color = Black; value = x; left = a; right = b };
                   right = c;
                 };
             right = d;
           })
  | Node
      {
        color = Red;
        value = y;
        left = Leaf Double_Black;
        right = Node { color = Black; value = z; left = c; right = d };
      } ->
      balance
        (Node
           {
             color = Black;
             value = z;
             left =
               Node { color = Red; value = y; left = Leaf Black; right = c };
             right = d;
           })
  | Node
      {
        color = Red;
        value = y;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Node { color = Double_Black; value = z; left = c; right = d };
      } ->
      balance
        (Node
           {
             color = Black;
             value = x;
             left = a;
             right =
               Node
                 {
                   color = Red;
                   value = y;
                   left = b;
                   right =
                     Node { color = Black; value = z; left = c; right = d };
                 };
           })
  | Node
      {
        color = Red;
        value = y;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Leaf Double_Black;
      } ->
      balance
        (Node
           {
             color = Black;
             value = x;
             left = a;
             right =
               Node { color = Red; value = y; left = b; right = Leaf Black };
           })
  (* Case 2 *)
  | Node
      {
        color = Black;
        value = y;
        left = Node { color = Double_Black; value = x; left = a; right = b };
        right = Node { color = Black; value = z; left = c; right = d };
      } ->
      balance
        (Node
           {
             value = z;
             color = Double_Black;
             left =
               Node
                 {
                   value = y;
                   color = Red;
                   left = Node { value = x; color = Black; left = a; right = b };
                   right = c;
                 };
             right = d;
           })
  | Node
      {
        color = Black;
        value = y;
        left = Leaf Double_Black;
        right = Node { color = Black; value = z; left = c; right = d };
      } ->
      balance
        (Node
           {
             value = z;
             color = Double_Black;
             left =
               Node { value = y; color = Red; left = Leaf Black; right = c };
             right = d;
           })
  | Node
      {
        color = Black;
        value = y;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Node { color = Double_Black; value = z; left = c; right = d };
      } ->
      balance
        (Node
           {
             value = x;
             color = Double_Black;
             left = a;
             right =
               Node
                 {
                   value = y;
                   color = Red;
                   left = b;
                   right =
                     Node { value = z; color = Black; left = c; right = d };
                 };
           })
  | Node
      {
        color = Black;
        value = y;
        left = Node { color = Black; value = x; left = a; right = b };
        right = Leaf Double_Black;
      } ->
      balance
        (Node
           {
             value = x;
             color = Double_Black;
             left = a;
             right =
               Node { value = y; color = Red; left = b; right = Leaf Black };
           })
      (* Case 3 *)
  | Node
      {
        value = x;
        color = Black;
        left = Node { value = w; color = Double_Black; left = a; right = b };
        right =
          Node
            {
              value = z;
              color = Red;
              left = Node { value = y; color = Red; left = c; right = d };
              right = e;
            };
      } ->
      Node
        {
          value = z;
          color = Black;
          left =
            balance
              (Node
                 {
                   value = y;
                   color = Black;
                   left =
                     Node
                       {
                         value = x;
                         color = Red;
                         left =
                           Node
                             { value = w; color = Black; left = a; right = b };
                         right = c;
                       };
                   right = d;
                 });
          right = e;
        }
  | Node
      {
        value = x;
        color = Black;
        left = Leaf Double_Black;
        right =
          Node
            {
              value = z;
              color = Red;
              left = Node { value = y; color = Red; left = c; right = d };
              right = e;
            };
      } ->
      Node
        {
          value = z;
          color = Black;
          left =
            balance
              (Node
                 {
                   value = y;
                   color = Black;
                   left =
                     Node
                       { value = x; color = Red; left = Leaf Black; right = c };
                   right = d;
                 });
          right = e;
        }
  | Node
      {
        value = y;
        color = Black;
        left =
          Node
            {
              value = w;
              color = Red;
              left = a;
              right = Node { value = x; color = Black; left = b; right = c };
            };
        right = Node { value = z; color = Double_Black; left = d; right = e };
      } ->
      Node
        {
          value = w;
          color = Black;
          left = a;
          right =
            balance
              (Node
                 {
                   value = x;
                   color = Black;
                   left = b;
                   right =
                     Node
                       {
                         value = y;
                         color = Red;
                         left = c;
                         right =
                           Node
                             { value = z; color = Black; left = d; right = e };
                       };
                 });
        }
  | Node
      {
        value = y;
        color = Black;
        left =
          Node
            {
              value = w;
              color = Red;
              left = a;
              right = Node { value = x; color = Black; left = b; right = c };
            };
        right = Leaf Double_Black;
      } ->
      Node
        {
          value = w;
          color = Black;
          left = a;
          right =
            balance
              (Node
                 {
                   value = x;
                   color = Black;
                   left = b;
                   right =
                     Node
                       { value = y; color = Red; left = c; right = Leaf Black };
                 });
        }
  | Node x -> Node x
  | Leaf x -> Leaf x

let rec delete_inorder_successor = function
  | Node { value; color = Black; left = Leaf Black; right = Leaf Black } ->
      (value, Leaf Double_Black)
  (* On this pattern match right should always be a Leaf *)
  | Node { value; color = Red; left = Leaf Black; right = Leaf Black } ->
      (value, Leaf Black)
  | Node
      {
        value;
        color = Black;
        left = Leaf Black;
        right =
          Node
            {
              color = Red;
              value = right_child_value;
              left = right_child_left;
              right = right_child_right;
            };
      } ->
      ( value,
        Node
          {
            value = right_child_value;
            color = Black;
            left = right_child_left;
            right = right_child_right;
          } )
  | Node
      {
        value;
        color = Black;
        left = Leaf Black;
        right =
          Node
            {
              color = Black;
              value = right_child_value;
              left = right_child_left;
              right = right_child_right;
            };
      } ->
      ( value,
        Node
          {
            value = right_child_value;
            color = Double_Black;
            left = right_child_left;
            right = right_child_right;
          } )
  | Node { value; color; left = left_node; right } ->
      let inorder_successor, left = delete_inorder_successor left_node in
      (inorder_successor, rotate (Node { value; color; left; right }))
  | Leaf _ -> failwith "cannot input a leaf node into the function"

let delete_with_no_right = function
  | Node { color = Red; right = Leaf Black; _ } -> Leaf Black
  | Node { color = Black; left = Leaf Black; right = Leaf Black; value = _ } ->
      Leaf Double_Black
  | Node
      {
        color = Black;
        left = Node { color = Red; value; left; right };
        right = Leaf Black;
        value = _;
      } ->
      Node { value; left; right; color = Black }
  | _ -> failwith "Invalid input entered"

let delete target tree =
  let rec delete_helper = function
    | Node { value; color; left; right } as n ->
        if target < value then
          rotate
            (Node { value; color; left = rotate (delete_helper left); right })
        else if target > value then
          rotate
            (Node { value; color; left; right = rotate (delete_helper right) })
        else if right = Leaf Black then rotate (delete_with_no_right n)
        else
          let inorder_successor, corrected_right =
            delete_inorder_successor right
          in
          rotate
            (Node
               {
                 value = inorder_successor;
                 color;
                 left;
                 right = corrected_right;
               })
    | Leaf _ -> failwith "cannot find node to be deleted"
  in
  match rotate (delete_helper tree) with
  | Node { value; color = _; left; right } ->
      Node { value; color = Black; left; right }
  (* Todo: check if this gives desired outcome *)
  | Leaf _ -> Leaf Black
