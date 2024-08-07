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
  (* | Node
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
           } *)
  | Node { value; color; left; right } -> Node { value; color; left; right }
  | Leaf Black -> Leaf Black
  | _ -> failwith "Invalid tree entered"

let insert insert_value tree =
  let rec insert_helper insert_value = function
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
          balance
            (Node
               { value; color; left = insert_helper insert_value left; right })
        else if insert_value > value then
          balance
            (Node
               { value; color; left; right = insert_helper insert_value right })
        else n
    | _ -> failwith "Invalid tree entered"
  in
  match insert_helper insert_value tree with
  | Node { value; color = _; left; right } ->
      Node { value; color = Black; left; right }
  | Leaf _ -> failwith "Helper function returned invalid value"

let rec print_json = function
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
  | Leaf _ -> print_string "Leaf\" "
