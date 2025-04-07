open System

type Tree<'a> =
    | Item of 'a
    | Section of Tree<'a> list

type Path<'a> =
    | Top
    | Node of Tree<'a> list * Path<'a> * Tree<'a> list

type Loc<'a> = Loc of Tree<'a> * Path<'a>

let go_left (Loc(current_element_of_tree, path)) =
    match path with
    | Top -> failwith "Left of top"
    | Node(head :: left, p', right) -> Loc(head, Node(left, p', current_element_of_tree :: right))
    | _ -> failwith "Left of first"

let go_right (Loc(current_element_of_tree, path)) =
    match path with
    | Top -> failwith "Right of top"
    | Node(left, p', r :: right) -> Loc(r, Node(current_element_of_tree :: left, p', right))
    | _ -> failwith "Right of last"


let go_up (Loc(current_element_of_tree, path)) =
    match path with
    | Top -> failwith "Up of top"
    | Node(left, p', right) -> Loc(Section(List.rev left @ [ current_element_of_tree ] @ right), p')

let go_down (Loc(tree, path)) =
    match tree with
    | Item(_) -> failwith "Down of item"
    | Section(first :: rest) -> Loc(first, Node([], path, rest))
    | _ -> failwith "Down of empty section"

let rec nth n loc =
    match n with
    | 0 -> go_down loc
    | n when n > 0 -> nth (n - 1) loc |> go_right
    | _ -> failwith "Negative index"

let change t (Loc(_, p)) = Loc(t, p)

let insert_right r (Loc(t, p)) =
    match p with
    | Top -> failwith "Insert right of top"
    | Node(left, p', right) -> Loc(t, Node(left, p', r :: right))

let insert_left l (Loc(t, p)) =
    match p with
    | Top -> failwith "Insert left of top"
    | Node(left, p', right) -> Loc(t, Node(l :: left, p', right))

let insert_down t1 (Loc(t, p)) =
    match t with
    | Item _ -> failwith "Insert down of item"
    | Section children -> Loc(t1, Node([], p, children))

let delete (Loc(_, p)) =
    match p with
    | Top -> failwith "Delete top"
    | Node(left, p', r :: right) -> Loc(r, Node(left, p', right))
    | Node(l :: left, p', []) -> Loc(l, Node(left, p', []))
    | Node([], p', []) -> Loc(Section [], p')



let t =
    Section[Section[Item "a"
                    Item "+"
                    Item "b"]]

let loc = Loc(t, Top)

printfn "Initial tree: %A" loc
