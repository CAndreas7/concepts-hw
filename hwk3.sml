fun rowToString(row: int list) : string =
  let
    fun helper([], tmp: string) = tmp
    | helper(h::t, tmp) = helper(t, tmp ^ (Int.toString(h) ^ " "))
  in
    helper(row, "")
  end;

fun matrixToString(matrix: int list list) : string =
  let
    fun helper([], tmp: string) = tmp
    | helper(h::t, tmp) = helper(t, tmp ^ (rowToString h) ^ "\n")
  in
    "\n" ^ helper(matrix, "")
  end;

fun item(list: 'a list, i: int) : 'a =
  let
    fun helper([], _, _) = raise Domain
    | helper(h::t, 1, tmp) = h
    | helper(h::t, i, tmp) = helper(t, i-1, tmp)
  in
    helper(list, i, false)
  end;

fun column(matrix: 'a list list, i: int) : 'a list =
  let
    fun helper([], tmp: 'a list) = tmp
    | helper(h::t, tmp) = helper(t, tmp @ [item(h, i)])
  in
    helper(matrix, [])
  end;

val x = [[1,2,3],[4,5,6],[7,8,9]];

print (matrixToString x);
print (rowToString (item (x, 2)));