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

fun matrixAdd (X : int list list, Y : int list list) =
    let
      fun helper (x : int list, y : int list) =
        if (null x) then
          []
        else
          (hd x + hd y) :: (helper (tl x, tl y))
    in
      if (null X) then
        []
      else
        (helper (hd X, hd Y)) :: (matrixAdd (tl X, tl Y))
    end;

fun transpose(matrix: 'a list list) : 'a list list =
  let
    fun helper(matrix: 'a list list, i: int, tmp: 'a list list) =
      if i > List.length (hd matrix) then
        tmp
      else
        helper(matrix, i+1, tmp @ [column(matrix, i)])
  in
    helper(matrix, 1, [])
  end;

fun dotProduct (x: int list, y: int list): int =
  if (List.length x <> List.length y) then 
	raise Fail "not equal length"
  else let
    fun helper (x: int list, y: int list, acc: int): int =
      case (x, y) of
        ([], []) => acc
        | (x::xs, y::ys) => helper (xs, ys, acc + x*y)
        | _ => raise Fail "not equal length"
  in
    helper (x, y, 0)
  end;

fun scalarMatrixProduct(scalar: int, matrix: int list list): int list list =
  List.map (List.map (fn x => x * scalar)) matrix;

fun matrixProduct (X : int list list, Y : int list list) : int list list = 
  let
    fun helper (x : int list, y : int list list) =
      if (null y) then
        []
      else
        (dotProduct (x, hd y)) :: (helper (x, tl y))
    in
      if (null X) then
        []
      else
        (helper (hd X, transpose Y)) :: (matrixProduct (tl X, Y))
    end;

val show = print o matrixToString;
val x = [[1,2,3], [4,5,6], [7,8,9]];
val y = [[1,1,1],[2,2,2],[3,3,3]];
show (matrixAdd(x,y));
dotProduct ([1,2,3],[4,5,6]);
show (transpose x);
show (scalarMatrixProduct (10, x));
show (matrixProduct (x, y));