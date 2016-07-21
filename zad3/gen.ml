type hcellPos = HLeft | HTop;;

let hcell hmode = Printf.sprintf "hcell_%s_%d_%d" 
    (match hmode with
        | HLeft -> "left"
        | HTop -> "top"
    );;

let cell = Printf.sprintf "cell_%d_%d";;

let go n = 
begin
print_endline
"MODULE main
FROZENVAR";
Printf.printf "\tmatrix : array 1..%d of array 1..%d of boolean;\n" n n;
print_endline "VAR";

for row = 1 to n do
    for col = 1 to 2 * n - 1 do
        let input = if col = 1 then "FALSE" else hcell HLeft row (col - 1) ^ ".out"
        and initial = match col - (n - row) with
            | i when 0 < i && i <= n -> Printf.sprintf "matrix[%d][%d]" row (n - i + 1)
            | _ -> "FALSE"
        in  Printf.printf "\t%s : hcell(%s, %s);\n" (hcell HLeft row col) input initial
    done
done;

for row = 1 to 2 * n - 1 do
    for col = 1 to n do
        let input = if row = 1 then "FALSE" else hcell HTop (row - 1) col ^ ".out"
        and initial = match row - (n - col) with
            | i when 0 < i && i <= n -> Printf.sprintf "matrix[%d][%d]" (n - i + 1) col
            | _ -> "FALSE"
        in  Printf.printf "\t%s : hcell(%s, %s);\n" (hcell HTop row col) input initial
    done
done;

for row = 1 to n do
    for col = 1 to n do
        let left = if col = 1 then hcell HLeft row (2 * n - 1) ^ ".out" else cell row (col - 1) ^ ".right"
        and top = if row = 1 then hcell HTop (2 * n - 1) col ^ ".out" else cell (row - 1) col ^ ".down"
        in Printf.printf "\t%s : cell(%s, %s);\n" (cell row col) left top
    done
done;

Printf.printf "\titer : 0..%d;\n" (4 * n);

print_endline "ASSIGN";
print_endline "\tinit(iter) := 0;";
Printf.printf "\tnext(iter) := (iter + 1) mod (4 * %d);\n" n;


print_endline "DEFINE";
Printf.printf "\tend := iter = %d;\n" (3 * n);

print_string
"\tnumones := count(FALSE";
for row = 1 to n do
    for col = 1 to n do
        Printf.printf ", matrix[%d][%d]" row col
    done
done;
print_endline ");";

print_string
"\tidentity := TRUE";
for row = 1 to n do
    for col = 1 to n do
        Printf.printf " & %s%s.accum" (if row = col then "" else "!") (cell row col)
    done
done;
print_endline ";";

Printf.printf "SPEC numones > %d -> AG (end -> !identity);\n" n;

print_endline
"MODULE cell(left, up)
VAR
\taccum : boolean;
\tright : boolean;
\tdown : boolean;
ASSIGN
\tinit(accum) := FALSE;
\tinit(right) := FALSE;
\tinit(down) := FALSE;

\tnext(accum) := accum xor (left & up);
\tnext(right) := left;
\tnext(down) := up;
";

print_endline
"MODULE hcell(input, initial)
VAR
\tout : boolean;
ASSIGN
\tinit(out) := initial;
\tnext(out) := input;
";
end;;

Scanf.scanf "%d\n" go;;
