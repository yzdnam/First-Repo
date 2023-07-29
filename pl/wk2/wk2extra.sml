type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail;

(* 1. takes a final_grade and returns pass if the garde field contains SOME i for an i >= 75 (else fail). *)
fun pass_or_fail({ id = sid, grade = s_grade }) =
    case s_grade of
	NONE => fail
      | SOME i => if i >= 75
		  then pass
		  else fail

(* 2. returns true if and only if the grade field contains SOME i for an i >= 75 *)

							   
