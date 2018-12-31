
type topic = int

val inj : sql_injectable topic = _

type postType = int

val inj2 : sql_injectable postType = _

val ptChess : postType = 0
val ptWeiqi : postType = 1
val ptShogi : postType = 2
val ptFreeText : postType = 3

fun strToTyp (s : string) : topic =
    case (read s : option int) of
	Some 0 => ptChess
      | Some 1 => ptWeiqi
      | _ => ptFreeText
			      
fun optStrToTyp (s: option string) : topic = 
    case s of
	None => ptFreeText
      | Some s' => strToTyp s'
