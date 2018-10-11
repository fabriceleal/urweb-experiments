
type pgnGroup = string * Nregexpgn.pgnTag
type lsGroups = list pgnGroup

val pgnToGame : string -> Chess.pgnRoot

val test : string -> lsGroups
		     
val pgnsToGames : string -> list Chess.pgnRoot
val pgnsToStrs : string -> list (list string)
			   
val split : string -> (list string) * string

val stringLToGame: list string -> Chess.pgnRoot
				 
