
type pgnGroup = string * Nregexpgn.pgnTag
type lsGroups = list pgnGroup

val pgnToGame : string -> Chess.pgnRoot

val test : string -> lsGroups
		     
