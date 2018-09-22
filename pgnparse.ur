open Nregexpgn
open Chess

type pgnGroup = string * pgnTag
type lsGroups = list pgnGroup
     
fun containsHTags (g : list pgnGroup) : bool =
    case g of
	[] => True
      | h :: t =>
	(case h of
	     (_, tag) => (case tag of
			       HeaderKey => True
			     | HeaderValue => True
			     | _ => containsHTags t))

fun pgnToGame (pgn : string) : pgnRoot =
    let
	val decomposed = decomposePgn pgn

	fun bypassHeaders (ls : list lsGroups) : list lsGroups =
	    case ls of
		[] => []
	      | h :: t =>
		if (containsHTags h) then
		    bypassHeaders t
		else
		    ls

	fun lsMovesToTree (ls : list pgnGroup) : option pgnTree =
	    case ls of
		[] => None
	      | h :: t =>
		(case h of
		     (raw, tag) =>
		     (case tag of
			  PawnMove =>
			  let
			      val move = ""
			      val pos = ""
			      val alg = ""
			  in
			      Some (Node (0, move, pos, alg, []))
			  end
			| MoveNbr => lsMovesToTree t
			| _ => None))
    in
	
	(Root (0, "", []))
    end
    
