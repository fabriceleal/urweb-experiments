open Nregex
open Nregexpgn
open Chess

type pgnGroup = string * pgnTag
type lsGroups = list pgnGroup
     
fun containsHTags (g : list pgnGroup) : bool =
    case g of
	[] => False
      | h :: t =>
	(case h of
	     (_, tag) => (case tag of
			       HeaderKey => True
			     | HeaderValue => True
			     | _ => containsHTags t))

fun bypassHeaders (ls : list lsGroups) : list lsGroups =
    case ls of
	[] => []
      | h :: t =>
	if (containsHTags h) then
	    bypassHeaders t
	else
	    ls

fun test (pgn : string) : list pgnGroup =
    List.foldr List.append [] (bypassHeaders (decomposePgn pgn))

fun pgnToGame (pgn : string) : pgnRoot =
    let
	val decomposed = decomposePgn pgn
				  
	fun lsMovesToTree state (ls : list pgnGroup) : option pgnTree =
	    case ls of
		[] => None
	      | h :: t =>
		(case h of
		     (raw, tag) =>
		     (case tag of
			  PawnMove =>
			  (case (pawnAlgebraicToMove state raw) of
			       None =>
			       None
			     | Some smove =>
			       (case doMove state smove of
				    None => None
				  | Some newState =>
				    let
					val newFen = state_to_fen newState
					val newMove = moveStr smove
					val newMoveAlg = moveToAlgebraicClean state smove newState
				    in
					Some (Node (0, newFen, newMove, newMoveAlg, (optToList (lsMovesToTree newState t))))
				    end))
			| Piece =>
			  (case (pieceAlgebraicToMove state raw) of
			       None =>
			       None
			     | Some smove =>
			       (case doMove state smove of
				    None => None
				  | Some newState =>
				    let
					val newFen = state_to_fen newState
					val newMove = moveStr smove
					val newMoveAlg = moveToAlgebraicClean state smove newState
				    in
					Some (Node (0, newFen, newMove, newMoveAlg, (optToList (lsMovesToTree newState t))))
				    end))
			| Castle =>
			  (case (castleAlgebraicToMove state raw) of
			       None =>
			       None
			     | Some smove =>
			       (case doMove state smove of
				    None => None
				  | Some newState =>
				    let
					val newFen = state_to_fen newState
					val newMove = moveStr smove
					val newMoveAlg = moveToAlgebraicClean state smove newState
				    in
					Some (Node (0, newFen, newMove, newMoveAlg, (optToList (lsMovesToTree newState t))))
				    end))
			| PieceDesamb =>			  
			  (case (pieceDesambAlgebraicToMove state raw) of
			       None =>
			       None
			     | Some smove =>
			       (case doMove state smove of
				    None => None
				  | Some newState =>
				    let
					val newFen = state_to_fen newState
					val newMove = moveStr smove
					val newMoveAlg = moveToAlgebraicClean state smove newState
				    in
					Some (Node (0, newFen, newMove, newMoveAlg, (optToList (lsMovesToTree newState t))))
				    end))
			| MoveNbr => lsMovesToTree state t
			| Comment => lsMovesToTree state t
			| _ => None))
		
	val state = fen_to_state startingFen
	val moves = List.foldr List.append [] (bypassHeaders decomposed)
    in	
	(Root (0, state_to_fen state, (optToList (lsMovesToTree state moves))))
    end
    
