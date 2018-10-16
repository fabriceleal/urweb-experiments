open Nregex
open Nregexpgn
open Chess

type pgnGroup = string * pgnTag
type lsGroups = list pgnGroup
type lsHeaders = list (string * string)

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


fun readValueOfLine (l : lsGroups) : option string =
    case l of
	[] => None
      | h :: _ =>
	case h of
	    (value,  tag) => (case tag of
				  HeaderValue => Some value
				| _ => None )
	    
fun readHeaderOfLine (l : lsGroups) : option (string * string) =
    case l of
	[] => None
      | h :: t =>
	(case h of
	     (key, tag) => (case tag of				
				HeaderKey =>
				let
				    val v = readValueOfLine t
				in
				    case v of
					None => None
				      | Some v' => Some (key, v')
				end
			      | _ => None))
	
fun readHeaders (ls : list lsGroups) : (list lsGroups) * lsHeaders =
    case ls of
	[] => ([], [])
      | h :: t =>
	case (readHeaderOfLine h) of
	    None => (ls, [])
	  | Some hdr =>
	    let
		val (ls', moreHdrs) = readHeaders t
	    in
		(ls', hdr :: moreHdrs)
	    end


fun test (pgn : string) : list pgnGroup =
    List.foldr List.append [] (bypassHeaders (decomposePgn pgn))

fun stringLToGame lines : pgnRoot =
    let
	val decomposed = decomposePgnL lines
				  
	fun lsMovesToTree state (ls : list pgnGroup) : option pgnTree =
	    case ls of
		[] => None
	      | h :: t =>
		(case h of
		     (raw, tag) =>
		     (case tag of
			  PawnMove =>
			  (case (pawnAlgebraicToMove state raw) of
			       None => None
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
			       None => None
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
			| LongCastle =>
			  (case (castleAlgebraicToMove state raw) of
			       None => None
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
			       None => None
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
			       None => None
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
			| Promotion =>
			  (case (pawnAlgebraicToMove state raw) of
			       None => None
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
			| StartVariation =>
			  (* moves until variation *)
			  None
			| _ => None))
		
	val state = fen_to_state startingFen
	val (rest, hdrs) = readHeaders decomposed
	val moves = List.foldr List.append [] rest
    in	
	(Root (0, state_to_fen state, (optToList (lsMovesToTree state moves)), hdrs))
    end

fun pgnsToStrs (pgn : string) : list (list string) =
(* we will start splitting lines. after we're sure we stopped reading headers, we'll read lines until we reach more headers *)
    let
	fun splitUntilHeaders full =
	    case (String.ssplit {Haystack=full, Needle= "\n"}) of
		None =>
		(full :: [], "")
	      | Some (h, rest) =>
		if (Nregex.startsWith h "[") then (* FIXME for some reason, a line may start with [ but it may be inside a comment *)
		    ([], full)
		else
		    case (splitUntilHeaders rest) of
			(ls, rest') => (h :: ls, rest')
	    
	and split full =
	    case (String.ssplit {Haystack=full, Needle= "\n"}) of
		None =>
		(full :: [], "")
	      | Some (h, rest) =>
		if (Nregex.startsWith h "[") then (* FIXME for some reason, a line may start with [ but it may be inside a comment *)
		    (case (split rest) of
			 (ls, rest') => (h :: ls, rest'))
		else 
		    (case (splitUntilHeaders rest) of
			 (ls, rest') => (h :: ls, rest'))
	    
	and splitGames (full : string) : list (list string) =
	    case (split full) of
		(linesGame, rest) =>
		let
		    val r = linesGame
		in
		    if (strlen rest) = 0 then
			r :: []
		    else
			r :: (splitGames rest)
		end		
	    
    in
	splitGames pgn
    end

fun splitUntilHeaders full =
    case (String.ssplit {Haystack=full, Needle= "\n"}) of
	None =>
	(full :: [], "")
      | Some (h, rest) =>
	if (Nregex.startsWith h "[Event") then
	    ([], full)
	else
	    case (splitUntilHeaders rest) of
		(ls, rest') => (h :: ls, rest')
			       
and split full =
    case (String.ssplit {Haystack=full, Needle= "\n"}) of
	None =>
	(full :: [], "")
      | Some (h, rest) =>
	if (Nregex.startsWith h "[") then
	    (case (split rest) of
		 (ls, rest') => (h :: ls, rest'))
	else 
	    (case (splitUntilHeaders rest) of
		 (ls, rest') => (h :: ls, rest'))
	    
and splitGames full =
    case (split full) of
	(linesGame, rest) =>
	let
	    val r = stringLToGame linesGame
	in
	    if (strlen rest) = 0 then
		[]
	    else
		r :: (splitGames rest)
	end	
    
fun pgnsToGames (pgn : string) : list pgnRoot =
(* we will start splitting lines. after we're sure we stopped reading headers, we'll read lines until we reach more headers *)
    splitGames pgn

fun pgnToGame (pgn : string) : pgnRoot =
    stringLToGame (Nregex.splitAllLines pgn)
