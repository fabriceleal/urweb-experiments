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

fun scanToEndVariation ls acc =
    case ls of
	[] => (acc, []) (* shouldnt happen *)
      | h :: t =>
	case h of
	    (raw, tag) =>
	    case tag of
		EndVariation => (h :: acc, t)
	      | _ => scanToEndVariation t (h :: acc)

and splitVariation (ls: list pgnGroup) (acc : list pgnGroup) : (list pgnGroup) * (list pgnGroup) =
    case ls of
	[] => (acc, []) (* shouldnt happen *)
      | h :: t =>
	(case h of
	     (raw, tag) =>
	     (case tag of 
		  StartVariation =>
		(* nest variation. include StartVariation and EndVariation tags *)
		  let
		      val (result, rest) = scanToEndVariation ls []
		  in
		      splitVariation rest (List.append result acc)
		  end
		  
		| EndVariation =>
		  (acc, t)
		| _ =>
		  splitVariation t (h :: acc))
	)

and handleComments (ls: list pgnGroup) : (list pgnGroup) * (list string) =
    case ls of
	[] => ([], [])
      | h :: t =>
	case h of
	    (raw, tag) =>
	    case tag of
		Comment =>
		let
		    val (rest, comments) = handleComments t
		in
		    (rest, raw :: comments)
		end
	      | _ =>
		(ls, [])
		
    
and handleVariations (state : gamestate) (ls: list pgnGroup) : (list pgnGroup) * (list pgnTree) =
    (* 
     this should handle variations. bail out if next token is not the start of a variation.
     *)
    case ls of 
	[] => ([], [])
      | h :: t =>   
	case h of
	    (raw, tag) =>
	    (case tag of
		 StartVariation => (* TODO keep handling variations until we hit a non startVariation token *)
		 let
		     val (elems, ls') = splitVariation t []
		     val (ls'', restVars) = handleVariations state ls'
		 in
		     (ls'', List.append (lsMovesToTree state (List.rev elems)) restVars)
		 end
	       | _ =>
		 (ls, []))
	    
and lsMovesToTree (state : gamestate) (ls : list pgnGroup) : list pgnTree =
    case ls of
	[] => []
      | h :: t =>
	(case h of
	     (raw, tag) =>
	     (case tag of
		  PawnMove =>
		  (case (pawnAlgebraicToMove state raw) of
		       None => []
		     | Some smove =>
		       (case doMove state smove of
			    None => []
			  | Some newState =>
			    let
				val newFen = state_to_fen newState
				val newMove = moveStr smove
				val newMoveAlg = moveToAlgebraicClean state smove newState
				val (t', cs) = handleComments t
				val (t'', brothers) = handleVariations state t'
			    in
				(Node (0, fen_to_state newFen, newMove, newMoveAlg, cs, (lsMovesToTree newState t''))) :: brothers
			    end))
		| Piece =>
		  (case (pieceAlgebraicToMove state raw) of
		       None => []
		     | Some smove =>
		       (case doMove state smove of
			    None => []
			  | Some newState =>
			    let
				val newFen = state_to_fen newState
				val newMove = moveStr smove
				val newMoveAlg = moveToAlgebraicClean state smove newState
				val (t', cs) = handleComments t
				val (t'', brothers) = handleVariations state t'
			    in
				(Node (0, fen_to_state newFen, newMove, newMoveAlg, cs, (lsMovesToTree newState t''))) :: brothers
			    end))
		| LongCastle =>
		  (case (castleAlgebraicToMove state raw) of
		       None => []
		     | Some smove =>
		       (case doMove state smove of
			    None => []
			  | Some newState =>
			    let
				val newFen = state_to_fen newState
				val newMove = moveStr smove
				val newMoveAlg = moveToAlgebraicClean state smove newState
				val (t', cs) = handleComments t
				val (t'', brothers) = handleVariations state t'
			    in
				(Node (0, fen_to_state newFen, newMove, newMoveAlg, cs, (lsMovesToTree newState t''))) :: brothers
			    end))
		| Castle =>
		  (case (castleAlgebraicToMove state raw) of
		       None => []
		     | Some smove =>
		       (case doMove state smove of
			    None => []
			  | Some newState =>
			    let
				val newFen = state_to_fen newState
				val newMove = moveStr smove
				val newMoveAlg = moveToAlgebraicClean state smove newState
				val (t', cs) = handleComments t
				val (t'', brothers) = handleVariations state t'
			    in
				(Node (0, fen_to_state newFen, newMove, newMoveAlg, cs, (lsMovesToTree newState t''))) :: brothers
			    end))
		| PieceDesamb =>			  
		  (case (pieceDesambAlgebraicToMove state raw) of
		       None => []
		     | Some smove =>
		       (case doMove state smove of
			    None => []
			  | Some newState =>
			    let
				val newFen = state_to_fen newState
				val newMove = moveStr smove
				val newMoveAlg = moveToAlgebraicClean state smove newState
				val (t', cs) = handleComments t
				val (t'', brothers) = handleVariations state t'
			    in
				(Node (0, fen_to_state newFen, newMove, newMoveAlg, cs, (lsMovesToTree newState t''))) :: brothers
			    end))
		| Promotion =>
		  (case (pawnAlgebraicToMove state raw) of
		       None => []
		     | Some smove =>
		       (case doMove state smove of
			    None => []
			  | Some newState =>
			    let
				val newFen = state_to_fen newState
				val newMove = moveStr smove
				val newMoveAlg = moveToAlgebraicClean state smove newState
				val (t', cs) = handleComments t
				val (t'', brothers) = handleVariations state t'
			    in				
				(Node (0, fen_to_state newFen, newMove, newMoveAlg, cs, (lsMovesToTree newState t''))) :: brothers
			    end))
		| MoveNbr => lsMovesToTree state t (* we can ignore these *)
		| Comment => lsMovesToTree state t (* these will be dropped *)
		| StartVariation => []
		| EndVariation => []
		| Result => []
		| HeaderKey => []
		| HeaderValue => []
	))

fun getK hdrs key =
    case hdrs of
	[] => None
      | h :: t =>
	case h of
	    (k, v) =>
	    if k = key then
		Some v
	    else
		getK t key

fun getCustomFen (hdrs : lsHeaders) : option string =
    setUp <- getK hdrs "SetUp";
    if setUp = "1" then
	fen <- getK hdrs "FEN";
	Some fen
    else
	None
		
fun getStateFromHdrs hdrs =
    case (getCustomFen hdrs) of
	None => fen_to_state startingFen
      | Some fen => fen_to_state fen

fun stringLToGame lines : pgnRoot =
    let
	val decomposed = decomposePgnL lines
	val (rest, hdrs) = readHeaders decomposed
	val state = getStateFromHdrs hdrs
	val moves = List.foldr List.append [] rest
    in	
	(Root (0, state, (lsMovesToTree state moves), hdrs))
    end

fun splitUntilHeaders full =
    case (String.ssplit {Haystack=full, Needle= "\n"}) of
	None =>
	(full :: [], "")
      | Some (h, rest) =>
	if (isHeader h) then
	    ([], full)
	else
	    case (splitUntilHeaders rest) of
		(ls, rest') => (h :: ls, rest')
			       
and split full =
    case (String.ssplit {Haystack=full, Needle= "\n"}) of
	None =>
	(full :: [], "")
      | Some (h, rest) =>
	if (isHeader h) then
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
		r :: []
	    else
		r :: (splitGames rest)
	end	

and pgnsToStrs full =
    case (split full) of
	(linesGame, rest) =>
	let
	    val r = linesGame
	in
	    if (strlen rest) = 0 then
		r :: []
	    else
		r :: (pgnsToStrs rest)
	end
	
fun pgnsToGames (pgn : string) : list pgnRoot =
(* we will start splitting lines. after we're sure we stopped reading headers, we'll read lines until we reach more headers *)
    splitGames pgn

fun pgnToGame (pgn : string) : pgnRoot =
    stringLToGame (Nregex.splitAllLines pgn)
