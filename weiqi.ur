
datatype player = White | Black

val playerEq : eq player = mkEq (fn a b =>
				    case (a, b) of
					(White, White) => True
				      | (Black, Black) => True
				      | (_, _) => False)

type piecerec = { Piece: player, X: int, Y: int }

val plShow = mkShow (fn p => case p of White => "W" | Black => "B")
val prShow : show piecerec = mkShow (fn p => "{Piece=" ^ (show p.Piece) ^ ", X=" ^ (show p.X) ^ ", Y=" ^ (show p.Y) ^"}")

fun dbgSh [a ::: Type] (_ : show a) (ls:list a) : string=
    case ls of
	[] => ""
      | h :: t =>
	(show h) ^ ", " ^ (dbgSh t)

	
type position = { Pieces: list piecerec, Player: player }

type group = list piecerec

fun isadjacentto (h:piecerec) (t:piecerec) : bool =
    ((h.X = (t.X - 1) || h.X = (t.X + 1)) && h.Y = t.Y)
    ||
    ((h.Y = (t.Y - 1) || h.Y = (t.Y + 1)) && h.X = t.X)
    
		
fun isadjacenttoany (p:piecerec) (pieces:list piecerec) : bool =
    case pieces of
	[] => False
      | h :: t =>
	if isadjacentto h p then
	    True
	else
	    isadjacenttoany p t

fun inlist e l =
    case l of
	[] => False
      | h :: t =>
	if h.X = e.X && h.Y = e.Y && h.Piece = e.Piece then
	    True
	else
	    inlist e t

fun alladjacentto (piece: piecerec) (board : list piecerec) : group * list piecerec =
(* for each piece provided, find its adjacents *)
(* for each adjacent, get its adjacents *)
(* consume the board and return the rest *)
    let
	fun allAdjacentToAux piece board aLs aRest aConsidered =
	    case board of
		[] => (aLs, aRest, aConsidered)
	      | h :: t =>
		if h.Piece = piece.Piece && (isadjacentto h piece) then
		    (if inlist h aConsidered then
			 allAdjacentToAux piece t aLs aRest aConsidered
		     else
			 let
			     val (adjOfAdj, rest2, considered2) = allAdjacentToAux h board (h :: aLs) aRest (h :: aConsidered)
			 in
			     allAdjacentToAux piece t adjOfAdj rest2 considered2
			 end)
		else
		    allAdjacentToAux piece t aLs (h :: aRest) aConsidered

	val (ls, rest, _) = allAdjacentToAux piece board (piece :: []) [] (piece :: []) 
    in
	(ls, rest)
    end
   	    
		
fun allgroups (pieces:list piecerec) (pl:player) : list group =
    case pieces of
	[] => []
      | h :: t =>
	if h.Piece = pl then
	    let
		val (grp, rest) = alladjacentto h t
	    in
		grp :: (allgroups rest pl)
	    end
	else
	    allgroups t pl

fun other p =
    case p of
	White => Black
      | Black => White

fun piecesAdjacent p pl board =
    case board of
	[] => []
      | h :: t =>
	if (isadjacentto h p) && h.Piece = pl then
	    h :: (piecesAdjacent p pl t)
	else
	    piecesAdjacent p pl t

fun allgroupsofstones sqs board =
    case sqs of
	[] => []
      | h :: t =>
	let
	    val (grp, rest) = alladjacentto h board
	in
	    grp :: (allgroupsofstones t rest)
	end

fun groupsadjacentto h board =
    let
	val p = h.Piece
    in
	allgroupsofstones (piecesAdjacent h (other p) board) board
    end

fun free board x y =
    case board of
	[] => True
      | h :: t => if h.X = x && h.Y = y then
		      False
		  else
		      free t x y
	    
fun countlibertiesp (p:piecerec) (board:list piecerec) : int =
    (if free board (p.X + 1) p.Y then 1 else 0)
    +
    (if free board (p.X - 1) p.Y then 1 else 0)
    +
    (if free board p.X (p.Y + 1) then 1 else 0)
    +
    (if free board p.X (p.Y - 1) then 1 else 0)
    
	    
fun countliberties (grp : group) (board:list piecerec) : int =
    case grp of
	[] => 0
      | h :: t =>
	(countlibertiesp h board) + (countliberties t board)

fun removeIfInL torem pieces =
    case pieces of
	[] => []
      | h :: t =>
	if h.X = torem.X && h.Y = torem.Y then
	    t
	else
	    h :: (removeIfInL torem t)
    
fun removeAll p torem =
    case torem of
	[] => p
      | h :: t =>
	let
	    val r = removeIfInL h p
	in
	    removeAll r t
	end	    

fun removeAllG p gr =
    case gr of
	[] => p
      | h :: t =>
	removeAllG (removeAll p h) t

fun legal (pos: position) (newmove : piecerec) : bool =
    let
	val tmp = newmove :: pos.Pieces
	val (grp, _) = alladjacentto newmove tmp
    in
	(countliberties grp tmp) > 0
    end

fun move (pos: position) (newmove : piecerec) : transaction (option position) =
    if legal pos newmove then
	let
	    val tmp = newmove :: pos.Pieces
	    val adj = groupsadjacentto newmove tmp	    
	    val noliberties = List.filter
				  (fn g => (countliberties g tmp) = 0) adj
	    val removed = removeAllG tmp noliberties
	in
	    debug "a:";
	    debug (dbgSh (piecesAdjacent newmove (other newmove.Piece) tmp));
	    debug (dbgSh (allgroupsofstones (piecesAdjacent newmove (other newmove.Piece) tmp) tmp));   
	    debug "1:";
	    debug (show (List.length adj));
	    debug "2:";
	    debug (show (List.length noliberties));
	    debug "3:";
	    debug (show (case noliberties of [] => 0 | h :: _ => (List.length h) ));
	    return (Some {Pieces = removed,
		  Player = (other pos.Player)})
	end
    else
	return None
