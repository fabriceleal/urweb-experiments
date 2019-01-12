open Canvas_FFI

datatype player = White | Black

val playerEq : eq player = mkEq (fn a b =>
				    case (a, b) of
					(White, White) => True
				      | (Black, Black) => True
				      | (_, _) => False)

type piecerec = { Piece: player, X: int, Y: int }

val precEq : eq piecerec = mkEq (fn a b => a.X = b.X && a.Y = b.Y && a.Piece = b.Piece)
(*
val plShow = mkShow (fn p => case p of White => "W" | Black => "B")
val prShow : show piecerec = mkShow (fn p => "{Piece=" ^ (show p.Piece) ^ ", X=" ^ (show p.X) ^ ", Y=" ^ (show p.Y) ^"}")
*)

type positionSimple = { Pieces: list piecerec }
type position = { Pieces: list piecerec, Player: player, Previous : option positionSimple }

val coordinates = "abcdefghijklmnopqrstuvwxyz"

fun pToS (p : position) : string =
    let
	fun showplayer pl =
	    case pl of
		White => "W"
	      | Black => "B"

	fun piece piece =
	    (show (strsub coordinates piece.X)) ^ (show (strsub coordinates piece.Y))
	    
	fun stones pieces player =
	    case pieces of
		[] => ""
	      | h :: t =>
		if player = h.Piece then
		    "[" ^ (piece h)  ^ "]" ^ (stones t player)
		else
		    stones t player
    in
	"AB" ^ (stones p.Pieces Black) ^ "AW" ^ (stones p.Pieces White) ^ "PL" ^ "[" ^ (showplayer p.Player) ^ "]" 
    end

fun sToP (s : string) : position =
	let
	    fun consumePlayer s =
		if (strlen s) > 2 then
		    case (substring s 0 3) of
			"[W]" => (White, strsuffix s 3)
		      | "[B]" => (Black, strsuffix s 3)
		      | _ => (Black, strsuffix s 3)
		else
		    (Black, "")
			 
	    fun consumeStones s p =
		let
		    fun consumeAux s p ls =
			if (strlen s) > 0 then
			    let
				val h = substring s 0 1
			    in
				if h = "[" then
				    let
					val col = substring s 1 1
					val row = substring s 2 1
				    in
					(case (strsindex coordinates col, strsindex coordinates row) of
					     (Some col', Some row') => 
					     consumeAux (strsuffix s 4) p ({Piece = p, X = col', Y = row'} :: ls)
					   | _ => (ls, (strsuffix s 4)))
				    end
				else
				    (ls, s)
			    end
			else
			    (ls, "")
		in
		    consumeAux s p []
		end
	    fun consume p s =
		if (strlen s) > 1 then
		    let
			val h = substring s 0 2
		    in
			if h = "AW" then
			    let 
				val (stones, s2) = consumeStones (strsuffix s 2) White
			    in
				consume {Pieces = List.append p.Pieces stones, Player = p.Player} s2
			    end
			else
			    (if h = "AB" then
				 let 
				     val (stones, s2) = consumeStones (strsuffix s 2) Black
				 in
				     consume {Pieces = List.append p.Pieces stones, Player = p.Player} s2
				 end
			     else
				 (if h = "PL" then
				      let
					  val (p', s2) = consumePlayer (strsuffix s 2)
				      in
					  consume { Pieces = p.Pieces, Player = p' } s2
				      end
				  else
				      p))			    
		    end
		else
		    p

	    val consumed = consume { Pieces = [], Player = Black } s
		   
	in
	    { Pieces = consumed.Pieces, Player = consumed.Player, Previous = None }
	end

    
val startingPosition : position = { Pieces = [], Player = Black, Previous = None }
		
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
	if h = e then
	    True
	else
	    inlist e t

fun alladjacentto (piece: piecerec) (board : list piecerec) : group * list piecerec =
(* for each piece provided, find its adjacents *)
(* for each adjacent, get its adjacents *)
(* consume the board and return the rest *)
    let
	fun allAdjacentToAux piece board aLs aRest aPending =
	    case board of
		[] =>
		(case aPending of
		     [] => (aLs, aRest)
		   | h :: t =>
		     allAdjacentToAux h aRest aLs [] t)
	      | h :: t =>
		if h.Piece = piece.Piece && (isadjacentto h piece) then
		    (if inlist h aLs then
			 allAdjacentToAux piece t aLs aRest aPending
		     else
			 allAdjacentToAux piece t (h :: aLs) aRest (h :: aPending))
		else
		    (if inlist h aLs then
			 allAdjacentToAux piece t aLs aRest aPending
		     else
			 allAdjacentToAux piece t aLs (h :: aRest) aPending)

	val (ls, rest) = allAdjacentToAux piece board (piece :: []) [] []
    in
	if inlist piece board then
	    (ls, rest)
	else
	    ([], board)
    end
   	    
		
fun allgroups (pieces:list piecerec) (pl:player) : list group =
    case pieces of
	[] => []
      | h :: t =>
	if h.Piece = pl then
	    let
		val (grp, rest) = alladjacentto h t
	    in
		if (List.length grp) > 0 then
		    grp :: (allgroups rest pl)
		else
		    allgroups rest pl
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
	    if (List.length grp) > 0 then		
		grp :: (allgroupsofstones t rest)
	    else
		allgroupsofstones t rest
	end

fun groupsadjacentto h board =
    let
	val p = h.Piece
	val agrps = allgroupsofstones (piecesAdjacent h (other p) board) board
    in
	agrps
    end

fun free board x y =
    if x > -1 && x <= 18 && y > -1 && y <= 18 then
	(case board of
	     [] => True
	   | h :: t => if h.X = x && h.Y = y then
			   False
		       else
			   free t x y)
    else
	False
	    
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
	if h = torem then
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

fun makemove (pos: position) (newmove : piecerec) : position =
    let
	val tmp = newmove :: pos.Pieces
	val adj = groupsadjacentto newmove tmp
	val noliberties = List.filter
			      (fn g => (countliberties g tmp) = 0) adj
	val removed = removeAllG tmp noliberties
    in
	{Pieces = removed,
	 Player = (other pos.Player),
	 Previous = Some {Pieces = pos.Pieces}}
    end

fun containsPieceAt ps x y =
    case ps of
	[] => False
      | h :: t =>
	if h.X = x && h.Y = y then
	    True
	else
	    containsPieceAt t x y

fun diff lsa lsb =
    case (lsa, lsb) of
	([], []) => False
      | ([], _ :: _) => True
      | (_ :: _, []) => True
      | (a' :: ta', _) =>
	let
	    fun remLs e l =
		case l of
		    [] => None
		  | h :: t =>
		    if h = e then
			Some t
		    else
			(case (remLs e t) of
			     None => None
			   | Some l => Some (h :: l))
			  
	    val res = remLs a' lsb
	in
	    case res of
		None => True
	      | Some tb' => diff ta' tb'
	end

    
fun legal (pos: position) (newmove : piecerec) : bool =
    if containsPieceAt pos.Pieces newmove.X newmove.Y then
	False
    else	
	let
	    val tmp' = makemove pos newmove
	    val tmp = tmp'.Pieces
	    val (grp, _) = alladjacentto newmove tmp
	in
	    (countliberties grp tmp) > 0
	    &&
	    (case pos.Previous of
		 None => True
	       | Some prev => diff prev.Pieces tmp)
	end

fun move (pos: position) (newmove : piecerec) : option position =
    if legal pos newmove then
	Some (makemove pos newmove)
    else
	None

datatype gameTree = Node of int * position * piecerec * string * (list string) * (list gameTree)		    
datatype gameRoot = Root of int * position * list gameTree * list (string * string)

fun getPosition t =
    case t of
	Root (_, p, _, _) =>
	p

fun generate_board (tree : gameRoot) (onChange : position -> transaction unit) : transaction xbody =
    c <- fresh;
    rs <- source (Some {Mouse=None, Position=getPosition tree});
    let
	val stonehf = 13
	val stonesz = stonehf * 2
	val space = 27
	val offs = stonesz
	val lines = 18 (* actual lines - 1*)
	val w = space * lines
	val h = space * lines
	val cw = w + (offs * 2)
	val ch = h + (offs * 2)

	fun screenToCoord rawX rawY =
	    let
		val xx = (rawX - offs + space / 2) / space
		val yy = (rawY - offs + space / 2) / space
	    in
		if xx > -1 && xx <= lines &&
		   yy > -1 && yy <= lines then
		    Some {X=xx , Y=yy}
		else
		    None
	    end

	fun mousemove e =
	    let
		val c = screenToCoord e.OffsetX e.OffsetY
	    in
		s <- get rs;
		case s of
		    None => return ()
		  | Some s' =>
		    set rs (Some {
			    Position = s'.Position,
			    Mouse = c
			   })
	    end
	    
	fun mouseclick e =
	    let
		val cc = screenToCoord e.OffsetX e.OffsetY
	    in
		case cc of
		    None => return ()
		  | Some c => 	       
		    s <- get rs;
		    case s of
			None => return ()
		      | Some s' =>
			let
			    val r = {Piece=s'.Position.Player,
				     X = c.X,
				     Y = c.Y}
			in
			    case (move s'.Position r) of
				None => return ()
			      | Some p =>
				set rs (Some {
					Position = p,
					Mouse = s'.Mouse
				       });
				onChange p
			end			    
	    end
	    
	fun onloadFn () =
	    ctx <- getContext2d c;
	    wstone <- make_img(bless("/w.svg"));
	    bstone <- make_img(bless("/b.svg"));
	    let
		fun playerToStone p =
		    case p of
			White => wstone
		      | Black => bstone

		and noneAt pieces x y =
		    case pieces of
			[] => True
		      | h :: t =>
			if h.X = x && h.Y = y then
			    False
			else
			    noneAt t x y
			    
		and drawHs () =
		    let
			fun drawH n =
			    drawLine ctx offs (offs + n * space) (w + offs) (offs + n * space);
			    if n = 0 then
				return ()
			    else
				drawH (n - 1)
				
		    in
			drawH lines
		    end

		and drawVs () =
		    let
			fun drawV n =
			    drawLine ctx (offs + n * space) offs (offs + n * space) (h + offs);
			    if n = 0 then
				return ()
			    else				
				drawV (n - 1)
				
		    in
			drawV lines
		    end

		and drawMarker x y =		    
		    beginPath ctx;
		    arc ctx
			(float ((x * space) + offs))
			(float ((y * space) + offs))
			4.0 0.0 (3.14 * 2.0) False;
		    closePath ctx;
		    fill ctx
		    
		and drawStone img x y =
		    drawImage2 ctx img
			       (float ((x * space) + offs - stonehf))
			       (float ((y * space) + offs - stonehf))
			       (float stonesz) (float stonesz)

		and drawStones pieces =
		    case pieces of
			[] => return ()
		      | h :: t =>
			drawStone (playerToStone h.Piece) h.X h.Y;
			drawStones t
			
		and drawPosition p =
		    case p of
			None => return ()
		      | Some p' =>
			drawStones p'.Position.Pieces;
			case p'.Mouse of
			    None => return ()
			  | Some p'' =>
			    if noneAt p'.Position.Pieces p''.X p''.Y then
				drawStone (playerToStone p'.Position.Player) p''.X p''.Y
			    else
				return ()
				
		and drawBoard () =
		    clearRect ctx (float 0) (float 0) (float cw) (float ch);
		    setFillStyle ctx (make_rgba 212 172 89 1.0);
		    fillRect ctx 0 0 cw ch;		    
		    drawHs ();
		    drawVs ();

		    setFillStyle ctx (make_rgba 0 0 0 1.0);
		    drawMarker 3 3;
		    drawMarker 3 9;
		    drawMarker 3 15;

		    drawMarker 9 3;
		    drawMarker 9 9;
		    drawMarker 9 15;
		    
		    drawMarker 15 3;
		    drawMarker 15 9;
		    drawMarker 15 15;			

		    p <- get rs;
		    drawPosition p;
		    
		    return ()
	    in
		requestAnimationFrame2 drawBoard;
		return <xml></xml>
	    end	    
    in	
	return <xml>
	  <canvas id={c} width={cw} height={ch} onclick={mouseclick} onmousemove={mousemove}>
	  </canvas>
	  <active code={onloadFn ()}>
	  </active>
	</xml>
    end
