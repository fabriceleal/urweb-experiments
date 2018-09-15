
datatype piece = WhiteKing | WhiteQueen | WhiteRook | WhiteBishop | WhiteKnight | WhitePawn |
	 BlackKing | BlackQueen | BlackRook | BlackBishop | BlackKnight | BlackPawn

datatype kind = King | Queen | Rook | Bishop | Knight | Pawn

datatype player = White | Black

datatype castle = Kingside | Queenside

type piecerec = { X: int, Y : int, Piece : piece  }

type square = { X: int, Y : int}

type move = { Src: square, Dest: square, Prom: option kind}
	      
type gamestate = {
     Pieces : list piecerec,
     Player : player,
     WK : bool,
     WQ : bool,
     BK : bool,
     BQ : bool,
     EnPassant : option square,
     HalfMove : int,
     FullMove : int
     }

(*  
val pieces : list piecerec =
    { X= 0, Y= 0, Piece= BlackRook}  ::
				     { X= 1, Y= 0, Piece= BlackKnight} ::
				     { X= 2, Y= 0, Piece= BlackBishop} ::
				     { X= 3, Y= 0, Piece= BlackQueen} ::
				     { X= 4, Y= 0, Piece= BlackKing} ::
				     { X= 5, Y= 0, Piece= BlackBishop}  ::
				     { X= 6, Y= 0, Piece= BlackKnight} ::
				     { X= 7, Y= 0, Piece= BlackRook} ::
				     { X= 0, Y= 1, Piece= BlackPawn} ::
				     { X= 1, Y= 1, Piece= BlackPawn} ::
				     { X= 2, Y= 1, Piece= BlackPawn} ::
				     { X= 3, Y= 1, Piece= BlackPawn} ::
				     { X= 4, Y= 1, Piece= BlackPawn} ::
				     { X= 5, Y= 1, Piece= BlackPawn} ::
				     { X= 6, Y= 1, Piece= BlackPawn} ::
				     { X= 7, Y= 1, Piece= BlackPawn} ::
				     { X= 0, Y= 6, Piece= WhitePawn} ::
				     { X= 1, Y= 6, Piece= WhitePawn} ::
				     { X= 2, Y= 6, Piece= WhitePawn} ::
				     { X= 3, Y= 6, Piece= WhitePawn} ::
				     { X= 4, Y= 6, Piece= WhitePawn} ::
				     { X= 5, Y= 6, Piece= WhitePawn} ::
				     { X= 6, Y= 6, Piece= WhitePawn} ::
				     { X= 7, Y= 6, Piece= WhitePawn} ::
				     { X= 0, Y= 7, Piece= WhiteRook}  ::
				     { X= 1, Y= 7, Piece= WhiteKnight} ::
				     { X= 2, Y= 7, Piece= WhiteBishop} ::
				     { X= 3, Y= 7, Piece= WhiteQueen} ::
				     { X= 4, Y= 7, Piece= WhiteKing} ::
				     { X= 5, Y= 7, Piece= WhiteBishop}  ::
				     { X= 6, Y= 7, Piece= WhiteKnight} ::
				     { X= 7, Y= 7, Piece= WhiteRook} :: []
*)
    	
(* functions *)
		 
fun fileStr f =
    case f of
	0 => "a"
      | 1 => "b"
      | 2 => "c"
      | 3 => "d"
      | 4 => "e"
      | 5 => "f"
      | 6 => "g"
      | 7 => "h"
      | _ => "?"

fun fileToI i =
    case i of
	#"a" => 0
      | #"b" => 1
      | #"c" => 2
      | #"d" => 3
      | #"e" => 4
      | #"f" => 5
      | #"g" => 6
      | #"h" => 7
      | _ => -1
	
fun validSq x y = x >= 0 && x < 8 && y >= 0 && y < 8
						       
(* transform list option T -> list T *) 
fun removeNones [a] (ls : list (option a)) : list a  =
    case ls of
	h :: t =>
	(case h of
	     Some thing => thing :: removeNones t
	   | None => removeNones t)
      | [] => []

fun other a =
    case a of
      | White => Black
      | Black => White
		 
fun peq a b =
    case a of
	White => (case b of
		     Black => False
		   | White => True)
      | Black => (case b of
		      White => False
		    | Black => True)


fun keq a b =
    case (a, b) of
    | (King, King) => True
    | (Queen, Queen) => True
    | (Rook, Rook) => True
    | (Bishop, Bishop) => True
    | (Knight, Knight) => True
    | (Pawn, Pawn) => True
    | _ => False


fun piece_to_kind p =
    case p of
	BlackKing => King | WhiteKing => King
      | BlackQueen => Queen | WhiteQueen => Queen
      | BlackRook => Rook | WhiteRook => Rook
      | BlackBishop => Bishop | WhiteBishop => Bishop
      | BlackKnight => Knight | WhiteKnight => Knight
      | BlackPawn => Pawn | WhitePawn => Pawn

fun piece_to_player p =
    case p of
    	BlackKing => Black | WhiteKing => White
      | BlackQueen => Black | WhiteQueen => White
      | BlackRook => Black | WhiteRook => White
      | BlackBishop => Black | WhiteBishop => White
      | BlackKnight => Black | WhiteKnight => White
      | BlackPawn => Black | WhitePawn => White

fun mkPiece k p =
    case p of
     White => (case k of
		   King => WhiteKing
		 | Queen => WhiteQueen
		 | Rook => WhiteRook
		 | Bishop => WhiteBishop
		 | Knight => WhiteKnight
		 | Pawn => WhitePawn)
   | Black => (case k of
		   King => BlackKing
		 | Queen => BlackQueen
		 | Rook => BlackRook
		 | Bishop => BlackBishop
		 | Knight => BlackKnight
		 | Pawn => BlackPawn)
					 
fun piece_to_char p =
    case p of
	BlackKing => #"k"
      | BlackQueen => #"q"
      | BlackRook => #"r"
      | BlackBishop => #"b"
      | BlackKnight => #"n"
      | BlackPawn => #"p"
		     
      | WhiteKing => #"K"
      | WhiteQueen => #"Q"
      | WhiteRook => #"R"
      | WhiteBishop => #"B"
      | WhiteKnight => #"N"
      | WhitePawn => #"P"

fun kind_to_str p =
    case p of
	King => "k"
      | Queen => "q"
      | Rook => "r"
      | Bishop => "b"
      | Knight => "n"
      | Pawn => "p"

fun str_to_kind s =
    case s of
	"k" => Some King
      | "q" => Some Queen
      | "r" => Some Rook
      | "b" => Some Bishop
      | "n" => Some Knight
      | "p" => Some Pawn
      | _ => None
		  
fun char_to_piece c =
    case c of
	#"k" => Some BlackKing
      | #"q" => Some BlackQueen
      | #"r" => Some BlackRook
      | #"b" => Some BlackBishop
      | #"n" => Some BlackKnight
      | #"p" => Some BlackPawn

      | #"K" => Some WhiteKing
      | #"Q" => Some WhiteQueen
      | #"R" => Some WhiteRook
      | #"B" => Some WhiteBishop
      | #"N" => Some WhiteKnight
      | #"P" => Some WhitePawn
		
      | _ => None


fun playerToFen c =
    case c of
	White => "w"
      | Black => "b"

fun fenToPlayer f =
    case f of
	#"w" => Some White
      | #"b" => Some Black
      | _ => None
		 
fun fen_to_state s =
    let
	fun parts_pieces_aux pieces player wk wq bk bq enpx enpy hm fm =	
	    {
	     Pieces = pieces,
	     Player = player,
	     WK = wk,
	     WQ = wq,
	     BK = bk,
	     BQ = bq,
	     EnPassant = if (validSq enpx enpy) then
			     Some {X=enpx,Y=enpy}
			 else
			     None,
	     HalfMove = hm,
	     FullMove = fm
	    }

	and fromFM s pieces player wk wq bk bq enpx enpy hm fm =
	    let
		val l = strlen s
	    in
		if l = 0 then
		    (* failback *)
		    parts_pieces_aux pieces player wk wq bk bq enpx enpy hm 1
		else
		    let
			val fst = (strsub s 0)
		    in
			case fst of
			    #" " =>
			    fromFM (substring s 1 (l -1)) pieces player wk wq bk bq enpx enpy hm (-1)
			  | _ => 
			    case (read s : option int) of
				None => (* failback *)
				parts_pieces_aux pieces player wk wq bk bq enpx enpy hm 1
			      | Some v =>
				parts_pieces_aux pieces player wk wq bk bq enpx enpy hm v
		    end
	    end
	

	and fromHM s pieces player wk wq bk bq enpx enpy hm =
	    let
		val l = strlen s
	    in
		if l = 0 then
		    (* failback *)
		    parts_pieces_aux pieces player wk wq bk bq enpx enpy 0 1
		else
		    let
			val fst = (strsub s 0)
		    in
			case fst of
			    #" " =>
			    fromHM (substring s 1 (l -1)) pieces player wk wq bk bq enpx enpy (-1)
			  | _ =>
			    let
				val idx = strsindex s " "
			    in
				case idx of
				    None => (* failback *)
				    parts_pieces_aux pieces player wk wq bk bq enpx enpy 0 1
				  | Some idx' =>
				    case (read (substring s 0 idx') : option int) of
					None => (* failback *)
					parts_pieces_aux pieces player wk wq bk bq enpx enpy 0 1
				      | Some v =>
					fromFM (substring s idx' ((strlen s) - idx')) pieces player wk wq bk bq enpx enpy v 1
			    end
		    end
	    end
			
	    
	and fromEnPassant s pieces player wk wq bk bq enpx enpy =
	    let
		val l = strlen s
	    in
		if l = 0 then		    
		    parts_pieces_aux pieces player wk wq bk bq enpx enpy 0 1 (* trigger error ? *)
		else
		    let
			val fst = (strsub s 0)
		    in
			case fst of			  
			    #" " => parts_pieces_aux pieces player wk wq bk bq enpx enpy 0 1
			  | #"-" => (*parts_pieces_aux pieces player wk wq bk bq enpx enpy 0 1*)
				    fromHM (substring s 1 (l -1)) pieces player wk wq bk bq enpx (-1) (-1)
			  | _ =>
			    if (isdigit fst) then
				case (read (show fst) : option int) of
				    None => parts_pieces_aux pieces player wk wq bk bq enpx enpy 0 1
				  | Some v => fromHM (substring s 1 (l -1)) pieces player wk wq bk bq enpx (7 - (v - 1)) (-1)
			    else
				fromEnPassant (substring s 1 (l -1)) pieces player wk wq bk bq (fileToI fst) enpy
			    
		    end
	    end

	and fromcastling s pieces player wk wq bk bq =
	    let
		val l = strlen s
	    in
		if l = 0 then		    
		    parts_pieces_aux pieces player wk wq bk bq (-1) (-1) 0 1 (* trigger error ? *)
		else
		    let
			val fst = (strsub s 0)
		    in
			case fst of
			    #"K" => fromcastling (substring s 1 (l -1)) pieces player True wq bk bq
			  | #"Q" => fromcastling (substring s 1 (l -1)) pieces player wk True bk bq 
			  | #"k" => fromcastling (substring s 1 (l -1)) pieces player wk wq True bq 
			  | #"q" => fromcastling (substring s 1 (l -1)) pieces player wk wq bk True 
			  | #"-" => fromcastling (substring s 1 (l -1)) pieces player wk wq bk bq 
			  | #" " => fromEnPassant (substring s 1 (l -1)) pieces player wk wq bk bq (-1) (-1) 
			  | _ => parts_pieces_aux pieces player wk wq bk bq (-1) (-1) 0 1 (* trigger error ? *)
		    end
	    end
	    
	and fromplayer s pieces =
	    let
		val l = strlen s
	    in
		if l = 0 then
		    parts_pieces_aux pieces White False False False False (-1) (-1) 0 1 (* trigger error ? *)
		else
		    let
			val fst = (strsub s 0)
		    in
			case (fenToPlayer fst) of
			    None => parts_pieces_aux pieces White False False False False (-1) (-1) 0 1 (* trigger error? *)
			  | Some player => fromcastling (substring s 2 (l -2)) pieces player False False False False
		    end
	    end
	
	and fen_to_pieces_aux s row col pieces =
	    let
		val l = strlen s
	    in
		if l = 0 then
		    parts_pieces_aux pieces White False False False False (-1) (-1) 0 1 (* trigger error ? *)
		else
		    let
			val fst = (strsub s 0)
		    in
			case fst of
			    #"/" => fen_to_pieces_aux (substring s 1 (l -1)) (row + 1) 0 pieces
			  | #" " => fromplayer (substring s 1 (l -1)) pieces
			  | _ => 			
			    if isdigit fst then
				case (read (show fst) : option int) of
				    Some i => fen_to_pieces_aux (substring s 1 (l -1)) row (col + i) pieces
				  | None => parts_pieces_aux pieces White False False False False (-1) (-1) 0 1 (* trigger error ? *)
			    else
				case (char_to_piece fst) of
				    None => parts_pieces_aux pieces White False False False False (-1) (-1) 0 1 (* trigger error? *)
				  | Some p => fen_to_pieces_aux (substring s 1 (l - 1)) row (col + 1)
								({X = col, Y = row, Piece= p} :: pieces)
		    end
	    end
	    
    in
	fen_to_pieces_aux s 0 0 []
    end


fun pieceInSquare x y =
    let
	fun tmp (pp : piecerec) =
	    pp.X = x && pp.Y = y
    in
	tmp
    end
(*  
fun pieceAt (ls : list piecerec) (f : piecerec -> bool) : option piecerec =
    case ls of
	h :: r =>
	if (f h) then
	    Some h
	else
	    pieceAt r f
      | [] => None
*)
fun pieceAtKP (ls : list piecerec) kind player : option piecerec =
    case ls of
	h :: r =>
	(case (peq player (piece_to_player h.Piece), (keq kind (piece_to_kind h.Piece))) of
	    (True, True) => Some h
	  | _ => pieceAtKP r kind player)
      | [] => None 
	    
	      
fun pieceAt2 (ls : list piecerec) (x : int) (y : int) : option piecerec =
    case ls of
	h :: r =>
	if (h.X = x && h.Y = y) then
	    Some h
	else
	    pieceAt2 r x y
      | [] => None
	      
fun removePSquare (ls : list piecerec) (f : piecerec -> bool) : (list piecerec) =
    case ls of
	h :: r =>
	if (f h) then
	    r
	else
	    h :: (removePSquare r f)		       
      | [] => []

fun removePSquare2 (ls : list piecerec) x y : (list piecerec) =
    case ls of
	h :: r =>
	if (h.X = x && h.Y = y) then
	    r
	else
	    h :: (removePSquare2 r x y)		       
      | [] => []
	      
fun removeFromAddAt (pieces : list piecerec) (sqSrc : square) (sqDest : square) =
    let
	val maybepiece = pieceAt2 pieces sqSrc.X sqSrc.Y
    in
	case maybepiece of
	    Some piece => 
	    { Piece=piece.Piece,X=sqDest.X, Y=sqDest.Y } :: (removePSquare2 (removePSquare2 pieces sqSrc.X sqSrc.Y) sqDest.X sqDest.Y)
	  | None => pieces
    end


fun rank_to_fenFrag (pieces : list piecerec) (rank : int) (empty : int) (file : int) : list string  =
    let
	val p = pieceAt2 pieces file rank
    in
	if file = 8 then
	    if (empty > 0) then
		    (show empty) :: []
		else
		    []
	else	    
	    case p of
		Some piece =>
		(if (empty > 0) then
		     show empty
		 else
		     "") :: ((show (piece_to_char piece.Piece)) :: (rank_to_fenFrag pieces rank 0 (file + 1)))
	      | None =>	    
		rank_to_fenFrag pieces rank (empty + 1) (file + 1)
		
    end

fun pieces_to_fen pieces =
    let
	fun pieces_to_fen_aux pieces rank =
	    if (rank = 8) then
		[]
	    else
		List.append ("/" :: (rank_to_fenFrag pieces rank 0 0)) (pieces_to_fen_aux pieces (rank + 1))
    in
	List.append (rank_to_fenFrag pieces 0 0 0) (pieces_to_fen_aux pieces 1)
    end

fun castlingToFen state =
    if (state.WK || state.WQ || state.BK || state.BQ) then
	removeNones ((if state.WK then
			  Some "K"
		      else
			  None) :: (if state.WQ then
					Some "Q"
				    else
					None) :: (if state.BK then
						      Some "k"
						  else
						      None) :: (if state.BQ then
								    Some "q"
								else
								    None) :: [])
    else
	"-" :: []

fun sqStr sq =
    strcat (fileStr sq.X) (show (7 - sq.Y + 1))

fun moveStr (mov : move) =
    (sqStr mov.Src) ^ (sqStr mov.Dest) ^ (case mov.Prom of
						    Some k => kind_to_str k
						  | None => "")

fun str_to_move str =
    let
	val len = strlen str
		
	fun str_to_sq s =
	    let
		val l = strlen s
	    in
		if l > 1 then
		    let
			val fst = strsub s 0
			val snd = strsub (substring s 1 (l -1)) 0
		    in		    
			{X = fileToI fst, Y = (if (isdigit snd) then
						   (case (read (show snd) : option int) of
							None => -1
						      | Some v => (7 - (v - 1)))   
					       else
						   -1) }
		    end
		else
		    {X=-1, Y=-1}
	    end
    in
	{Src=(str_to_sq str), Dest = (str_to_sq (substring str 2 (len -2))), Prom = (str_to_kind (substring str 4 (len -4))) }
    end

    
fun enPassantToFen state =
    case state.EnPassant of
	None => "-" :: []
      | Some sq => sqStr sq :: []
    
fun state_to_fen state : string  =
    List.foldr strcat "" (List.append (pieces_to_fen state.Pieces)
				      (" " :: (playerToFen state.Player ::
							   (List.append
								(" " :: (castlingToFen state))
								(List.append
								     (" " :: (enPassantToFen state))
								     (" " :: (show state.HalfMove) :: " " :: (show state.FullMove) :: []) ) 
								
			 ))))


datatype sqPresence = Empty | Foe | Nvm

fun testSq pieces player x y =
    if (validSq x y) then
	case (pieceAt2 pieces x y) of
	    Some piece =>
	    let
		val pplayer = (piece_to_player piece.Piece)
	    in
		case (peq pplayer player) of
		    True => Nvm
		  | False => Foe
	    end
	  | None => Empty
    else
	Nvm

				    
(* is valid sq and is empty *)
fun pempty pieces player x y =
    case (testSq pieces player x y) of
	Empty => True
      | _ => False

(* is valid sq and has a foe *)
fun pfoe pieces player x y =
    case (testSq pieces player x y) of
	Foe => True
      | _ => False

fun pfoeOrSq pieces player x y sqq =
    (case sqq of
	 Some sq => x = sq.X && y = sq.Y
       | None => False) || pfoe pieces player x y 

(* is valid sq and is empty or has a foe *)
fun pemptyOrFoe pieces player x y =
    case (testSq pieces player x y) of
	Empty => True
      | Foe => True
      | _ => False

fun legalsPawnSL pieces player src =
    case player of
	White =>
	List.append	    
	    (List.append
		 (if src.Y = 6 && (pempty pieces player src.X (src.Y - 1)) && (pempty pieces player src.X (src.Y - 2)) then
		      {X = src.X, Y = src.Y - 2} :: []
		  else 
		      [])
		 (if (pempty pieces player src.X (src.Y - 1)) then
		      { X=src.X, Y = src.Y - 1 } :: []
		  else
		      []))
	    
	    (List.append 
		 (if (pfoe pieces player (src.X - 1) (src.Y - 1) ) then
		      { X=src.X - 1, Y = src.Y - 1 } :: []
		  else
		      [])
		 
		 (if (pfoe pieces player (src.X + 1) (src.Y - 1) ) then
		      { X=src.X + 1,  Y = src.Y - 1 } :: []
		  else
		      []))
	

      | Black =>
	List.append	    
	    (List.append
		 (if src.Y = 1 && (pempty pieces player src.X (src.Y + 1)) && (pempty pieces player src.X (src.Y + 2)) then
		      {X = src.X, Y = src.Y + 2} :: []
		  else 
		      [])
		 (if (pempty pieces player src.X (src.Y + 1)) then
		      { X=src.X, Y = src.Y + 1 } :: []
		  else
		      []))
	    
	    (List.append 
		 (if (pfoe pieces player (src.X - 1) (src.Y + 1) ) then
		      { X=src.X - 1, Y = src.Y + 1 } :: []
		  else
		      [])
		 
		 (if (pfoe pieces player (src.X + 1) (src.Y + 1) ) then
		      { X=src.X + 1,  Y = src.Y + 1 } :: []
		  else
		      []))

	     
	     
fun legalsPawn state player src =
    case player of
	White =>
	List.append	    
	    (List.append
		 (if src.Y = 6 && (pempty state.Pieces player src.X (src.Y - 1)) && (pempty state.Pieces player src.X (src.Y - 2)) then
		      {X = src.X, Y = src.Y - 2} :: []
		  else 
		      [])
		 (if (pempty state.Pieces player src.X (src.Y - 1)) then
		      { X=src.X, Y = src.Y - 1 } :: []
		  else
		      []))
	    
	    (List.append 
		 (if (pfoeOrSq state.Pieces player (src.X - 1) (src.Y - 1) state.EnPassant) then
		      { X=src.X - 1, Y = src.Y - 1 } :: []
		  else
		      [])
		 
		 (if (pfoeOrSq state.Pieces player (src.X + 1) (src.Y - 1) state.EnPassant) then
		      { X=src.X + 1,  Y = src.Y - 1 } :: []
		  else
		      []))
	

      | Black =>
	List.append	    
	    (List.append
		 (if src.Y = 1 && (pempty state.Pieces player src.X (src.Y + 1)) && (pempty state.Pieces player src.X (src.Y + 2)) then
		      {X = src.X, Y = src.Y + 2} :: []
		  else 
		      [])
		 (if (pempty state.Pieces player src.X (src.Y + 1)) then
		      { X=src.X, Y = src.Y + 1 } :: []
		  else
		      []))
	    
	    (List.append 
		 (if (pfoeOrSq state.Pieces player (src.X - 1) (src.Y + 1) state.EnPassant) then
		      { X=src.X - 1, Y = src.Y + 1 } :: []
		  else
		      [])
		 
		 (if (pfoeOrSq state.Pieces player (src.X + 1) (src.Y + 1) state.EnPassant) then
		      { X=src.X + 1,  Y = src.Y + 1 } :: []
		  else
		      []))

fun genSlide pieces player src dX dY = 
    let
	val testX = src.X + dX
	val testY = src.Y + dY
    in
	case (testSq pieces player testX testY) of
	    Empty =>
	    let
		val sq = {X=testX, Y=testY}
	    in
		sq :: genSlide pieces player sq dX dY
	    end
	  | Foe =>
	    {X=testX, Y=testY} :: []
	  | Nvm => []
    end

(* get square if its valid for landing a piece *)
fun offTest pieces player src dX dY =
    let
	val test = {X=src.X + dX,Y=src.Y + dY}
    in
	if (pemptyOrFoe pieces player test.X test.Y) then
	    Some test
	else
	    None
    end
	
fun legalsKnight pieces player src =
    removeNones (offTest pieces player src (-1) (-2) ::
			 offTest pieces player src (-1) (2) ::
			 offTest pieces player src (1) (-2) ::
			 offTest pieces player src (1) (2) ::
			 offTest pieces player src (-2) (-1) ::
			 offTest pieces player src (-2) (1) ::
			 offTest pieces player src (2) (-1) ::
			 offTest pieces player src (2) (1) ::
			 [])

fun playerCanCastleK state player =
    case player of
    | White =>
      state.WK
    | Black =>
      state.BK

fun playerCanCastleQ state player =
    case player of
    | White =>
      state.WQ
    | Black =>
      state.BQ

fun legalsKingSL pieces player src =
    removeNones (offTest pieces player src (-1) (1) ::
			 offTest pieces player src (-1) (0) ::
			 offTest pieces player src (-1) (-1) ::
			 offTest pieces player src (1) (1) ::
			 offTest pieces player src (1) (0) ::
			 offTest pieces player src (1) (-1) ::
			 offTest pieces player src (0) (1) ::
			 offTest pieces player src (0) (-1) :: [])

fun legalsKing state player src =
    let
	val pieces = state.Pieces
    in
	removeNones (offTest pieces player src (-1) (1) ::
			     offTest pieces player src (-1) (0) ::
			     offTest pieces player src (-1) (-1) ::
			     offTest pieces player src (1) (1) ::
			     offTest pieces player src (1) (0) ::
			     offTest pieces player src (1) (-1) ::
			     offTest pieces player src (0) (1) ::
			     offTest pieces player src (0) (-1) ::
			     (if (playerCanCastleK state player) then
				  offTest pieces player src 2 0
			      else
				  None) ::
			     (if (playerCanCastleQ state player) then
				  offTest pieces player src (-2) 0
			      else
				  None) ::
			     [])
    end
        
fun legalsDiagonals pieces player src =
    List.append 
	(List.append
	     (genSlide pieces player src (-1) (-1))
	     (genSlide pieces player src 1 (-1)))
	(List.append
	     (genSlide pieces player src (-1) 1)
	     (genSlide pieces player src 1 1))

fun legalsOrtho pieces player src =
    List.append 
	(List.append
	     (genSlide pieces player src 0 (-1))
	     (genSlide pieces player src 0 1))
	(List.append
	     (genSlide pieces player src (-1) 0)
	     (genSlide pieces player src 1 0))

fun legalsSlide pieces player src =
    List.append (legalsDiagonals pieces player src) (legalsOrtho pieces player src)

(* test if list contains square *)
fun hasDest ls sq =
    case ls of
	h :: t =>
	if (h.X = sq.X && h.Y = sq.Y) then
	    True
	else
	    hasDest t sq
      | [] => False
	      
fun hasDestOneOf ls lssq =
    case lssq of
	h :: t =>
	if hasDest ls h then
	    True
	else
	    hasDestOneOf ls t
      | [] => False

fun legalsForPieceSL pieces piece =
    let
	val src = {X=piece.X,Y=piece.Y}
    in
	case (piece_to_kind piece.Piece) of
	    Pawn => legalsPawnSL pieces (piece_to_player piece.Piece) src 
	  | Bishop => legalsDiagonals pieces (piece_to_player piece.Piece) src
	  | Rook => legalsOrtho pieces (piece_to_player piece.Piece) src
	  | Queen => legalsSlide pieces (piece_to_player piece.Piece) src
	  | Knight => legalsKnight pieces (piece_to_player piece.Piece) src
	  | King => legalsKingSL pieces (piece_to_player piece.Piece) src
    end
	      
fun legalsForPiece state piece =
    let
	val src = {X=piece.X,Y=piece.Y}
    in
	case (piece_to_kind piece.Piece) of
	    Pawn => legalsPawn state (piece_to_player piece.Piece) src
	  | Bishop => legalsDiagonals state.Pieces (piece_to_player piece.Piece) src
	  | Rook => legalsOrtho state.Pieces (piece_to_player piece.Piece) src
	  | Queen => legalsSlide state.Pieces (piece_to_player piece.Piece) src
	  | Knight => legalsKnight state.Pieces (piece_to_player piece.Piece) src
	  | King => legalsKing state (piece_to_player piece.Piece) src
    end

fun allLegals state =
    let
	fun f e =
	    peq state.Player (piece_to_player e.Piece)
	    
	val pieces = List.filter f state.Pieces
	val legals = List.mp (legalsForPiece state) pieces 
    in
	List.foldl List.append [] legals
    end

fun getKing (state: gamestate) : option piecerec =
    pieceAtKP state.Pieces King state.Player
    
fun getEnemyKing (state: gamestate) : option piecerec =
    pieceAtKP state.Pieces King (other state.Player)


(* checks if king can be captured or if it castled through an attacked square, signal of a illegal move played *)
fun isKingCapturable (state : gamestate) castle src : bool =
    case (getEnemyKing state) of
	None => False
      | Some prec =>
	let
	    val moves = (allLegals state)
	in
	    (case castle of
		 None => False
	       | Some Kingside => hasDestOneOf moves ({X=4,Y=src.Y} :: {X=5,Y=src.Y} :: {X=6,Y=src.Y} :: [])
	       | Some Queenside => hasDestOneOf moves ({X=4,Y=src.Y} :: {X=3,Y=src.Y} :: {X=2,Y=src.Y} :: [])
	    )
	    || hasDest moves {X=prec.X, Y=prec.Y}
	end

datatype gamestatus =
	 Playing
       | Check
       | Checkmate
(*	 
fun checkState (poststate : gamestate) move =
    case (getEnemyKing state) of
	None => Playing
      | Some prec =>
	let
	    val moves = allLegals state
	in
	    if hasDest moves {X=prec.X, Y=prec.Y} then
		
	    else
		Playing
	end
	*)
fun validForProm k =
    case k of
	Queen => True
      | Rook => True
      | Bishop => True
      | Knight => True
      | _ => False  
		  
fun requiresPromotion (piece : piecerec) destY =
    case (piece_to_kind piece.Piece) of
	Pawn =>
	(case (piece_to_player piece.Piece) of
	     White => destY = 0
	   | Black => destY = 7)
      | _ => False

fun requiresPromotionSq pieces srcX srcY destX destY =
    case (pieceAt2 pieces srcX srcY) of
	None => False
      | Some p => requiresPromotion p destY

(* a move as correct promotion info if <there's no promotion specified and move doesnt require it> OR <there's a kind, 
that kind is an available kind for promotion and the move requires promotion>  *)
fun promOk piece move =
    case move.Prom of
	None => not (requiresPromotion piece move.Dest.Y)
      | Some k => (requiresPromotion piece move.Dest.Y) && (validForProm k)

(* test if a move is pseudo-legal *)
(* a move is pseudo-legal if it obeys to piece move rules. not putting/leaving the own king in check is checked afterwards *)
(* check here if promotion is correctly given as well *)
fun testLegal state move  =
    let
	val pieces = state.Pieces
	val maybepiece = pieceAt2 pieces move.Src.X move.Src.Y
    in
	case maybepiece of
	    Some piece =>
	    if (peq (piece_to_player piece.Piece) state.Player) then
		hasDest (legalsForPiece state piece) move.Dest && promOk piece move
	    else
		False
	  | None => False
    end

fun isPawnUp2Sq pieces src dest =
    case (pieceAt2 pieces src.X src.Y) of
       None => False
     | Some p =>
       case (piece_to_kind p.Piece) of
	   Pawn => abs((float src.Y) - (float dest.Y)) = 2.0
	 | _ => False

fun isKingMove player pieces src =
    case (pieceAt2 pieces src.X src.Y) of
       None => False
     | Some p =>
       if (peq (piece_to_player p.Piece) player) then
	   case (piece_to_kind p.Piece) of
	       King => True
	     | _ => False
       else
	   False

fun isRookQMove player pieces src =
    case (pieceAt2 pieces src.X src.Y) of
       None => False
     | Some p =>
       if (peq (piece_to_player p.Piece) player) then
	   case (piece_to_kind p.Piece) of
	       Rook => p.X < 4
	     | _ => False
       else
	   False
	   
fun isRookKMove player pieces src =
    case (pieceAt2 pieces src.X src.Y) of
       None => False
     | Some p =>
       if (peq (piece_to_player p.Piece) player) then
	   case (piece_to_kind p.Piece) of
	       Rook => p.X > 4
	     | _ => False
       else
	   False


fun isCastle pieces src dest =
    case (pieceAt2 pieces src.X src.Y) of
       None => None
     | Some p =>
       case (piece_to_kind p.Piece) of
	   King =>
	   if(float(dest.X) - float(src.X) > 1.0) then
	       Some Kingside
	   else
	       if(float(dest.X) - float(src.X) < -1.0) then
		   Some Queenside
	       else
		   None
	 | _ => None

fun sqBack sq player =
    let
	val back = case player of
		       White => 1
		     | Black => -1
    in
	{X=sq.X,Y=sq.Y + back}
    end
		
fun isEnPassantCapture enp pieces2 player dest =
    case enp of
	None => None
      | Some sq =>
	if (dest.X = sq.X && dest.Y = sq.Y) then
	    case (pieceAt2 pieces2 dest.X dest.Y) of
		None => None
	      | Some p => case (piece_to_kind p.Piece) of
			      Pawn => Some (sqBack sq player)
			    | _ => None
	else
	    None

fun isPawnMoveOrCapture pieces src dest =
    case (pieceAt2 pieces dest.X dest.Y) of
	None =>
	(case (pieceAt2 pieces src.X dest.Y) of
	    None => False
	  | Some p =>
	    (case (piece_to_kind p.Piece) of
		Pawn => True
	      | _ => False))
      | Some _ => True

fun doMove state move =
    case (testLegal state move) of
	True =>
	let
	    val src = move.Src
	    val dest = move.Dest
	    val requiresEnPassant = isPawnUp2Sq state.Pieces src dest
	    val piecesnew = removeFromAddAt state.Pieces src dest
	    val castled = isCastle state.Pieces src dest
	    val resetCounter = isPawnMoveOrCapture state.Pieces src dest
	    (* handle castling, rooks move places *)
	    val piecesnew2 = case castled of
			     | Some Kingside => removeFromAddAt piecesnew {X=7,Y=src.Y} {X=5,Y=src.Y}
			     | Some Queenside => removeFromAddAt piecesnew {X=0,Y=src.Y} {X=3,Y=src.Y}
			     | None => piecesnew
	    (* handle en passant, pawn disapper*)
	    val piecesnew3 = case (isEnPassantCapture state.EnPassant piecesnew2 state.Player dest) of
				 None => piecesnew2
			       | Some sq => removePSquare2 piecesnew2 sq.X sq.Y
	    (* handle promotion, pawns morphing into pieces *)
	    val piecesnew4 = case move.Prom of
				 None => piecesnew3
			       | Some k =>
				 {Piece=(mkPiece k state.Player),X=dest.X,Y=dest.Y} :: removePSquare2 piecesnew3 dest.X dest.Y
	    val newState = {
		Pieces = piecesnew4,
		Player = other state.Player,
		WK = if (state.WK) && (peq White state.Player) then
			 if (isKingMove White state.Pieces src) || (isRookKMove White state.Pieces src) then
			     False
			 else
			     True
		     else
			 state.WK,
		WQ =  if (state.WQ) && (peq White state.Player) then
			  if (isKingMove White state.Pieces src) || (isRookQMove White state.Pieces src) then
			      False
			  else
			      True
		      else
			  state.WQ,
		BK = if (state.BK) && (peq Black state.Player) then
			 if (isKingMove Black state.Pieces src) || (isRookKMove Black state.Pieces src) then
			     False
			 else
			     True
		     else
			 state.BK,
		BQ = if (state.BQ) && (peq Black state.Player) then
			 if (isKingMove Black state.Pieces src) || (isRookQMove Black state.Pieces src) then
			     False
			 else
			     True
		     else
			 state.BQ,
		EnPassant = if requiresEnPassant then
				case state.Player of
				    White => Some { X = dest.X, Y = dest.Y + 1}
				  | Black => Some { X = dest.X, Y = dest.Y - 1 }
			    else
				None,
		HalfMove = if resetCounter then
			       0
			   else
			       state.HalfMove + 1,
		FullMove = if (peq state.Player Black) then
			       state.FullMove + 1
			   else
			       state.FullMove
	    }
	in
	    if (isKingCapturable newState castled src) then
		None
	    else
		Some newState
	end
      | False => None

fun sqEq mSq sq =
    case mSq of
	None => False
      | Some sq' => sq'.X = sq.X && sq'.Y = sq.Y

fun isRegularCapture state move =
    case (pieceAt2 state.Pieces move.Dest.X move.Dest.Y, pieceAt2 state.Pieces move.Src.X move.Src.Y) of
	(Some p, Some p') =>
	not (peq (piece_to_player p.Piece) (piece_to_player p'.Piece))
      | (_, _) => False
	
(* TODO fix ambiguous moves *)

fun moveAlgWNbr (state:gamestate) withNbr =
    if withNbr || (peq White state.Player) then
	(show state.FullMove) ^ "." ^ (case state.Player of
					   Black => ".. "
					 | _ => " ")
    else
	""

fun attacks piecesAll player  =
    let
	fun f e =
	    peq player (piece_to_player e.Piece) (*  && keq (piece_to_kind e.Piece) Queen *)
	    
	val pieces = List.filter f piecesAll
	val legals = List.mp (legalsForPieceSL pieces) pieces
	val final = List.foldl List.append [] legals
    in
	final
    end

fun isOwnKingAttacked state =
    case (getKing state) of
	None => False
      | Some krec =>
	let
	    val moves = attacks state.Pieces (other state.Player)
	in
	    hasDest moves {X=krec.X,Y=krec.Y}
	end

fun moveToAlgebraic (state : gamestate) (move : move) (algeb : string) (withNbr : bool) = 
    (moveAlgWNbr state withNbr) ^ algeb

fun any [a] (ls : list a) =
    case ls of
	[] => False
      | _ :: _ => True

fun moveToAlgebraicClean (state : gamestate) (move : move) (nextstate: gamestate) = 
    let
	val prec = pieceAt2 state.Pieces move.Src.X move.Src.Y
	val regularX = isRegularCapture state move
	val isCheck = isOwnKingAttacked nextstate
	val anyLegals = any (allLegals nextstate)
	val checkSuffix = (if isCheck then
			       (if anyLegals then
				    "+"
				else
				    "#")
			   else
			       "")
    in 	
	(case prec of
	     None => ""
	   | Some p =>
	     case (piece_to_kind p.Piece) of
		 Pawn =>
		 (*check if there was capture. otherwise just output the dest sq*)
		 if (sqEq state.EnPassant move.Dest) then
		     (fileStr move.Src.X) ^ "x" ^ (sqStr move.Dest) ^ "e.p." ^ checkSuffix
		 else
		     if regularX then
			 (fileStr move.Src.X) ^ "x" ^ (sqStr move.Dest) ^ checkSuffix
		     else
			 (sqStr move.Dest) ^ checkSuffix
	       | Knight =>
		 "N" ^ (if regularX then
			    "x"
			else
			    "") ^ (sqStr move.Dest) ^ checkSuffix
	       | Bishop =>
		 "B" ^ (if regularX then
			    "x"
			else
			    "") ^ (sqStr move.Dest) ^ checkSuffix
	       | Rook =>
		 "R" ^ (if regularX then
			    "x"
			else
			    "") ^ (sqStr move.Dest) ^ checkSuffix
	       | Queen =>
		 "Q" ^ (if regularX then
			    "x"
			else
			    "") ^ (sqStr move.Dest) ^ checkSuffix
	       | King =>
		 (case (isCastle state.Pieces move.Src move.Dest) of
		      Some Kingside => "O-O" ^ checkSuffix
		    | Some Queenside => "O-O-O" ^ checkSuffix
		    | None => 
		      "K" ^ (if regularX then
				 "x"
			     else
				 "") ^ (sqStr move.Dest) ^ checkSuffix)
	       | _  => moveStr move)
    end

