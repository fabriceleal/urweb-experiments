
datatype piece = WhiteKing | WhiteQueen | WhiteRook | WhiteBishop | WhiteKnight | WhitePawn |
	 BlackKing | BlackQueen | BlackRook | BlackBishop | BlackKnight | BlackPawn

datatype kind = King | Queen | Rook | Bishop | Knight | Pawn

datatype player = White | Black

type piecerec = { X: int, Y : int, Piece : piece  }

type square = { X: int, Y : int}

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
		
(* functions *)

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
	"w" => Some White
      | "b" => Some Black
      | _ => None
		 
fun fen_to_state s =
    let
	fun fen_to_pieces_aux s row col =
	    let
		val l = strlen s
	    in
		if l = 0 then
		    []
		else
		    let
			val fst = (strsub s 0)
		    in
			if fst = #"/" then
			    fen_to_pieces_aux (substring s 1 (l -1)) (row + 1) 0
			else
			    if isdigit fst then
				case (read (show fst) : option int) of
				    Some i => fen_to_pieces_aux (substring s 1 (l -1)) row (col + i)
				  | None => [] (*trigger error ? *)
			    else
				case (char_to_piece fst) of
				    None => [] (*trigger error? *)
				  | Some p => {X = col, Y = row, Piece= p} :: (fen_to_pieces_aux (substring s 1 (l - 1)) row (col + 1))
		    end
	    end
	    
    in
	{
	 Pieces = fen_to_pieces_aux s 0 0,
	 Player = White,
	 WK = True,
	 WQ = True,
	 BK = True,
	 BQ = True,
	 EnPassant = None,
	 HalfMove = 0,
	 FullMove = 1
	}
    end


fun pieceInSquare x y =
    let
	fun tmp (pp : piecerec) =
	    pp.X = x && pp.Y = y
    in
	tmp
    end
    
fun pieceAt (ls : list piecerec) (f : piecerec -> bool) : option piecerec =
    case ls of
	h :: r =>
	if (f h) then
	    Some h
	else
	    pieceAt r f
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
    
fun state_to_fen state : string  =
    List.foldr strcat "" (List.append (pieces_to_fen state.Pieces) (" " :: (playerToFen state.Player :: [])))

fun validSq x y = x >= 0 && x < 8 && y >= 0 && y < 8

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
	     
(* is valid sq and is empty or has a foe *)
fun pemptyOrFoe pieces player x y =
    case (testSq pieces player x y) of
	Empty => True
      | Foe => True
      | _ => False
			   
(* TODO en passant and promotion *)
(* TODO remove self checks *)
fun legalsPawn pieces player src =
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
		 (if (pfoe pieces player (src.X - 1) (src.Y - 1)) then
		      { X=src.X - 1, Y = src.Y - 1 } :: []
		  else
		      [])
		 
		 (if (pfoe pieces player (src.X + 1) (src.Y - 1)) then
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
		 (if (pfoe pieces player (src.X - 1) (src.Y + 1)) then
		      { X=src.X - 1, Y = src.Y + 1 } :: []
		  else
		      [])
		 
		 (if (pfoe pieces player (src.X + 1) (src.Y + 1)) then
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


fun removeNones ls =
    case ls of
	h :: t =>
	(case h of
	     Some thing => thing :: removeNones t
	   | None => removeNones t)
      | [] => []

fun offTest pieces player src dX dY =
    let
	val test = {X=src.X + dX,Y=src.Y + dY}
    in
	if (pemptyOrFoe pieces player test.X test.Y) then
	    Some test
	else
	    None
    end
	
(* TODO remove self checks *)
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
    
(* TODO remove self checks *)
(* TODO castling *)
(* TODO track castling *)
fun legalsKing pieces player src =
    removeNones (offTest pieces player src (-1) (1) ::
			 offTest pieces player src (-1) (0) ::
			 offTest pieces player src (-1) (-1) ::
			 offTest pieces player src (1) (1) ::
			 offTest pieces player src (1) (0) ::
			 offTest pieces player src (1) (-1) ::
			 offTest pieces player src (0) (1) ::
			 offTest pieces player src (0) (-1) ::
			 [])
        
(* TODO remove self checks *)
fun legalsDiagonals pieces player src =
    List.append 
	(List.append
	     (genSlide pieces player src (-1) (-1))
	     (genSlide pieces player src 1 (-1)))
	(List.append
	     (genSlide pieces player src (-1) 1)
	     (genSlide pieces player src 1 1))

(* TODO in case of rook, track castling *)
(* TODO remove self checks *)
fun legalsOrtho pieces player src =
    List.append 
	(List.append
	     (genSlide pieces player src 0 (-1))
	     (genSlide pieces player src 0 1))
	(List.append
	     (genSlide pieces player src (-1) 0)
	     (genSlide pieces player src 1 0))

(* TODO remove self checks *)
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

(* test if a move is legal *)
fun testLegal state src dest =
    let
	val pieces = state.Pieces
	val maybepiece = pieceAt2 pieces src.X src.Y
    in
	case maybepiece of
	    Some piece =>
	    (case (piece_to_kind piece.Piece) of
		 Pawn => hasDest (legalsPawn pieces (piece_to_player piece.Piece) src) dest
	       | Bishop => hasDest (legalsDiagonals pieces (piece_to_player piece.Piece) src) dest
	       | Rook => hasDest (legalsOrtho pieces (piece_to_player piece.Piece) src) dest
	       | Queen => hasDest (legalsSlide pieces (piece_to_player piece.Piece) src) dest
	       | Knight => hasDest (legalsKnight pieces (piece_to_player piece.Piece) src) dest
	       | King => hasDest (legalsKing pieces (piece_to_player piece.Piece) src) dest)
	  | None => False
    end

fun doMove state src dest =
    case (testLegal state src dest) of
	True => Some {
		Pieces = (removeFromAddAt state.Pieces src dest),
		Player = other state.Player,
		WK = state.WK,
		WQ = state.WQ,
		BK = state.BK,
		BQ = state.BQ,
		EnPassant = None,
		HalfMove = state.HalfMove + 1,
		FullMove = state.FullMove + 1
		}
      | False => None
		
