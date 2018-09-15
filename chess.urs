
datatype piece = WhiteKing | WhiteQueen | WhiteRook | WhiteBishop | WhiteKnight | WhitePawn |
	 BlackKing | BlackQueen | BlackRook | BlackBishop | BlackKnight | BlackPawn

datatype kind = King | Queen | Rook | Bishop | Knight | Pawn

type piecerec = { X: int, Y : int, Piece : piece  }

type square = { X: int, Y : int}
	      
type move = { Src: square, Dest: square, Prom: option kind}
	      
datatype player = White | Black

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
val piece_to_char : piece -> char
val char_to_piece : char -> option piece
			    
val fen_to_state : string -> gamestate
val state_to_fen : gamestate -> string
			

val pieceInSquare : int -> int -> (piecerec -> bool)
(*val pieceAt : list piecerec -> (piecerec -> bool) -> option piecerec*)

val pieceAt2 : list piecerec -> int -> int -> option piecerec
val removePSquare : list piecerec -> (piecerec -> bool) -> list piecerec

val removePSquare2 : list piecerec -> int -> int -> list piecerec
							   
val removeFromAddAt : list piecerec -> square -> square -> list piecerec
	      
val testLegal : gamestate -> move -> bool						   

val doMove : gamestate -> move -> option gamestate
					      
val kind_to_str : kind -> string

val moveStr : move -> string
			  
val requiresPromotionSq : list piecerec -> int -> int -> int -> int -> bool

val moveToAlgebraic : gamestate -> move -> string -> bool -> string
val moveToAlgebraicClean : gamestate -> move -> gamestate -> string

val str_to_move : string -> move

val any : a ::: Type -> list a -> bool

val isOwnKingAttacked : gamestate -> bool
				     
val attacks : list piecerec -> player -> list square

val other : player -> player

val sqStr : square -> string

val playerStr : player -> string
			  
