
type lsHeaders = list (string * string)
		 
datatype pgnTree =
	 Node of int * string * string * string * (list string) * (list pgnTree)
	 
datatype pgnRoot =
	 Root of int * string * list pgnTree * lsHeaders

val getH : lsHeaders -> string -> string

val show_pgn_tree : show pgnTree

val show_pgn_root : show pgnRoot
		 
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

val startingFen : string
		 
(* functions *)									  
val piece_to_char : piece -> char
val char_to_piece : char -> option piece
			    
val fen_to_state : string -> gamestate
val state_to_fen : gamestate -> string

val fileToI : char -> int
val rankToI : char -> int

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
			  
val pawnAlgebraicToMove :  gamestate -> string -> option move
val pieceAlgebraicToMove :  gamestate -> string -> option move						  
val castleAlgebraicToMove :  gamestate -> string -> option move
val pieceDesambAlgebraicToMove :  gamestate -> string -> option move
						    
						   


type position = { Id: int, Previous : int, State: gamestate, Old: gamestate, Move : string, MoveAlg: string, Highlight: list square }
		
datatype chessboardmsg =
	 MHighlight of square
       | MPosition of position
       | MComment of string
       | MChangeName of string
			
type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }

type promstate = { Src: square, Dest: square }
	      
type boardstate = { Highlight: list square, Pieces: list piecerec, DragPiece: option draggingPiece,
		    Full : gamestate, Prom: option promstate  }

		  
datatype serverchessboardmsg =
	 SMovePiece of square * square * option kind
       | SHighlight of square
       | SBack 
       | SForward
       | SPosition of int
       | SComment of string
       | SNewPost of option int * string
       | SChangeName of int * string

val emptyTopLevelHandler : chessboardmsg -> transaction unit

val emptyTree : unit -> transaction pgnRoot

val emptyOnGameState : gamestate -> transaction unit

val generate_board : string ->
		     id ->
		     int ->
		     bool ->
		     (unit -> transaction pgnRoot) ->
		     (unit -> transaction (list string)) ->
		     (serverchessboardmsg -> transaction unit) ->
		     (chessboardmsg -> transaction unit) ->
		     (gamestate -> transaction unit) ->
		     option (channel chessboardmsg) ->
		     transaction (xbody * xbody * xbody * (unit -> transaction (option gamestate)))

val state_to_board : gamestate -> boardstate

val fen_to_board : string -> boardstate

datatype mutableTree =
	 Move of {Id:int, Move: string, MoveAlg: string, Position: gamestate, Children: source (list mutableTree)}
		 
datatype mutableTreeRoot =
	 StartP of {Id:int, Position:gamestate, Children: source (list mutableTree) }

val treeToMtree : pgnRoot -> transaction mutableTreeRoot
			     
