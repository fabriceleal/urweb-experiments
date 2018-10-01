type piecerec = Chess.piecerec
datatype piece = datatype Chess.piece
type square = Chess.square
type gamestate = Chess.gamestate

type position = { Id: int, State: gamestate, Highlight: list square }
		
datatype boardmsg =
	 Highlight of square
       | Position of position
		 
type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }

type promstate = { Src: square, Dest: square }
	      
type boardstate = { Highlight: list square, Pieces: list piecerec, DragPiece: option draggingPiece,
		    Full : gamestate, Prom: option promstate  }

val generate_board : id -> int -> transaction xbody

val state_to_board : gamestate -> boardstate

val fen_to_board : string -> boardstate
			     

