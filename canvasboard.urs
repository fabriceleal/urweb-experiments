type piecerec = Chess.piecerec
datatype piece = datatype Chess.piece
datatype kind = datatype Chess.kind
datatype pgnRoot = datatype Chess.pgnRoot
type square = Chess.square
type gamestate = Chess.gamestate

type position = { Id: int, State: gamestate, Highlight: list square }
		
datatype boardmsg =
	 Highlight of square
       | Position of position
       | Comment of string
		 
type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }

type promstate = { Src: square, Dest: square }
	      
type boardstate = { Highlight: list square, Pieces: list piecerec, DragPiece: option draggingPiece,
		    Full : gamestate, Prom: option promstate  }

		  
datatype serverboardmsg =
	 SMovePiece of square * square * option kind
       | SHighlight of square
       | SBack 
       | SForward
       | SPosition of int
       | SComment of string
       | SNewPost of option int * string
		      
val generate_board : string -> id -> int ->
		     (unit -> transaction pgnRoot) ->
		     (unit -> transaction (list string)) ->
		     (serverboardmsg -> transaction unit) ->
		     channel boardmsg ->
		     transaction (xbody * xbody * xbody)

val state_to_board : gamestate -> boardstate

val fen_to_board : string -> boardstate
			     

