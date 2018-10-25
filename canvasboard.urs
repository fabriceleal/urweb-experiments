type piecerec = Chess.piecerec
datatype piece = datatype Chess.piece
datatype kind = datatype Chess.kind
datatype pgnRoot = datatype Chess.pgnRoot
type square = Chess.square
type gamestate = Chess.gamestate

type position = { Id: int, Previous : int, State: gamestate, Old: gamestate, Move : string, MoveAlg: string, Highlight: list square }
		
datatype boardmsg =
	 Highlight of square
       | Position of position
       | Comment of string
       | ChangeName of string
			
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
       | SChangeName of int * string

val emptyTopLevelHandler : boardmsg -> transaction unit

val emptyTree : unit -> transaction pgnRoot

val generate_board : string ->
		     id ->
		     int ->
		     bool ->
		     (unit -> transaction pgnRoot) ->
		     (unit -> transaction (list string)) ->
		     (serverboardmsg -> transaction unit) ->
		     (boardmsg -> transaction unit) ->
		     channel boardmsg ->
		     transaction (xbody * xbody * xbody)

val state_to_board : gamestate -> boardstate

val fen_to_board : string -> boardstate

datatype mutableTree =
	 Move of {Id:int, Move: string, MoveAlg: string, Position: gamestate, Children: source (list mutableTree)}
		 
datatype mutableTreeRoot =
	 StartP of {Id:int, Position:gamestate, Children: source (list mutableTree) }

val treeToMtree : pgnRoot -> transaction mutableTreeRoot
			     
