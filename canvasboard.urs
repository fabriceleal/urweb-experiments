type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: Chess.piece }

type promstate = { Src: Chess.square, Dest: Chess.square }
	      
type boardstate = { Highlight: list Chess.square, Pieces: list Chess.piecerec, DragPiece: option draggingPiece,
		    Full : Chess.gamestate, Prom: option promstate  }

type position = { Id: int, State: Chess.gamestate,
		  Highlight: list Chess.square } 

datatype boardmsg =
	 Highlight of Chess.square
       | Position of position

datatype serverboardmsg =
	 SMovePiece of Chess.square * Chess.square * option Chess.kind
       | SHighlight of Chess.square
       | SBack 
       | SForward
       | SPosition of int
	
type graphicsCtx = {
     Id : id,
     Bk : Canvas_FFI.img,
     Bq : Canvas_FFI.img,
     Br : Canvas_FFI.img,
     Bb : Canvas_FFI.img,
     Bn : Canvas_FFI.img,
     Bp : Canvas_FFI.img,
     Wk : Canvas_FFI.img,
     Wq : Canvas_FFI.img,
     Wr : Canvas_FFI.img,
     Wb : Canvas_FFI.img,
     Wn : Canvas_FFI.img,
     Wp : Canvas_FFI.img,
     Ctx: Canvas_FFI.canvas2d,
     RenderState: source (option boardstate)
}

type boardSpec = {
     Id : id,
     Size : int,
     OffProm : int,
     CanvasW : int,
     CanvasH : int,
     CanMakeMoves : bool,
     LoadGraphics: unit -> transaction graphicsCtx,
     MouseMove: mouseEvent -> transaction unit,
     MouseUp: mouseEvent -> transaction unit,
     MouseDown: mouseEvent -> transaction unit,
     PgnState : source Chess.pgnRoot
}

type boardInterface = {
     GetTree : unit -> transaction Chess.pgnRoot,
     StartRender: unit -> option boardstate,
     ListenerLoop: source Chess.pgnRoot -> source (option boardstate) -> transaction unit,
     Speak: serverboardmsg -> transaction unit
}

val identInterface : string -> boardInterface
		      
val bSpec : id -> int -> bool -> boardInterface -> transaction boardSpec
val generateBoard : boardSpec -> xml ([Body = (), Dyn = (), MakeForm = ()]) ([]) ([])
(*
val generateViewer : boardSpec -> boardInterface -> xml ([Body = (), Dyn = (), MakeForm = ()]) ([]) ([])
*)
				 
val fen_to_board : string -> boardstate

			     
