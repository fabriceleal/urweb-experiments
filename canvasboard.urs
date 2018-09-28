type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: Chess.piece }

type promstate = { Src: Chess.square, Dest: Chess.square }
	      
type boardstate = { Highlight: list Chess.square, Pieces: list Chess.piecerec, DragPiece: option draggingPiece,
		    Full : Chess.gamestate, Prom: option promstate  }


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
     Ctx: Canvas_FFI.canvas2d
}

type boardSpec = {
     Id : id,
     Size : int,
     OffProm : int,
     CanvasW : int,
     CanvasH : int,
     CanMakeMoves : bool
}

val bSpec : id -> int -> bool -> boardSpec

val loadGraphics : boardSpec -> transaction graphicsCtx
		   
val generateBoard : boardSpec -> xml ([Body = (), Dyn = (), MakeForm = ()]) ([]) ([])
			   
val fen_to_board : string -> boardstate

			     
