open Canvas_FFI
open Chess

val light = make_rgba 239 238 240 1.0
val dark = make_rgba 119 138 181 1.0
val red = make_rgba 255 0 0 1.0
val promBg = make_rgba 244 244 244 1.0
val promBgSel = make_rgba 211 211 211 1.0
     
type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }

type promstate = { Src: square, Dest: square }
	      
type boardstate = { Highlight: list square, Pieces: list piecerec, DragPiece: option draggingPiece,
		    Full : gamestate, Prom: option promstate  }


fun state_to_board state =
    { Highlight = [], Full = state, Pieces=state.Pieces, DragPiece = None, Prom = None}
	
fun fen_to_board fen =
    let
	val state = fen_to_state fen
    in
	state_to_board state
    end

     
type graphicsCtx = {
     Id : id,
     Bk : img,
     Bq : img,
     Br : img,
     Bb : img,
     Bn : img,
     Bp : img,
     Wk : img,
     Wq : img,
     Wr : img,
     Wb : img,
     Wn : img,
     Wp : img,
     Ctx: canvas2d,
     RenderState: source (option boardstate)
}

type boardSpec = {
     Id: id,
     Size: int,
     OffProm : int,
     CanvasW: int,
     CanvasH: int,
     CanMakeMoves: bool
}

fun bSpec id size mmoves =
    let
	val offProm = 2
	val canvasH = size * 8
	val canvasW = if mmoves then
			  size * 9 + offProm
		      else
			  canvasH
	      
    in
	{Id= id, Size= size, OffProm = offProm, CanvasW = canvasW, CanvasH = canvasH, CanMakeMoves = mmoves }
    end

val testFen = "rnbqkbnr/ppp1ppp1/7p/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6"

fun loadGraphics spec =    
    renderstate <- source None;
    mousestate <- source {RawX=0,RawY=0};
    
    bk <- make_img(bless("/BlackKing.png"));
    bq <- make_img(bless("/BlackQueen.png"));
    br <- make_img(bless("/BlackRook.png"));
    bb <- make_img(bless("/BlackBishop.png"));
    bn <- make_img(bless("/BlackKnight.png"));
    bp <- make_img(bless("/BlackPawn.png"));
    
    
    wk <- make_img(bless("/WhiteKing.png"));
    wq <- make_img(bless("/WhiteQueen.png"));
    wr <- make_img(bless("/WhiteRook.png"));
    wb <- make_img(bless("/WhiteBishop.png"));
    wn <- make_img(bless("/WhiteKnight.png"));
    wp <- make_img(bless("/WhitePawn.png"));
    
    ctx <- getContext2d spec.Id;
    
    let
	val size = spec.Size
	val offProm = spec.OffProm
		      
	fun clampToPromSq rawX rawY =
	    if rawX >= (size * 8) + offProm && rawX <= (size * 9) + offProm then		    
		yPromToKind (clampToBoardCoordinateY rawY)
	    else
		None

	and insideQuad rawX rawY srcX srcY size =
	    rawX >= srcX && rawX <= (srcX + size) && rawY >= srcY && rawY <= (srcY + size)
	
	and clampToBoardCoordinateX rawX =
	    trunc (float(rawX) / float(size))

	and clampToBoardCoordinateY rawY =
	    trunc (float(rawY) / float(size))
	    
	and yPromToKind y =
	    case y of
		0 => Some Queen
	      | 7 => Some Queen
	      | 1 => Some Rook
	      | 6 => Some Rook
	      | 2 => Some Bishop
	      | 5 => Some Bishop
	      | 3 => Some Knight
	      | 4 => Some Knight
	      | _ => None
		     		
	and paint_row0 ctx row =	    
	    fillRect ctx 0 (row * size) size size;
	    fillRect ctx (size * 2) (row * size) size size;
	    fillRect ctx (size * 4) (row * size) size size;
	    fillRect ctx (size * 6) (row * size) size size
	    
	and paint_row1 ctx row =	    
	    fillRect ctx (size) (row * size) size size;
	    fillRect ctx (size * 2 + size) (row * size) size size;
	    fillRect ctx (size * 4 + size) (row * size) size size;
	    fillRect ctx (size * 6 + size) (row * size) size size

	and paint_prom_sq ctx row piece ms =
	    (if (insideQuad ms.RawX ms.RawY (size * 8 + offProm) (row * size) size) then			    
		 setFillStyle ctx promBgSel
	     else
		 setFillStyle ctx promBg); 
	    fillRect ctx (size * 8 + offProm) (row * size) size size;
	    draw_piece_dl ctx piece (float (size * 8 + offProm)) (float (row * size))

	and paint_prom ctx pr x' =
	    case pr of
		Some p =>
		if p.Dest.Y = 0 then
		    (paint_prom_sq ctx 0 WhiteQueen x';
		     paint_prom_sq ctx 1 WhiteRook x';
		     paint_prom_sq ctx 2 WhiteBishop x';
		     paint_prom_sq ctx 3 WhiteKnight x')
		else
		    (paint_prom_sq ctx 4 BlackKnight x';
		     paint_prom_sq ctx 5 BlackBishop x';
		     paint_prom_sq ctx 6 BlackRook x';
		     paint_prom_sq ctx 7 BlackQueen x')
	      | None => return ()
			
	and piece_to_id piece =
	    case piece of
		WhiteKing => wk
	      | WhiteQueen => wq
	      | WhiteRook => wr
	      | WhiteBishop => wb
	      | WhiteKnight => wn
	      | WhitePawn => wp
	      | BlackKing => bk
	      | BlackQueen => bq
	      | BlackRook => br
	      | BlackBishop => bb
	      | BlackKnight => bn
	      | BlackPawn => bp

	and draw_piece_dl ctx piece x y =
	    drawImage2 ctx (piece_to_id piece) x y (float size) (float size)
	    
	and draw_piece ctx (p : piecerec) =		    
	    (*	drawImage2 ctx (piece_to_id p.Piece) (float (size * p.X)) (float (size * p.Y)) (float size) (float size) *)
	    draw_piece_dl ctx p.Piece (float (size * p.X)) (float (size * p.Y))
	    
	and draw_pieces ctx ps =
	    case ps of
		h :: rest =>
		draw_piece ctx h;
		draw_pieces ctx rest
	      | _ => return ()

	and drawHighlight ctx (h : square) =
	    fillRect ctx (size * h.X) (size * h.Y) size size
	    
	and drawHighlights ctx hs =
	    case hs of
		h :: rest =>
		drawHighlight ctx h;
		drawHighlights ctx rest
	      | _ => return ()

	and draw_piecedrag ctx pd =
	    case pd of
		Some pd' =>
		drawImage2 ctx (piece_to_id pd'.Piece) (float(pd'.Current.RawX) - (float(size) / 2.0)) (float(pd'.Current.RawY) - (float(size) / 2.0)) (float size) (float size)
	      | _ => return ()
		     
	and drawBoard ctx x x' =
	    let
		val hs = x.Highlight
		val ps = x.Pieces
		val pd = x.DragPiece
		val pr = x.Prom
	    in
		clearRect ctx (float 0) (float 0) (float spec.CanvasW) (float spec.CanvasH);
		setFillStyle ctx light;
		paint_row0 ctx 0;
		paint_row1 ctx 1;
		paint_row0 ctx 2;
		paint_row1 ctx 3;
		paint_row0 ctx 4;
		paint_row1 ctx 5;
		paint_row0 ctx 6;
		paint_row1 ctx 7;
		
		setFillStyle ctx dark;
		paint_row1 ctx 0;
		paint_row0 ctx 1;
		paint_row1 ctx 2;
		paint_row0 ctx 3;
		paint_row1 ctx 4;
		paint_row0 ctx 5;
		paint_row1 ctx 6;
		paint_row0 ctx 7;
		
		paint_prom ctx pr x';
		(* TODO otherwise just clear this section? *)

		setFillStyle ctx red;
		drawHighlights ctx hs;

		draw_pieces ctx ps;

		draw_piecedrag ctx pd;
		
		return ()
	    end

	(* TODO arrows *)
	and drawBoard2 ctx x x'=
	    drawBoard ctx x x'

	and drawBoard3 () =
	    x2 <- get renderstate;
	    x3 <- get mousestate;
	    case x2  of
		Some x => 
		drawBoard2 ctx x x3
	      | _ => return ()

	and drawBoard4 () =
	    drawBoard3 ();
	    setTimeout drawBoard4 30
    
    in
	requestAnimationFrame2 drawBoard3;
	
	return {Id = spec.Id,
		Bk = bk, Bq = bq, Br = br, Bb = bb, Bn = bn, Bp = bp,
		Wk = wk, Wq = wq, Wr = wr, Wb = wb, Wn = wn, Wp = wp,
		Ctx = ctx, RenderState = renderstate
	       }
    end
	    

fun generateBoard spec = 
    <xml>
      <canvas id={spec.Id} width={spec.CanvasW} height={spec.CanvasH}>	
      </canvas>
    </xml>

    
