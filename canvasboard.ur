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

type position = { Id: int, State: gamestate,
		  Highlight: list square }
		
datatype boardmsg =
	 Highlight of square
       | Position of position
		     
datatype serverboardmsg =
	 SMovePiece of square * square * option kind
       | SHighlight of square
       | SBack 
       | SForward
       | SPosition of int
		      
type boardInterface = {
     GetTree : unit -> transaction pgnRoot,
     GetTreeI : unit -> transaction pgnRoot,
     StartRender: unit -> option boardstate,
     ListenerLoop: source pgnRoot -> source (option boardstate) -> transaction unit,
     Speak: serverboardmsg -> transaction unit     
}


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
     RenderState: source (option boardstate),
     HandleMsg: boardmsg -> transaction unit
}

		      
		   
type boardSpec = {
     Id: id,
     Size: int,
     OffProm : int,
     CanvasW: int,
     CanvasH: int,
     CanMakeMoves: bool,
     LoadGraphics : unit -> transaction graphicsCtx,
     MouseMove : mouseEvent -> transaction unit,
     MouseDown : mouseEvent -> transaction unit,
     MouseUp : mouseEvent -> transaction unit,
     PgnState : source pgnRoot
}
		 

val testFen = "rnbqkbnr/ppp1ppp1/7p/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6"

fun identInterface fen =
    let
	fun get () =
	    return (Root (0, fen, []))
	and stRend () =
	    Some (fen_to_board fen)
	and listLoop _ _ =
	    return ()
	and speak _ =
	    return ()
    in
	{
	 GetTree = get,
	 GetTreeI = get,
	 StartRender = stRend,
	 ListenerLoop = listLoop,
	 Speak = speak	 
	}
    end

fun bSpec id size mmoves interf =
    tree <- interf.GetTreeI ();
    renderstate <- source None;
    mousestate <- source {RawX=0,RawY=0};
    pgnstate <- source tree;
    
    let
	val offProm = 2
	val canvasH = size * 8
	val canvasW = if mmoves then
			  size * 9 + offProm
		      else
			  canvasH

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
		     
	and mousedown e =
	    p' <- get renderstate;
	    case p' of
		Some p'' => 
		let
		    val sqX = clampToBoardCoordinateX e.OffsetX
		    val sqY = clampToBoardCoordinateY e.OffsetY
		    val f' = (pieceInSquare sqX sqY)
		    val maybepiecer = List.find f' p''.Pieces
		in
		    (* if we hit a square with a piece, grab that piece *)
		    case maybepiecer of
			None => return ()
		      | Some piecer =>
			let		
			    val st : boardstate = {
				Highlight = [], 
				Pieces = (removePSquare p''.Pieces f'),
				Full = p''.Full,
				DragPiece = Some {
				Src = { RawX = e.OffsetX,
					RawY = e.OffsetY
				      },
				Current = { RawX = e.OffsetX,
					    RawY = e.OffsetY
					  },
				Piece = piecer.Piece
				},
				Prom = None}
			in
			    set renderstate (Some st);
			    return ()
			end	
		end
	      | None => return ()

	and handlePseudoLegalMoveUI (rstate : boardstate) (move : move) =
	    case (doMove rstate.Full move) of
		None =>
		let
		    val st = {Highlight = rstate.Highlight,
			      Pieces = rstate.Full.Pieces,
			      Full = rstate.Full,
			      DragPiece = None,
			      Prom = None}
		in
		    set renderstate (Some st);
		    return ()
		end
	      | Some newState =>
		let
		    val st = {Highlight = [],
			      Pieces = newState.Pieces,
			      Full = newState,
			      DragPiece = None,
			      Prom = None}
		in
		    set renderstate (Some st);
		    interf.Speak (SMovePiece (move.Src, move.Dest, move.Prom));
		    return ()
		end
		
	and mouseup e =
	    p' <- get renderstate;
	    case p' of
		Some p'' =>
		(case p''.DragPiece of
		     None =>
		     (case p''.Prom of
			  None => return ()
			| Some prom' => 
			  
			  (* detect click in promotion area *)
			  (case (clampToPromSq e.OffsetX e.OffsetY) of
			       None => return ()
			     | Some p =>
			       handlePseudoLegalMoveUI p'' {Src=prom'.Src, Dest = prom'.Dest, Prom=Some p}))
		     
		   | Some d =>
		     let
			 val sqX = clampToBoardCoordinateX e.OffsetX
			 val sqY = clampToBoardCoordinateY e.OffsetY

			 val srcX = clampToBoardCoordinateX d.Src.RawX
			 val srcY = clampToBoardCoordinateY d.Src.RawY

		     in
			 if (requiresPromotionSq p''.Full.Pieces srcX srcY sqX sqY) then
			     let
				 val st = {Highlight = p''.Highlight,
					   Pieces = p''.Full.Pieces,
					   Full = p''.Full,
					   DragPiece = None,
					   Prom = Some {Src={X=srcX,Y=srcY},Dest={X=sqX,Y=sqY}}}
			     in
				 set renderstate (Some st);
				 return ()
			     end
			 else
			     handlePseudoLegalMoveUI p'' {Src={X=srcX,Y=srcY}, Dest = {X=sqX,Y=sqY}, Prom=None}
		     end)
	      | None => return ()
			

	and mousemove e =
	    p' <- get renderstate;
	    (case p' of
		 None => return ()
	       | Some p'' =>
		 case p''.DragPiece of
		     None => return ()			
		   | Some d => 		    
		     let
			 val st : boardstate = {
			     Highlight = p''.Highlight,
			     Pieces = p''.Pieces,
			     Full = p''.Full,
			     DragPiece = Some {
			     Src = d.Src,
			     Current = {
			     RawX = e.OffsetX,
			     RawY = e.OffsetY
			     },
			     Piece = d.Piece
			     },
			     Prom = None}
		     in
			 set renderstate (Some st);
			 return ()
		     end);
	    set mousestate {RawX = e.OffsetX, RawY = e.OffsetY}
	    			  
	and loadGraphics () =
	   
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
	    
	    ctx <- getContext2d id;
	    
	    let
		fun handle_boardmsg s =
		    case s of			   
			Highlight(sq) => 
			(s' <- get renderstate;
			 case s' of
		      | Some s'' =>
			set renderstate (Some {
					 Highlight = sq :: [],
					 Pieces = s''.Pieces,
					 Full = s''.Full,
					 DragPiece = s''.DragPiece,
					 Prom = None
					})
		      | None => return ())
		      | Position(p) => 
			(s' <- get renderstate;		
			 case s' of
			| Some s'' =>
			  set renderstate (Some {
					   Highlight = [],
					   Pieces = p.State.Pieces,
					   Full = p.State,
					   DragPiece = None,
					   Prom = None
					  });
			  x <- interf.GetTree ();
			  set pgnstate x
			| None => return ())
			
		     	     
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
			clearRect ctx (float 0) (float 0) (float canvasW) (float canvasH);
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
		set renderstate (interf.StartRender ());
		
		requestAnimationFrame2 drawBoard3;

		return {Id = id,
			Bk = bk, Bq = bq, Br = br, Bb = bb, Bn = bn, Bp = bp,
			Wk = wk, Wq = wq, Wr = wr, Wb = wb, Wn = wn, Wp = wp,
			Ctx = ctx, RenderState = renderstate, HandleMsg = handle_boardmsg }
	    end
	    
    in
	return {Id= id, Size = size, OffProm = offProm,
		CanvasW = canvasW, CanvasH = canvasH,
		CanMakeMoves = mmoves, LoadGraphics = loadGraphics,
		MouseMove = mousemove, MouseUp = mouseup,
		MouseDown = mousedown, PgnState = pgnstate }
    end

    
fun generateBoard spec =
    <xml>
      <canvas id={spec.Id} width={spec.CanvasW} height={spec.CanvasH} onmousemove={spec.MouseMove} onmousedown={spec.MouseDown} onmouseup={spec.MouseUp}>	
      </canvas>
    </xml>

(*
fun generateViewer spec interf =
    <xml>
      <dyn signal={m <- signal spec.PgnState; interf.RenderPgn m } />
    </xml>
*)
