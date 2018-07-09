
open Canvas_FFI
	       
val light = make_rgba 239 238 240 1.0
val dark = make_rgba 119 138 181 1.0
val red = make_rgba 255 0 0 1.0
val size = 60
val x = 10
val y = 10

datatype piece = WhiteKing | WhiteQueen | WhiteRook | WhiteBishop | WhiteKnight | WhitePawn |
	 BlackKing | BlackQueen | BlackRook | BlackBishop | BlackKnight | BlackPawn
	      
type piecerec = { X: int, Y : int, Piece : piece  }

type square = { X: int, Y : int}

type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }
	      
type boardstate = { Highlight: option square, Pieces: list piecerec, DragPiece: option draggingPiece }
  
val pieces : list piecerec =
    { X= 0, Y= 0, Piece= BlackRook}  ::
				     { X= 1, Y= 0, Piece= BlackKnight} ::
				     { X= 2, Y= 0, Piece= BlackBishop} ::
				     { X= 3, Y= 0, Piece= BlackQueen} ::
				     { X= 4, Y= 0, Piece= BlackKing} ::
				     { X= 5, Y= 0, Piece= BlackBishop}  ::
				     { X= 6, Y= 0, Piece= BlackKnight} ::
				     { X= 7, Y= 0, Piece= BlackRook} ::
				     { X= 0, Y= 1, Piece= BlackPawn} ::
				     { X= 1, Y= 1, Piece= BlackPawn} ::
				     { X= 2, Y= 1, Piece= BlackPawn} ::
				     { X= 3, Y= 1, Piece= BlackPawn} ::
				     { X= 4, Y= 1, Piece= BlackPawn} ::
				     { X= 5, Y= 1, Piece= BlackPawn} ::
				     { X= 6, Y= 1, Piece= BlackPawn} ::
				     { X= 7, Y= 1, Piece= BlackPawn} ::
				     { X= 0, Y= 6, Piece= WhitePawn} ::
				     { X= 1, Y= 6, Piece= WhitePawn} ::
				     { X= 2, Y= 6, Piece= WhitePawn} ::
				     { X= 3, Y= 6, Piece= WhitePawn} ::
				     { X= 4, Y= 6, Piece= WhitePawn} ::
				     { X= 5, Y= 6, Piece= WhitePawn} ::
				     { X= 6, Y= 6, Piece= WhitePawn} ::
				     { X= 7, Y= 6, Piece= WhitePawn} ::
				     { X= 0, Y= 7, Piece= WhiteRook}  ::
				     { X= 1, Y= 7, Piece= WhiteKnight} ::
				     { X= 2, Y= 7, Piece= WhiteBishop} ::
				     { X= 3, Y= 7, Piece= WhiteQueen} ::
				     { X= 4, Y= 7, Piece= WhiteKing} ::
				     { X= 5, Y= 7, Piece= WhiteBishop}  ::
				     { X= 6, Y= 7, Piece= WhiteKnight} ::
				     { X= 7, Y= 7, Piece= WhiteRook} :: []
    
fun main () =	
    bk <- fresh; bq <- fresh; br <- fresh; bb <- fresh; bn <- fresh; bp <- fresh;
    wk <- fresh; wq <- fresh; wr <- fresh; wb <- fresh; wn <- fresh; wp <- fresh;

    
    c <- fresh;
    p <- source None;
    
    let
	
	fun paint_row0 ctx row =	    
	    fillRect ctx 0 (row * size) size size;
	    fillRect ctx (size * 2) (row * size) size size;
	    fillRect ctx (size * 4) (row * size) size size;
	    fillRect ctx (size * 6) (row * size) size size

	and renderCanvas sg =	    
	    x2 <- signal sg;
	    case x2 of
		Some x => 
		return <xml>
		  <active code={
		drawBoard2 (case x.Highlight of
				Some t => t :: []
			      | _ => []) x.Pieces x.DragPiece;
		return <xml></xml>}>
		  </active> 
		</xml>
	      | None => return <xml><div></div></xml>
(**)
	and paint_row1 ctx row =	    
	    fillRect ctx (size) (row * size) size size;
	    fillRect ctx (size * 2 + size) (row * size) size size;
	    fillRect ctx (size * 4 + size) (row * size) size size;
	    fillRect ctx (size * 6 + size) (row * size) size size

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

	and draw_piece ctx (p : piecerec)  =
	    drawImage ctx (piece_to_id p.Piece) 0 0 80 80 (size * p.X) (size * p.Y) size size
	    
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
		drawImage ctx (piece_to_id pd'.Piece) 0 0 80 80 (pd'.Current.RawX - (trunc (float(size) / 2.0))) (pd'.Current.RawY - (trunc (float(size) / 2.0))) size size
	      | _ => return ()
		     
	and drawBoard ctx hs ps pd =
	    
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

	    setFillStyle ctx red;
	    drawHighlights ctx hs;

	    draw_pieces ctx ps;

	    draw_piecedrag ctx pd;
	    
	    return ()

	(* TODO arrows *)
	and drawBoard2 hs ps db =
	    ctx <- getContext2d c;
	    drawBoard ctx hs ps db

	and drawBoard3 () =
	    x2 <- get p;
	    case x2 of
		Some x => 
		drawBoard2 (case x.Highlight of
				Some t => t :: []
			      | _ => []) x.Pieces x.DragPiece
	      | _ => return ()

	and drawBoard4 () =
	    drawBoard3 ();
	    setTimeout drawBoard4 30
	    
	and clampToBoardCoordinateX rawX =
	    trunc (float(rawX) / float(size))

	and clampToBoardCoordinateY rawY =
	    trunc (float(rawY) / float(size))
(**)
	and pieceInSquare (x : int) (y : int) =
	    let
		fun tmp (pp : piecerec) =
		    pp.X = x && pp.Y = y
	    in
		tmp
	    end

	and removePSquare ls f =
	    case ls of
		h :: r =>
		if (f h) then
		    r
		else
		    h :: (removePSquare r f)		       
	      | [] => []
						   
	and mousedown e =
	    p' <- get p;
	    case p' of
		Some p'' => 
		let
		    val sqX = clampToBoardCoordinateX e.OffsetX
		    val sqY = clampToBoardCoordinateY e.OffsetY
		    val f' = (pieceInSquare sqX sqY)
		    val p''' = List.find f' p''.Pieces
		in
		  
		    case p''' of
			None => return ()
		      | Some p'''' =>
			let		
			    val st : boardstate = {Highlight = None, (* Some {X = 0, Y = 0}, *)
						   Pieces = (removePSquare p''.Pieces f'),
						   DragPiece = Some {
						   Src = { RawX = e.OffsetX,
							   RawY = e.OffsetY
							 },
						   Current = { RawX = e.OffsetX,
							       RawY = e.OffsetY
							     },
						   Piece = p''''.Piece
						  }}
			in
			    set p (Some st);
			    return ()
			end	
		end
	      | None => return ()
			
			
	and mouseup e =
	    p' <- get p;
	    case p' of
		Some p'' =>
		(case p''.DragPiece of
		    None => 		
		    let
			val st : boardstate = {Highlight = None,
					       Pieces = p''.Pieces,
					       DragPiece = None}
		    in
			set p (Some st);
			return ()
		    end
		  | Some d =>
		    let
			val sqX = clampToBoardCoordinateX e.OffsetX
			val sqY = clampToBoardCoordinateY e.OffsetY

				  (* TODO legal move validation, handle captures *)
			val st : boardstate = {Highlight = None,
					       Pieces = { Piece=d.Piece,X=sqX, Y=sqY } :: p''.Pieces,
					       DragPiece = None}
		    in
			set p (Some st);
			return ()
		    end	)
	      | None => return ()
	    

	and mousemove e =
	    p' <- get p;
	    case p' of
		None => return ()
	      | Some p'' =>
		case p''.DragPiece of
		    None =>
		    let
			val st : boardstate = {Highlight = Some {
					       X = clampToBoardCoordinateX e.OffsetX,
					       Y = clampToBoardCoordinateY e.OffsetY
					       },
					       Pieces = p''.Pieces,
					       DragPiece = None}
		    in
			set p (Some st);
			return ()
		    end
		  | Some d => 		    
		    let
			val st : boardstate = {Highlight = None,
					       Pieces = p''.Pieces,
					       DragPiece = Some {
					       Src = d.Src,
					       Current = {
					       RawX = e.OffsetX,
					       RawY = e.OffsetY
					       },
					       Piece = d.Piece
					       }}
		    in
			set p (Some st);
			return ()
		    end
	    
	and loadPage () =
	    giveFocus c;
	    ctx <- getContext2d c;
	    (*  *)
	    
	    (* drawBoard ctx [] pieces None; *)
	    set p (Some {Highlight = None, Pieces=pieces, DragPiece = None});
	    (*
	    requestAnimationFrame2 drawBoard3;
	     

	    drawBoard4 ();
	    *)
	    return ()
	    
    in
	
	return  <xml>
	  <head><title>Hello World</title></head>
	   <body onload={loadPage ()} >
	     <h1>hello world</h1>
	     
	     <img id={bk} alt="black king" height=80 width=80 src="/BlackKing.png" />
	     <img id={bq} alt="black queen" height=80 width=80 src="/BlackQueen.png" />
	     <img id={br} alt="black rook" height=80 width=80 src="/BlackRook.png" />
	     <img id={bb} alt="black bishop" height=80 width=80 src="/BlackBishop.png" />
	     <img id={bn} alt="black knight" height=80 width=80 src="/BlackKnight.png" />
	     <img id={bp} alt="black pawn" height=80 width=80 src="/BlackPawn.png" />
	     
	     <img id={wk} alt="white king" height=80 width=80 src="/WhiteKing.png" />
	     <img id={wq} alt="white queen" height=80 width=80 src="/WhiteQueen.png" />
	     <img id={wr} alt="white rook" height=80 width=80 src="/WhiteRook.png" />
	     <img id={wb} alt="white bishop" height=80 width=80 src="/WhiteBishop.png" />
	     <img id={wn} alt="white knight" height=80 width=80 src="/WhiteKnight.png" />
	     <img id={wp} alt="white pawn" height=80 width=80 src="/WhitePawn.png" />
	     
	     <a link={index()}>another page</a>

	     <div>
	       <dyn signal={renderCanvas p} />
	     </div>
(**)	     
	     <button value="click" onclick={fn _ => set p (Some {Highlight = Some {X = 0, Y = 0},
								 Pieces=pieces,
								 DragPiece = None}) } />
											       
	     <canvas id={c} width={size * 8} height={size * 8} onmousedown={mousedown} onmouseup={mouseup} onmousemove={mousemove} >
	     </canvas>
	 </body>
	 </xml>
     end
     
and index () = return <xml>
  <body>index
    <a link={main()}>new page</a></body></xml>

