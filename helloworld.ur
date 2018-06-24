
open Canvas_FFI
	       
val light = make_rgb 239 238 240
val dark = make_rgb 119 138 181
val size = 60
val x = 10
val y = 10

datatype piece = WhiteKing | WhiteQueen | WhiteRook | WhiteBishop | WhiteKnight | WhitePawn |
	 BlackKing | BlackQueen | BlackRook | BlackBishop | BlackKnight | BlackPawn
	      
type piecerec = { X: int, Y : int, Piece : piece  }
  
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
    
    let

	fun paint_row0 ctx row =	    
	    fillRect ctx 0 (row * size) size size;
	    fillRect ctx (size * 2) (row * size) size size;
	    fillRect ctx (size * 4) (row * size) size size;
	    fillRect ctx (size * 6) (row * size) size size

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
	    
	and draw_pieces ctx pieces =
	    case pieces of
		h :: rest =>
		draw_piece ctx h;
		draw_pieces ctx rest
	      | _ => return ()
		     
	and drawBoard ctx  =
	    
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

	    (* TODO highlighted squares *)
	    
	    draw_pieces ctx pieces

	    (* TODO arrows *)

	and mousedown e =
	    return ()

	and mouseup e =
	    return ()

	and mousemove e =
	    (*drawBoard ctx;*)

	    return ()
	    
	and loadPage () =
	    giveFocus c;
	    ctx <- getContext2d c;
	    
	    drawBoard ctx;
	    
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
	     
	     <canvas id={c} width={size * 8} height={size * 8} onmousedown={mousedown} onmouseup={mouseup} onmousemove={mousemove} >
	     </canvas>
	 </body>
	 </xml>
     end
     
and index () = return <xml>
  <body>index
    <a link={main()}>new page</a></body></xml>

