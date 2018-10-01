
open Canvas_FFI

type renderCtx = { BK : img, BQ : img, BR : img, BB : img, BN : img, BP : img,
		   WK : img, WQ : img, WR : img, WB : img, WN : img, WP : img,
		   C2D: canvas2d }

fun generate_board c =
    rctx <- source None;
    
    let
	
	fun mousemove e =
	    return ()

	and mouseup e =
	    return ()

	and mousedown e =
	    return ()

	and init () =

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
	    	    
	    ctx <- getContext2d c;

	    set rctx (Some {BK= bk, BQ= bq, BR = br, BB = bb, BN = bn, BP = bp,
			    WK= wk, WQ= wq, WR = wr, WB = wb, WN = wn, WP = wp,
			    C2D= ctx});
	    
	    debug "loaded";

	    let
		
	    in
		return <xml></xml>
	    end	    
	    

    in	
	return <xml>
	  <canvas id={c} onmousemove={mousemove} onmouseup={mouseup} onmousedown={mousedown}>      
	  </canvas>
	  <active code={init ()}>
	  </active>
	</xml>
    end
    
