open Canvas_FFI
open Chess
     
signature GAME = sig
    val name : string
    type position
    val startingPosition : unit -> position
    val editor : unit -> transaction xbody
end

			 
structure SChess : GAME = struct
    val name = "Chess"
    type position = Chess.gamestate
    fun startingPosition _ = Chess.fen_to_state Chess.startingFen
    fun editor _ = return <xml></xml>
end
		 
structure Weiqi : GAME = struct
    val name = "Weiqi"
    type position = { B: int }
    fun startingPosition _ = {B = 2}
    fun editor _ =
	c <- fresh;
	let
	    val stonehf = 13
	    val stonesz = stonehf * 2
	    val space = 27
	    val offs = stonesz
	    val lines = 18 (* actual lines - 1*)
	    val w = space * lines
	    val h = space * lines
	    val cw = w + (offs * 2)
	    val ch = h + (offs * 2)
		     
	    fun onloadFn () =
		ctx <- getContext2d c;
		wstone <- make_img(bless("/w.svg"));
		bstone <- make_img(bless("/b.svg"));
		let
		    fun drawHs () =
			let
			    fun drawH n =
				drawLine ctx offs (offs + n * space) (w + offs) (offs + n * space);
				if n = 0 then
				    return ()
				else
				    drawH (n - 1)
				    
			in
			    drawH lines
			end

		    fun drawVs () =
			let
			    fun drawV n =
				drawLine ctx (offs + n * space) offs (offs + n * space) (h + offs);
				if n = 0 then
				    return ()
				else				
				    drawV (n - 1)
				    
			in
			    drawV lines
			end

		    fun drawMarker x y =		    
			beginPath ctx;
			arc ctx
			    (float ((x * space) + offs))
			    (float ((y * space) + offs))
			    4.0 0.0 (3.14 * 2.0) False;
			closePath ctx;
			fill ctx
			
		    fun drawStone img x y =
			drawImage2 ctx img
				   (float ((x * space) + offs - stonehf))
				   (float ((y * space) + offs - stonehf))
				   (float stonesz) (float stonesz)
			
		    fun drawBoard () =
			clearRect ctx (float 0) (float 0) (float cw) (float ch);
			setFillStyle ctx (make_rgba 212 172 89 1.0);
			fillRect ctx 0 0 cw ch;		    
			drawHs ();
			drawVs ();

			setFillStyle ctx (make_rgba 0 0 0 1.0);
			drawMarker 3 3;
			drawMarker 3 9;
			drawMarker 3 15;

			drawMarker 9 3;
			drawMarker 9 9;
			drawMarker 9 15;
			
			drawMarker 15 3;
			drawMarker 15 9;
			drawMarker 15 15;
			
			drawStone wstone 3 5;
			drawStone bstone 3 4;
			
			return ()
		in
		    requestAnimationFrame2 drawBoard;
		    return <xml></xml>
		end	    
	in	
	    return <xml>
	      <canvas id={c} width={cw} height={ch}>			 
	      </canvas>
	      <active code={onloadFn ()}>
	      </active>
	    </xml>
	end
end
			 
structure Shogi : GAME = struct
    val name = "Shogi"
    type position = { C: int }
    fun startingPosition _ = {C = 3}
    fun editor _ =
	c <- fresh;
	let
	    val xp = 40
	    val yp = 48

	    val offs = 6
		       
	    val xd = xp + (offs * 2)
	    val yd = yp + (offs * 2)
		     
	    val w = xd * 9
	    val h = yd * 9

	    val cw = w + 10
	    val ch = h + 10	    

	    fun onloadFn () =
		ctx <- getContext2d c;
		p <- make_img(bless("/fuhyo.svg"));
		b <- make_img(bless("/kakugyo.svg"));
		r <- make_img(bless("/hisha.svg"));
		k <- make_img(bless("/osho.svg"));
		kj <- make_img(bless("/gyokusho.svg"));
		pr <- make_img(bless("/hisha.svg"));
		pb <- make_img(bless("/ryuma.svg"));
		g <- make_img(bless("/kinsho.svg"));
		s <- make_img(bless("/ginsho.svg"));
		ps <- make_img(bless("/narigin.svg"));
		n <- make_img(bless("/keima.svg"));
		pn <- make_img(bless("/narikei.svg"));
		l <- make_img(bless("/kyosha.svg"));
		pl <- make_img(bless("/narikyo.svg"));
		pp <- make_img(bless("/tokin.svg"));
		let
		    fun drawInverted img x y =
			save ctx;
			translate ctx (float (((x + 1) * xd) - offs)) (float (((y + 1) * yd) - offs));
			rotate ctx 3.14;
			drawImage2 ctx img (float 0) (float 0) (float xp) (float yp);		    
			restore ctx		    

		    fun draw img x y =
			drawImage2 ctx img (float ((x * xd) + offs)) (float ((y * yd) + offs)) (float xp) (float yp)
			
		    fun drawBoard () =
			clearRect ctx (float 0) (float 0) (float cw) (float ch);
			setFillStyle ctx (make_rgba 212 172 89 1.0);
			fillRect ctx 0 0 cw ch;
			
			save ctx;
			translate ctx 5.0 5.0;
			setLineWidth ctx 1.0;
			setStrokeStyle ctx (make_rgba 0 0 0 1.0);
			
			drawLine ctx 0 0 0 h;
			drawLine ctx xd 0 xd h;
			drawLine ctx (xd * 2) 0 (xd * 2) h;
			drawLine ctx (xd * 3) 0 (xd * 3) h;
			drawLine ctx (xd * 4) 0 (xd * 4) h;
			drawLine ctx (xd * 5) 0 (xd * 5) h;
			drawLine ctx (xd * 6) 0 (xd * 6) h;
			drawLine ctx (xd * 7) 0 (xd * 7) h;
			drawLine ctx (xd * 8) 0 (xd * 8) h;
			drawLine ctx w 0 w h;

		    	
			drawLine ctx 0 0 w 0;
			drawLine ctx 0 yd w yd;
			drawLine ctx 0 (yd * 2) w (yd * 2);
			drawLine ctx 0 (yd * 3) w (yd * 3);
			drawLine ctx 0 (yd * 4) w (yd * 4);
			drawLine ctx 0 (yd * 5) w (yd * 5);
			drawLine ctx 0 (yd * 6) w (yd * 6);
			drawLine ctx 0 (yd * 7) w (yd * 7);
			drawLine ctx 0 (yd * 8) w (yd * 8);
			drawLine ctx 0 h w h;


			drawInverted l 0 0;
			drawInverted n 1 0;
			drawInverted s 2 0;
			drawInverted g 3 0;
			drawInverted kj 4 0;
			drawInverted g 5 0;
			drawInverted s 6 0;
			drawInverted n 7 0;
			drawInverted l 8 0;
			
			drawInverted r 1 1;
			drawInverted b 7 1;
			
			drawInverted p 0 2;
			drawInverted p 1 2;
			drawInverted p 2 2;
			drawInverted p 3 2;
			drawInverted p 4 2;
			drawInverted p 5 2;
			drawInverted p 6 2;
			drawInverted p 7 2;
			drawInverted p 8 2;
		    	

			draw l 0 8;
			draw n 1 8;
			draw s 2 8;
			draw g 3 8;
			draw k 4 8;
			draw g 5 8;
			draw s 6 8;
			draw n 7 8;
			draw l 8 8;
			
			draw b 1 7;
			draw r 7 7;

			draw p 0 6;
			draw p 1 6;
			draw p 2 6;
			draw p 3 6;
			draw p 4 6;
			draw p 5 6;
			draw p 6 6;
			draw p 7 6;
			draw p 8 6;
			
			restore ctx;
			return ()
		in
		    requestAnimationFrame2 drawBoard;
		    return <xml></xml>
		end
	in
	    return <xml>
	      <canvas id={c} width={cw} height={ch}>
		
	      </canvas>
	      <active code={onloadFn ()}>
	      </active>
	    </xml>
	end
end
