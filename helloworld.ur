
open Canvas_FFI

type square = { X: int, Y : int}

datatype boardmsg =
	 MovePiece of square * square
       | Highlight of square
	 
structure Room = Sharedboard.Make(struct
				      type t = boardmsg
				  end)

sequence postSeq
table post : { Id : int, Nam : string, CurrentPositionId : option int, Room : Room.topic }
		 PRIMARY KEY Id

sequence positionSeq
table position : {Id: int, PostId: int, Fen : string, PreviousPositionId: option int }
		 PRIMARY KEY Id

datatype piece = WhiteKing | WhiteQueen | WhiteRook | WhiteBishop | WhiteKnight | WhitePawn |
	 BlackKing | BlackQueen | BlackRook | BlackBishop | BlackKnight | BlackPawn
	      
type piecerec = { X: int, Y : int, Piece : piece  }

type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }
	      
type boardstate = { (*Ctx: canvas2d,*) Highlight: option square, Pieces: list piecerec, DragPiece: option draggingPiece }

fun char_to_piece c =
    case c of
	#"k" => Some BlackKing
      | #"q" => Some BlackQueen
      | #"r" => Some BlackRook
      | #"b" => Some BlackBishop
      | #"n" => Some BlackKnight
      | #"p" => Some BlackPawn

      | #"K" => Some WhiteKing
      | #"Q" => Some WhiteQueen
      | #"R" => Some WhiteRook
      | #"B" => Some WhiteBishop
      | #"N" => Some WhiteKnight
      | #"P" => Some WhitePawn
		
      | _ => None
		  
fun fen_to_pieces (s : string) =
    let
	fun fen_to_pieces_aux (s : string) row col =
	    let
		val l = strlen s
	    in
		if l = 0 then
		    []
		else
		    let
			val fst = (strsub s 0)
		    in
			if fst = #"/" then
			    fen_to_pieces_aux (substring s 1 (l -1)) (row + 1) 0
			else
			    if isdigit fst then
				case (read (show fst) : option int) of
				    Some i => fen_to_pieces_aux (substring s 1 (l -1)) row (col + i)
				  | None => [] (*trigger error ? *)
			    else
				case (char_to_piece fst) of
				    None => [] (*trigger error? *)
				  | Some p => {X = col, Y = row, Piece= p} :: (fen_to_pieces_aux (substring s 1 (l - 1)) row (col + 1))
		    end
	    end
	    
    in
	fen_to_pieces_aux s 0 0
    end

val startingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    
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

    	 
val light = make_rgba 239 238 240 1.0
val dark = make_rgba 119 138 181 1.0
val red = make_rgba 255 0 0 1.0
val size = 60
val x = 10
val y = 10
	
fun postPage id () =
    let
	fun getRoom () =
	    r <- oneRow (SELECT post.Room FROM post WHERE post.Id = {[id]});
	    return r.Post.Room

	and speak line =
	    (case line of
	       | MovePiece (src, dest) =>
	     
		 idP <- nextval positionSeq;
		 
		 row <- oneRow (SELECT post.CurrentPositionId, position.Fen
				FROM post LEFT JOIN position ON post.Id = position.PostId WHERE post.Id = {[id]} );
		 dml (UPDATE post SET CurrentPositionId = {[Some idP]} WHERE Id = {[id]});
		 dml (INSERT INTO position (Id, PostId, Fen, PreviousPositionId) VALUES ({[idP]}, {[id]}, {[startingFen]},
										   {[row.Post.CurrentPositionId]}) );
	       
		 return ()
	       | _ => return ());
	    room <- getRoom ();
	    Room.send room line

	and doSpeak line =	 
	    rpc (speak line) 
    in

	current <- oneRow (SELECT post.Nam, post.Room, position.Fen FROM post LEFT JOIN position ON post.Id = position.Id WHERE post.Id = {[id]} );
	renderstate <- source None;
	ch <- Room.subscribe current.Post.Room;
	c <- fresh;
	
	let
	    
	    fun clampToBoardCoordinateX rawX =
		trunc (float(rawX) / float(size))

	    and clampToBoardCoordinateY rawY =
		trunc (float(rawY) / float(size))

	    and pieceInSquare (x : int) (y : int) =
		let
		    fun tmp (pp : piecerec) =
			pp.X = x && pp.Y = y
		in
		    tmp
		end

	    and pieceAt ls f =
		case ls of
		    h :: r =>
		    if (f h) then
			Some h
		    else
			pieceAt r f
		  | [] => None

	    and removePSquare ls f =
		case ls of
		    h :: r =>
		    if (f h) then
			r
		    else
			h :: (removePSquare r f)		       
		  | [] => []

	    and removeFromAddAt pieces sqSrc sqDest =
		let
		    val f = pieceInSquare sqSrc.X sqSrc.Y
		    val maybepiece = pieceAt pieces f
		in
		    case maybepiece of
			Some piece => 
			{ Piece=piece.Piece,X=sqDest.X, Y=sqDest.Y } :: (removePSquare pieces f)
		      | None => pieces
		end
			  
	    and mousedown e =
		p' <- get renderstate;
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
				val st : boardstate = {
				    Highlight = None, 
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
				set renderstate (Some st);
				return ()
			    end	
		    end
		  | None => return ()
			    
			    
	    and mouseup e =
		p' <- get renderstate;
		case p' of
		    Some p'' =>
		    (case p''.DragPiece of
			 None => 		
			 let
			     val st : boardstate = {
				 Highlight = None,
				 Pieces = p''.Pieces,
				 DragPiece = None}
			 in
			     set renderstate (Some st);
			     return ()
			 end
		       | Some d =>
			 let
			     val sqX = clampToBoardCoordinateX e.OffsetX
			     val sqY = clampToBoardCoordinateY e.OffsetY

			     (* TODO legal move validation, handle captures *)
			     val st : boardstate = {
				 Highlight = None,
				 Pieces = { Piece=d.Piece,X=sqX, Y=sqY } :: p''.Pieces,
				 DragPiece = None}
			 in
			     set renderstate (Some st);
			     doSpeak (MovePiece ({X=(clampToBoardCoordinateX d.Src.RawX), Y=(clampToBoardCoordinateY d.Src.RawY)}, {X=sqX,Y=sqY}));
			     return ()
			 end	)
		  | None => return ()
			    

	    and mousemove e =
		p' <- get renderstate;
		case p' of
		    None => return ()
		  | Some p'' =>
		    case p''.DragPiece of
			None =>
			let
			    val st : boardstate = {
				Highlight = None,
				(* Some {
				X = clampToBoardCoordinateX e.OffsetX,
				Y = clampToBoardCoordinateY e.OffsetY
				} *)
				Pieces = p''.Pieces,
				DragPiece = None}
			in
			    set renderstate (Some st);
			    return ()
			end
		      | Some d => 		    
			let
			    val st : boardstate = {
				Highlight = None,
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
			    set renderstate (Some st);
			    return ()
			end
			
	    and loadPage () =
		

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

		    and draw_piece ctx (p : piecerec) =		    
			drawImage2 ctx (piece_to_id p.Piece) (float (size * p.X)) (float (size * p.Y)) (float size) (float size)
			
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
		    and drawBoard2 ctx hs ps db =
			drawBoard ctx hs ps db

		    and drawBoard3 () =
			x2 <- get renderstate;
			case x2 of
			    Some x => 
			    drawBoard2 ctx (case x.Highlight of
						Some t => t :: []
					      | _ => []) x.Pieces x.DragPiece
			  | _ => return ()

		    and drawBoard4 () =
			drawBoard3 ();
			setTimeout drawBoard4 30

		    and handle_boardmsg s =
			case s of
			    MovePiece(src, dest) =>
			    s' <- get renderstate;
			    (case s' of
                              | Some s'' =>
				set renderstate (Some {
						 Highlight = s''.Highlight,
						 Pieces = (removeFromAddAt s''.Pieces src dest),
						 DragPiece = s''.DragPiece
						})
			      | None => return ()
			    )
			    
			  | Highlight(sq) =>
			    s' <- get renderstate;
			    case s' of
			      | Some s'' =>
				set renderstate (Some {
						 Highlight = Some sq,
						 Pieces = s''.Pieces,
						 DragPiece = s''.DragPiece
						})
			      | None => return ()
			
		    and listener () =
			s <- recv ch;
			handle_boardmsg s;
			listener ()
			    

		in

		    set renderstate (Some { Highlight = None, Pieces=(fen_to_pieces startingFen), DragPiece = None});
		    
		    requestAnimationFrame2 drawBoard3;

		    listener ();
		    return ()
		end

	in
	
	return  <xml>
	  <head><title>Post # {[id]}</title></head>
	  <body onload={loadPage ()} >
	    <h1>{[id]} {[current.Post.Nam]}</h1>
	    
	    <a link={index()}>another page</a>

	    <button value="Send" onclick={fn _ => doSpeak (Highlight {X=5, Y=5})} />	      
	    <button value="click" onclick={fn _ =>
						set renderstate (Some {Highlight = Some {X = 0, Y = 0},
								       Pieces=pieces,
								       DragPiece = None}) } />
            <canvas id={c} width={size * 8} height={size * 8} onmousedown={mousedown} onmouseup={mouseup} onmousemove={mousemove} >
	</canvas>
		    </body>
	    </xml>
    end
    
    end
     
and index () = return <xml>
  <body>index
    <a link={createPost ()}>create post</a>
    <a link={allPosts  ()}>all posts</a>
    <a link={main () }>new page</a></body></xml>

and main () =
    return <xml>
      <body>
      <h1>bla bla main!</h1>
      </body>
      </xml>

and allPosts () = 
  rows <- queryX (SELECT * FROM post)
		 (fn data => <xml>
		   <tr>
		     <td>{[data.Post.Nam]}</td>
		     <td>
		       <form>
			 <submit action={postPage data.Post.Id} value="Enter"/>
		       </form>
		     </td>
		 </tr></xml>);
    return <xml>
      <body>
      <table border=1>
	<tr><th>Name</th><th>Actions</th></tr>
	{rows}
      </table>

      <a link={createPost ()}>Create Post</a>
      </body>
    </xml>
 
and createPost () = return <xml>
  <body>
    <form>
      <table>
	
	<tr><th>Name:</th><td><textbox{#Nam}/></td></tr>
	<tr><th/><td><submit action={addPost} value="Create" /></td></tr>
      </table>
    </form>
  </body>
</xml>

and addPost newPost =
    id <- nextval postSeq;    
    idP <- nextval positionSeq;
    sharedboard <- Room.create;
    
    dml (INSERT INTO post (Id, Nam, CurrentPositionId, Room) VALUES ({[id]}, {[newPost.Nam]}, {[Some idP]}, {[sharedboard]}));
    dml (INSERT INTO position (Id, PostId, Fen, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[startingFen]}, {[None]} ));
    
    redirect (bless "/Helloworld/allPosts")
    
