
open Canvas_FFI
open Chess

type position = { Id: int, State: gamestate, Highlight: list square } 

datatype boardmsg =
	 Highlight of square
       | Position of position

datatype serverboardmsg =
	 SMovePiece of square * square
       | SHighlight of square
       | SBack 
       | SForward 

	 
structure Room = Sharedboard.Make(struct
				      type t = boardmsg
				  end)

sequence postSeq
sequence positionSeq
sequence commentSeq

table post : { Id : int, Nam : string, CurrentPositionId : int, Room : Room.topic }
		 PRIMARY KEY Id
	     
table position : {Id: int, PostId: int, Fen : string, PreviousPositionId: option int }
		     PRIMARY KEY Id

table comment : {Id: int, PositionId: int, Content: string }
		    PRIMARY KEY Id

type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }
	      
type boardstate = { Highlight: option square, Pieces: list piecerec, DragPiece: option draggingPiece, Full : gamestate }
	  

val startingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
 
val light = make_rgba 239 238 240 1.0
val dark = make_rgba 119 138 181 1.0
val red = make_rgba 255 0 0 1.0
val size = 60
val x = 10
val y = 10


fun state_to_board state =
    { Highlight = None, Full = state, Pieces=state.Pieces, DragPiece = None}
	
fun fen_to_board fen =
    let
	val state = fen_to_state fen
    in
	state_to_board state
    end
	
fun postPage id () =
    let
	fun getRoom () =
	    r <- oneRow (SELECT post.Room FROM post WHERE post.Id = {[id]});
	    return r.Post.Room

	and speak line =
	    case line of
		SMovePiece (src, dest) =>

		(* TODO check if move was already played *)
		
		idP <- nextval positionSeq;
		
		row <- oneRow (SELECT post.CurrentPositionId, position.Fen
			       FROM post JOIN position ON post.CurrentPositionId = position.Id
			       WHERE post.Id = {[id]} );
		
		let
		    val state = fen_to_state row.Position.Fen	
		in		     
		    case (doMove state src dest) of
		   | None => return ()
		   | Some manipulated =>
		     let			     
			 val newFen = state_to_fen manipulated
		     in
			 
			 dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});
			 dml (INSERT INTO position (Id, PostId, Fen, PreviousPositionId) VALUES ({[idP]}, {[id]}, {[newFen]},
											     {[Some row.Post.CurrentPositionId]}) );
			 
			 room <- getRoom ();
			 
			 Room.send room (Position {State = (fen_to_state newFen), Id = idP, Highlight = []})
		     end
		end
	      | SBack =>		
		row <- oneRow (SELECT post.CurrentPositionId FROM post WHERE post.Id = {[id]});
		row2 <- oneRow (SELECT position.Id, position.Fen
				FROM position
				WHERE position.PostId = {[id]} AND position.Id < {[row.Post.CurrentPositionId]}
				ORDER BY position.Id DESC LIMIT 1);
		let
		    val idP = row2.Position.Id
		in
		  dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});		
		  room <- getRoom ();
		  Room.send room (Position {State = (fen_to_state row2.Position.Fen), Id = idP, Highlight = []})
		end
	      | SForward =>		
		row <- oneRow (SELECT post.CurrentPositionId FROM post WHERE post.Id = {[id]});
		row2 <- oneRow (SELECT position.Id, position.Fen
				FROM position
				WHERE position.PostId = {[id]} AND position.Id > {[row.Post.CurrentPositionId]}
				ORDER BY position.Id ASC LIMIT 1);
		let
		    val idP = row2.Position.Id
		in
		  dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});		
		  room <- getRoom ();
		  Room.send room (Position {State = (fen_to_state row2.Position.Fen), Id = idP, Highlight = []})
		end
	      | SHighlight sq =>
		room <- getRoom ();
		Room.send room (Highlight sq)

	and doSpeak line =	 
	    rpc (speak line)
	    
    in

	current <- oneRow (SELECT post.Nam, post.Room, position.Fen
			   FROM post JOIN position ON post.CurrentPositionId = position.Id (* post.Id = position.PostId *)
			   WHERE post.Id = {[id]});
	renderstate <- source None;
	ch <- Room.subscribe current.Post.Room;
	c <- fresh;
	
	let
	    
	    fun clampToBoardCoordinateX rawX =
		trunc (float(rawX) / float(size))

	    and clampToBoardCoordinateY rawY =
		trunc (float(rawY) / float(size))
			  
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
				    Full = p''.Full,
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
				 Full = p''.Full,
				 DragPiece = None}
			 in
			     set renderstate (Some st);
			     return ()
			 end
		       | Some d =>
			 let
			     val sqX = clampToBoardCoordinateX e.OffsetX
			     val sqY = clampToBoardCoordinateY e.OffsetY

			     val srcX = clampToBoardCoordinateX d.Src.RawX
			     val srcY = clampToBoardCoordinateY d.Src.RawY
	
			 in

			     case (doMove p''.Full {X=srcX,Y=srcY} {X=sqX,Y=sqY}) of
				 None =>
				 let
				     val st = {Highlight = None,
					       Pieces = p''.Full.Pieces,
					       Full = p''.Full,
					       DragPiece = None}
				 in
				     set renderstate (Some st);
				     doSpeak (SMovePiece ({X=srcX, Y=srcY}, {X=sqX,Y=sqY}));
				     return ()
				 end
			       | Some newState =>
				 let
				     val st = {Highlight = None,
					       Pieces = newState.Pieces,
					       Full = newState,
					       DragPiece = None}
				 in
				     set renderstate (Some st);
				     doSpeak (SMovePiece ({X=srcX, Y=srcY}, {X=sqX,Y=sqY}));
				     return ()
				 end
			     
			 end)
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
				Pieces = p''.Pieces,
				Full = p''.Full,
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
				Full = p''.Full,
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
			  Highlight(sq) =>
			    (s' <- get renderstate;
			    case s' of
			      | Some s'' =>
				set renderstate (Some {
						 Highlight = Some sq,
						 Pieces = s''.Pieces,
						 Full = s''.Full,
						 DragPiece = s''.DragPiece
						})
			      | None => return ())
			  | Position(p) =>
			    (s' <- get renderstate;
			    case s' of
			      | Some s'' =>
				set renderstate (Some {
						 Highlight = None,
						 Pieces = p.State.Pieces,
						 Full = p.State,
						 DragPiece = None
						})
			      | None => return ())
			    
			
		    and listener () =
			s <- recv ch;
			handle_boardmsg s;
			listener ()
			    

		in		    
		    set renderstate (Some (fen_to_board current.Position.Fen));
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
	    <a link={allPosts()}>all posts</a>

	    <button value="Back" onclick={fn _ => doSpeak SBack } />
	    <button value="Fw" onclick={fn _ => doSpeak SForward } />
	    
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
    
    dml (INSERT INTO post (Id, Nam, CurrentPositionId, Room) VALUES ({[id]}, {[newPost.Nam]}, {[idP]}, {[sharedboard]}));
    dml (INSERT INTO position (Id, PostId, Fen, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[startingFen]}, {[None]} ));
    
    redirect (bless "/Helloworld/allPosts")
    
