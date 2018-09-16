
open Canvas_FFI
open Chess
open Bootstrap4

style move_clickable

type position = { Id: int, State: gamestate, Highlight: list square } 

datatype boardmsg =
	 Highlight of square
       | Position of position

datatype serverboardmsg =
	 SMovePiece of square * square * option kind
       | SHighlight of square
       | SBack 
       | SForward
       | SPosition of int

	 
structure Room = Sharedboard.Make(struct
				      type t = boardmsg
				  end)

		 
sequence postSeq
sequence positionSeq
sequence commentSeq

table post : { Id : int, Nam : string, RootPositionId: int, CurrentPositionId : int, Room : Room.topic }
		 PRIMARY KEY Id
	     
table position : {Id: int, PostId: int, Fen : string, Move: option string, MoveAlg: option string, PreviousPositionId: option int }
		     PRIMARY KEY Id

table comment : {Id: int, PositionId: int, Content: string }
		    PRIMARY KEY Id

open Pgn.Make(struct
		  con id = #Id
		  con parent = #PreviousPositionId
		  val tab = position
	      end)
		
type userId = int
	      
table user: {Id: userId, Nam: string, Pass: string}
		PRIMARY KEY Id

cookie login : {Id: userId, Pass: string}

fun currUser () =
    ro <- getCookie login;
    case ro of
	None => return None
      | Some r =>
	row <- oneRow (SELECT user.Id, user.Nam FROM user WHERE user.Id = {[r.Id]} AND user.Pass = {[r.Pass]});
	return (Some row.User)
	       
type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }

type promstate = { Src: square, Dest: square }
	      
type boardstate = { Highlight: list square, Pieces: list piecerec, DragPiece: option draggingPiece,
		    Full : gamestate, Prom: option promstate  }

val startingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
val testFen = "rnbqkbnr/ppp1ppp1/7p/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6"
 
val light = make_rgba 239 238 240 1.0
val dark = make_rgba 119 138 181 1.0
val red = make_rgba 255 0 0 1.0
val promBg = make_rgba 244 244 244 1.0
val promBgSel = make_rgba 211 211 211 1.0
val size = 60
val x = 10
val y = 10
val offProm = 2
val canvasW = size * 9 + offProm
val canvasH = size * 8



datatype pgnTree =
	 Node of int * string * string * string * list pgnTree
	 
datatype pgnRoot =
	 Root of int * list pgnTree
 

fun tree3 (root : option int) parentFen =
    let
	fun recurse root fen =
	    List.mapQueryM (SELECT position.Id, position.Fen, position.Move, position.MoveAlg
			    FROM position 
			    WHERE {eqNullable' (SQL position.PreviousPositionId) root})
			  (fn r =>
			      case (r.Position.Move, r.Position.MoveAlg) of
				  (Some move, Some alg) =>
				  ch <- recurse (Some r.Position.Id) r.Position.Fen;
				  return (Node (r.Position.Id, fen, move, alg, ch))
				| (_, _) =>
				  return (Node (r.Position.Id, "", "", "", []))
			  )
    in
	case root of
	    None =>
	    return (Root (0, []))
	  | Some root' => 
	    ch <- recurse root parentFen;
	    return (Root (root', ch))
    end
	(**)

fun tree4 (id: int) =
    current <- oneRow (SELECT post.RootPositionId, position.Fen FROM post JOIN position ON post.RootPositionId = position.Id WHERE post.Id = {[id]});
    tree3 (Some current.Post.RootPositionId) current.Position.Fen   
		     

fun state_to_board state =
    { Highlight = [], Full = state, Pieces=state.Pieces, DragPiece = None, Prom = None}
	
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
		SMovePiece (src, dest, kind) =>

		(* TODO check if move was already played *)
		
		idP <- nextval positionSeq;
		
		row <- oneRow (SELECT post.CurrentPositionId, position.Fen
			       FROM post JOIN position ON post.CurrentPositionId = position.Id
			       WHERE post.Id = {[id]} );
		
		let
		    val state = fen_to_state row.Position.Fen
		    val move = {Src=src, Dest=dest, Prom = kind}
		in		     
		    case (doMove state move) of
		   | None => return ()
		   | Some manipulated =>
		     let			     
			 val newFen = state_to_fen manipulated
			 val newMove = moveStr move
			 val newMoveAlg = moveToAlgebraicClean state move manipulated
		     in			 
			 dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});
			 dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId) VALUES ({[idP]}, {[id]}, {[newFen]},
												   {[Some newMove]},
												   {[Some newMoveAlg]},
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
	      | SPosition idP =>
		row2 <- oneRow (SELECT position.Id, position.Fen
				FROM position
				WHERE position.PostId = {[id]} AND position.Id = {[idP]});
		
		dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});		
		room <- getRoom ();
		Room.send room (Position {State = (fen_to_state row2.Position.Fen), Id = idP, Highlight = []})
		
	      | SHighlight sq =>
		room <- getRoom ();
		Room.send room (Highlight sq)

	and getTree () =
	    tree4 id
	    
	and doSpeak line =	 
	    rpc (speak line)
	    
    in

	current <- oneRow (SELECT post.Nam, post.Room, post.RootPositionId, Position.Fen, PositionR.Fen
			   FROM post
			     JOIN position AS Position ON post.CurrentPositionId = Position.Id
			     JOIN position AS PositionR ON post.RootPositionId = PositionR.Id
			   WHERE post.Id = {[id]});

	moveTree <- tree3 (Some current.Post.RootPositionId) current.PositionR.Fen;

	pgnstate <- source moveTree;
	renderstate <- source None;
	mousestate <- source {RawX=0,RawY=0};
	
	ch <- Room.subscribe current.Post.Room;
	c <- fresh;
	
	let
	    (* TODO algebraic *)
	    (* TODO variations, comments? *)

	    fun renderPgnN pgnN siblings forceAlg =
		case pgnN of
		    Node (idP, fen, move, moveAlg, children) =>
		    let
			val rest = case children of
				       [] => <xml></xml>
				     | a :: siblings' =>  renderPgnN a siblings' (any siblings)

			val siblingsRender = case siblings of
						 [] => <xml></xml>
					       | _ :: _ =>
						 <xml>
						   { List.foldl (fn rc acc => <xml>{acc} ( {renderPgnN rc [] True} )</xml>) <xml></xml> siblings}
						 </xml>
		    in
			<xml>
			  <span class={move_clickable} onclick={fn _ => doSpeak (SPosition idP)}>
			    {[(moveToAlgebraic (fen_to_state fen) (str_to_move move) moveAlg forceAlg)]}
			  </span>
			  {siblingsRender}
			  {rest}
			</xml>
		    end
		    
	    and  renderPgn pgn =
		case pgn of
		    Root (_, []) =>
		    return <xml> * </xml>
		  | Root (_, (a :: siblings)) => 
		    return <xml> {renderPgnN a siblings False} </xml>		
		    
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
			 
	    and clampToPromSq rawX rawY =
		if rawX >= (size * 8) + offProm && rawX <= (size * 9) + offProm then		    
		    yPromToKind (clampToBoardCoordinateY rawY)
		else
		    None

	    and insideQuad rawX rawY srcX srcY size =
		rawX >= srcX && rawX <= (srcX + size) && rawY >= srcY && rawY <= (srcY + size)
			  
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
			doSpeak (SMovePiece (move.Src, move.Dest, move.Prom));
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

		    and handle_boardmsg s =
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
				x <- rpc (getTree ());
				set pgnstate x
			      | None => return ())
			    
			
		    and listener () =
			s <- recv ch;
			handle_boardmsg s;
			listener ()
			    

		in
		    (*
		    debug (sqStr {X=0,Y=0});
		    debug (state_to_fen (fen_to_state testFen)); *)
		    set renderstate (Some (fen_to_board current.Position.Fen));
		    requestAnimationFrame2 drawBoard3;

		    listener ();
		    return ()
		end

	in
	
	return  <xml>
	  <head>
	    <title>Post # {[id]}</title>
	    <link rel="stylesheet" type="text/css" href="/exp.css"/>
	  </head>
	  <body onload={loadPage ()} >
	    <h1>{[id]} {[current.Post.Nam]}</h1>
	    
	    <a link={index()}>another page</a>
	    <a link={allPosts()}>all posts</a>

	    <button value="Back" onclick={fn _ => doSpeak SBack } />
	      <button value="Fw" onclick={fn _ => doSpeak SForward } />

		<a link={downloadPost id}>download</a>
	    
            <canvas id={c} width={canvasW} height={canvasH} onmousedown={mousedown} onmouseup={mouseup} onmousemove={mousemove} >
	</canvas>
	<dyn signal={m <- signal pgnstate; renderPgn m } />
		    </body>
	    </xml>
    end
    
    end

and downloadPost id =
    let
	fun renderPgnN pgnN siblings forceAlg =
	    case pgnN of
		Node (idP, fen, move, moveAlg, children) =>
		let
		    val rest = case children of
				   [] => ""
				 | a :: siblings' =>  renderPgnN a siblings' (any siblings)

		    val siblingsRender = case siblings of
					     [] => ""
					   | _ :: _ =>
					     (List.foldl (fn rc acc => acc ^ " (" ^ (renderPgnN rc [] True) ^ ") ") "" siblings)
		in
		    (moveToAlgebraic (fen_to_state fen) (str_to_move move) moveAlg forceAlg) ^ siblingsRender ^ " " ^ rest
		end		    
		
	and  renderPgn pgn =
	     case pgn of
		 Root (_, []) =>
		 "*"
	       | Root (_, (a :: siblings)) => 
		 renderPgnN a siblings False
    in
	tree <- tree4 id;
	setHeader (blessResponseHeader "Content-Disposition")
		  ("attachment; filename=post_" ^ (show id) ^ ".pgn");
	returnBlob (textBlob (renderPgn tree)) (blessMime "application/octet-stream")
    end
     
and index () = return <xml>
  <body>index
    <a link={createPost ()}>create post</a>
    <a link={allPosts  ()}>all posts</a>
    <a link={main () }>new page</a></body></xml>

and logon r =
    ro <- oneOrNoRows (SELECT user.Id FROM user WHERE user.Nam = {[r.Nam]} AND user.Pass = {[r.Pass]});
    case ro of
	None => error <xml>Wrong user or pass!</xml>
      | Some r' =>
	setCookie login {Value = {Id=r'.User.Id, Pass =r.Pass}, Secure=False, Expires = None};
	main ()

and main () =
    u <- currUser ();
    return (case u of
	       None =>
	       <xml>
		 <body>
		   <form>
		     <table>
		       <tr><th>Name</th><td><textbox{#Nam}/></td></tr>
		       <tr><th>Password:</th><td><textbox{#Pass}/></td></tr>
		       <tr><td><submit action={logon}/></td></tr>
		       <tr></tr>
		     </table>
		   </form>
		 </body>
	       </xml>
	     | Some u => 
	       <xml>
		 <body>
		   <h1>Welcome to the turtle corner! {[u.Nam]}</h1>
		   <a link={index ()}>index</a>
		 </body>
	       </xml>)

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
    
    dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room) VALUES ({[id]}, {[newPost.Nam]}, {[idP]}, {[idP]}, {[sharedboard]}));
    dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[startingFen]},
											{[None]}, {[None]}, {[None]} ));
    
    redirect (bless "/Helloworld/allPosts") 
(*    allPosts () *)

and testResponsive () =
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
      </head>
      <body>
	<div class={container}>
	  <div class={row}>
	    <div class={col_sm_4}>
	      <h3>game 1</h3>
	      <canvas width={220} height={200}>
		
	      </canvas>
	    </div>
	    <div class={col_sm_4}>
	      <h3>game 2</h3>
	      <canvas width={220} height={200}>
		
	      </canvas>
	    </div>
	    <div class={col_sm_4}>
	      <h3>game 3</h3>
	      <canvas width={220} height={200}>
		
	      </canvas>
	    </div>
	    <div class={col_sm_4}>
	      <h3>game 4</h3>
	      <canvas width={220} height={200}>
		
	      </canvas>
	    </div>
	    <div class={col_sm_4}>
	      <h3>game 5</h3>
	      <canvas width={220} height={200}>
		
	      </canvas>
	    </div>
	  </div>
	</div>
      </body>
    </xml>
(*
						    *)
