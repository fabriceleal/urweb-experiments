
open Canvas_FFI
open Chess
open Bootstrap4
open Pgnparse
open Canvasboard
open Nmarkdown

style form_signin

structure Room = Sharedboard.Make(struct
				      type t = boardmsg
				  end)

sequence postSeq
sequence positionSeq
sequence commentSeq
sequence inviteSeq

table post : { Id : int, Nam : string, RootPositionId: int, CurrentPositionId : int, ParentPostId : option int, Room : Room.topic }
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

sequence userSeq

table user: {Id: userId, Nam: string, Pass: Hash.digest, Salt: string }
		PRIMARY KEY Id
		CONSTRAINT Nam UNIQUE Nam

datatype inviteStatus =
	 Sent
       | Accepted
       | Cancelled

(* , Status: inviteStatus *)
table invite : {Id: int, UserId: userId, InvitedId: option userId, Code: string, Email: string, Sent: time, Status: int}
		   PRIMARY KEY Id,
		   CONSTRAINT Code UNIQUE Code,
		   CONSTRAINT Email UNIQUE Email

table rootAdmin : { Id : userId }
		      PRIMARY KEY Id,
      CONSTRAINT Id FOREIGN KEY Id REFERENCES user(Id)

fun insertUserWithId userid nam passS =
    salt <- Random.bytes 64;
    let
	val saltEncoded = Base64_FFI.encode(salt)
	val passraw = passS ^ saltEncoded
	val pass = Hash.sha512 (textBlob passraw)
    in
	debug ("saltEncoded:" ^ saltEncoded);
	dml (INSERT INTO user (Id, Nam, Pass, Salt) VALUES ({[userid]}, {[nam]}, {[pass]}, {[saltEncoded]}))
    end
      
task initialize = fn () =>
    b <- nonempty rootAdmin;
    if b then
        return ()
    else
	(* salt <- Random.bytes 64;
	let
	    val saltEncoded = Base64_FFI.encode(salt)
	    val passraw = "root" ^ saltEncoded
	    val pass = Hash.sha512 (textBlob passraw)
	in
	    debug ("saltEncoded:" ^ saltEncoded);
	    dml (INSERT INTO user (Id, Nam, Pass, Salt) VALUES (0, 'root', {[pass]}, {[saltEncoded]}));
            dml (INSERT INTO rootAdmin (Id) VALUES (0))
	end*)
	insertUserWithId 0 "root" "root";
	dml (INSERT INTO rootAdmin (Id) VALUES (0))


cookie login : {Id: userId}

fun currUser () =
    ro <- getCookie login;
    case ro of
	None => return None
      | Some r =>
	row <- oneRow (SELECT user.Id, user.Nam FROM user WHERE user.Id = {[r.Id]} );
	return (Some row.User)

fun currUserId () =
    ro <- getCookie login;
    case ro of
	None => return None
      | Some r => return (Some r.Id)

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
	    return (Root (0, "", []))
	  | Some root' => 
	    ch <- recurse root parentFen;
	    return (Root (root', parentFen, ch))
    end
	(**)

fun tree4 (id: int) =
    current <- oneRow (SELECT post.RootPositionId, position.Fen FROM post JOIN position ON post.RootPositionId = position.Id WHERE post.Id = {[id]});
    tree3 (Some current.Post.RootPositionId) current.Position.Fen   
		     

fun getRoom id =
    r <- oneRow (SELECT post.Room FROM post WHERE post.Id = {[id]});
    return r.Post.Room

fun addPostF idPostParent txt =
    let

	fun importChildren id idP fen children =
	    let
		val state = fen_to_state fen
		fun importChildrenAux children =
		    case children of
			[] => return ()
		      | h :: t =>
			
			case h of
			    Node (_, _, move, _, children2) =>
			    let
				val rmove = str_to_move move
			    in
				case (doMove state rmove) of
				    None => return ()
				  | Some newState => 
				    nidP <- nextval positionSeq;
				    let					
					val nfen = state_to_fen newState
					val nmove = moveStr rmove
					val alg = moveToAlgebraicClean state rmove newState
				    in
					dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId )
					     VALUES ({[nidP]}, {[id]}, {[nfen]}, {[Some nmove]}, {[Some alg]}, {[Some idP]} ));
					importChildren id nidP nfen children2;
					importChildrenAux t
				    end
			    end
		    
	    in
		importChildrenAux children
	    end
	    
	fun importTree id idP root =
	    case root of
		Root (_, fen, children) => 
		dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[fen]},
												    {[None]}, {[None]}, {[None]} ));
		importChildren id idP fen children
		
	fun insertPost tree =
	    id <- nextval postSeq;    
	    idP <- nextval positionSeq;
	    sharedboard <- Room.create;
    
	    dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId)
		 VALUES ({[id]}, {[txt]}, {[idP]}, {[idP]}, {[sharedboard]}, {[idPostParent]}));
	    
	    importTree id idP tree

	val tree = Root (0, startingFen, [])
    in
	insertPost tree
    end


fun speak id line =
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
		       dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId)
			    VALUES ({[idP]}, {[id]}, {[newFen]}, {[Some newMove]}, {[Some newMoveAlg]},
				{[Some row.Post.CurrentPositionId]}) );
		       
		       room <- getRoom id;
		       
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
	    room <- getRoom id;
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
	    room <- getRoom id;
	    Room.send room (Position {State = (fen_to_state row2.Position.Fen), Id = idP, Highlight = []})
	end
      | SPosition idP =>
	row2 <- oneRow (SELECT position.Id, position.Fen
			FROM position
			WHERE position.PostId = {[id]} AND position.Id = {[idP]});
	
	dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});		
	room <- getRoom id;
	Room.send room (Position {State = (fen_to_state row2.Position.Fen), Id = idP, Highlight = []})
	
      | SHighlight sq =>
	room <- getRoom id;
	Room.send room (Highlight sq)
      | SComment txt =>
	idC <- nextval commentSeq;
	dml (INSERT INTO comment (Id, PositionId, Content) VALUES({[idC]}, {[id]}, {[txt]} ));
	room <- getRoom id;
	Room.send room (Comment txt)
      | SNewPost (optP, txt) =>
	addPostF optP txt;
	return ()

fun getTree id =
    tree4 id

fun getComments (id : int) : transaction (list string) =
    List.mapQuery (SELECT comment.Content FROM comment WHERE comment.PositionId = {[id]} ORDER BY comment.Id)
		  (fn i => i.Comment.Content)
    
fun doSpeak id line =	 
    rpc (speak id line)

fun renderPostTree (id : int) : transaction xbody =
    let
        fun recurse (root : option int) =
            queryX' (SELECT * FROM post WHERE {eqNullable' (SQL post.ParentPostId) root})
                    (fn r =>
                        children <- recurse (Some r.Post.Id);
                        return <xml>
                          <li>			    
			    <form>
			      <submit action={postPage2 r.Post.Id} value={r.Post.Nam} />
			    </form>
			  </li>
                          
                          <ul>
                            {children}
                          </ul>
                        </xml>)
    in
        recurse (Some id)
    end

and postPage2 id () =
    current <- oneRow (SELECT post.Id, post.Nam, post.Room, post.RootPositionId, Position.Fen, PositionR.Fen
		       FROM post
			 JOIN position AS Position ON post.CurrentPositionId = Position.Id
			 JOIN position AS PositionR ON post.RootPositionId = PositionR.Id
		       WHERE post.Id = {[id]});
(*    postTree <- renderPostTree id; *)
    cid <- fresh;
    ch <- Room.subscribe current.Post.Room;
    (boardy, pgnviewer, commentviewer) <- generate_board current.Position.Fen cid 60 True
							 (fn _ => getTree current.Post.Id)
							 (fn _ => getComments current.Post.Id )
							 (fn s => doSpeak current.Post.Id s) ch;
    commenttxt <- source "";
    newpostname <- source "";

    genPage <xml>
	<div class={container}>
	  post # {[id]}
	  
	  <div class={row}>
	    
	    <div class={col_sm_2}>

	      <button value="Back" onclick={fn _ => doSpeak id SBack } />
		<button value="Fw" onclick={fn _ => doSpeak id SForward } />
		  <a link={downloadPost id}>download</a>
(*
		  { postTree }

		  <div>
		    <ctextbox source={newpostname} />
		    <button value="New Empty Post" onclick={fn _ =>
							       nam <- get newpostname;
							       doSpeak id (SNewPost (Some id, nam));
							       set newpostname "" } />
		  </div> *)
	    </div>
	    <div class={col_sm_6}>
	      {boardy}
	    </div>
	    <div class={col_sm_4}>
	      {pgnviewer}

	      {commentviewer}
		
	      <div>
		<ctextarea source={commenttxt} />
		<button value="Comment" onclick={fn _ =>
						    txt <- get commenttxt;
						    doSpeak id (SComment txt);
						    set commenttxt "" } />
	      </div>
	    </div>
	  </div>
	</div>
	  </xml>
    (*
and  postPage id () =
    
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
		      <span class="move_clickable wrapping_span" onclick={fn _ => doSpeak id (SPosition idP)}>
			{[(moveToAlgebraic (fen_to_state fen) (str_to_move move) moveAlg forceAlg)]}
		      </span>
		      {siblingsRender}
		      {rest}
		    </xml>
		end
		
	and  renderPgn pgn =
	     case pgn of
		 Root (_, _, []) =>
		 return <xml> * </xml>
	       | Root (_, _, (a :: siblings)) => 
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
		    doSpeak id (SMovePiece (move.Src, move.Dest, move.Prom));
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
			      x <- rpc (getTree id);
			      set pgnstate x
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
	  <head>
	    <title>Post # {[id]}</title>
	    <link rel="stylesheet" type="text/css" href="/exp.css"/>
	    <link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	  </head>
	  <body onload={loadPage ()} >
	    <div class="container">
	    <h1>{[id]} {[current.Post.Nam]}</h1>

	    <div class="row">
	      <div class={col_sm_2}>

		<a link={index()}>another page</a>
		<a link={allPosts()}>all posts</a>
		
		<button value="Back" onclick={fn _ => doSpeak id SBack } />
		  <button value="Fw" onclick={fn _ => doSpeak id SForward } />
		    <a link={downloadPost id}>download</a>
	      </div>
	      <div class={col_sm_6}>
		<canvas id={c} width={canvasW} height={canvasH} onmousedown={mousedown} onmouseup={mouseup} onmousemove={mousemove} >
	      </canvas>
	    </div>
	    <div class={col_sm_4}>
	      <dyn signal={m <- signal pgnstate; renderPgn m } />
	    </div>
	    </div>
	    
	</div>
			      </body>
			</xml>
    end *)


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
		 Root (_, _, []) =>
		 "*"
	       | Root (_, _, (a :: siblings)) => 
		 renderPgnN a siblings False
    in
	tree <- tree4 id;
	setHeader (blessResponseHeader "Content-Disposition")
		  ("attachment; filename=post_" ^ (show id) ^ ".pgn");
	returnBlob (textBlob (renderPgn tree)) (blessMime "application/octet-stream")
    end

and logon r =    
    ro <- oneOrNoRows (SELECT user.Id, user.Pass, user.Salt FROM user WHERE user.Nam = {[r.Nam]});
    case ro of
	None => error <xml>Wrong user or pass!</xml>
      | Some r' =>
	let
	    val hashed = Hash.sha512(textBlob(r.Pass ^ r'.User.Salt))
	in
	    if hashed = r'.User.Pass then
		setCookie login {Value = {Id=r'.User.Id}, Secure=False, Expires = None};
		redirect (url (index ()))
	    else
		error <xml>Wrong user or pass!</xml>
	end
	

and logoff () =
    clearCookie login;
    redirect (url (index ()))

and testMk () =
    return <xml>
      <body>
	{renderMk (compile "# title
blah blah blah [link](/Helloworld/allPosts)

---

blah [another link](/Helloworld/postPage2/3)


")}
      </body>
    </xml>

and validateUsernamePolicy u =
    (* check only for letters, numbers and underscore  *)    
    (strlen u) > 3

and validatePasswordPolicy p =
    (strlen p) > 6
    
and validateNewUser u =
    if validateUsernamePolicy u.Bleh then
	if validatePasswordPolicy u.Blah then
	    if u.Blah = u.ConfirmP then
		invite <- oneOrNoRows (SELECT invite.Id FROM invite WHERE invite.Code = {[u.InviteCode]} AND invite.Status = 0);
		case invite of
		    None => return (Some "invalid invite code")
		  | Some invite' => 		
		    existing <- oneOrNoRows (SELECT user.Id FROM user WHERE user.Nam = {[u.Bleh]} );
		    (case existing of
			 None => return None
		       | Some _ => return (Some "username already taken!"))
	    else
		return (Some "passwords dont match")	
	else
	    return (Some "password doesnt match policy: at least 6 characters long")
    else
	return (Some "username doesnt match policy: at least 3 characters long, using only letters numbers and _ ")

and validateAndCreate u =
    vRes <- validateNewUser u;
    case vRes of
	None =>
	idU <- nextval userSeq;
	dml (UPDATE invite SET InvitedId = {[Some idU]}, Status = 1 WHERE Code = {[u.InviteCode]} );
	insertUserWithId idU u.Bleh u.Blah;
	return None
      | _ => return vRes
    
    (*
and newUser u =
    vRes <- validateNewUser u;
    case vRes of
	None =>
	redirect (url (main ()))
      | Some str =>
	error <xml>{[str]}</xml>
	*)
	
(*    redirect (url (main ()))*)
	
and createAccount invCode =
    cinvitecode <- source invCode;
    cnam <- source "";
(*    cemail <- source "";*)
    cpass <- source "";
    cpassconf <- source "";
    cerr <- source "";
    return <xml>
      <body>

	<h2>Sign Up</h2>

	<dyn signal={m <- signal cerr; return <xml>{[m]}</xml>} />

								<dyn signal={i <- signal cinvitecode;
									     n <- signal cnam;
(*									     e <- signal cemail;*)
									      p <- signal cpass;
									      p2 <- signal cpassconf;
									     return <xml>
									       <active code={set cerr ""; return <xml/>} />
									       </xml>} />
	<div>
	  Invite Code: <ctextbox source={cinvitecode} /> <br />
(*	  Email: <ctextbox source={cemail}  /> <br />	  *)
	  Username: <ctextbox source={cnam}  /> <br />	  
	  Password: <cpassword source={cpass} /> <br />
	  Confirm: <cpassword source={cpassconf}  /> <br />

	  <button onclick={fn _ =>
			      set cerr "";
			      invitecode <- get cinvitecode;
			      nam <- get cnam;
(*			      email <- get cemail;*)
			      pass <- get cpass;
			      passconf <- get cpassconf;
			      res <- rpc (validateAndCreate {InviteCode=invitecode,Bleh=nam,Blah=pass,ConfirmP=passconf});
			      case res of
				  None => redirect (url (index ()))
				| Some err => set cerr err
			  } value="Sign Up"/>
	</div>
	
      </body>
    </xml>

			   
and generateInvite r =
u <- currUserId ();
    case u of
	None => error <xml>Not authenticated</xml>
      | Some u' =>	
	id <- nextval inviteSeq;
	sent <- now;
	bytes <- Random.bytes 4;
	dml (INSERT INTO invite (Id, Email, UserId, InvitedId, Code, Sent, Status)
	     VALUES ({[id]}, {[r.To]}, {[u']}, {[None]}, {[Base64_FFI.encode bytes]}, {[sent]}, {[0]}));

	minv <- oneOrNoRows (SELECT invite.Code FROM invite WHERE invite.Id = {[id]});

	case minv of
	    None => error <xml>Unable to create invite, please try again</xml>
	  | Some inv => 
	    return <xml>
	      <body>
		<h2>Invite</h2>

		Code: <b>{[inv.Invite.Code]}</b> <br />

		Copy (right click on link - Copy Link Location) and send this link:
		
		<a link={createAccount inv.Invite.Code}>Link</a>

		<br />
		<a link={invites ()}>Back to invite list</a>
	      </body>
	    </xml>
    
and invites () =
    u <- currUserId ();
    case u of
	None => error <xml>Not authenticated</xml>
      | Some u' =>
	
	rows <- query (SELECT invite.Code, invite.InvitedId, invite.Email, invite.Status FROM invite WHERE invite.UserId = {[u']})
		      (fn data acc =>
			  return <xml>
			    {acc}
			    <tr>
			      <td>{[data.Invite.Code]}</td>
			      <td>{[data.Invite.Email]}</td>
			      <td>
				{case data.Invite.InvitedId of
				     None => <xml><a link={createAccount data.Invite.Code}>Link</a></xml>
				   | Some id => <xml>
				     <a link={turtle id }>Accepted</a>
				   </xml>
				}
			      </td>
			    </tr>
			  </xml>) <xml/>; (**)
	return
	    <xml>
	      <body>
		<h2>Invites</h2>
		
		Send invite: you have x left
		<form>
		  <textbox{#To} />
		  <submit action={generateInvite} />
		</form>

		<h3>Invites sent</h3>
		<table>
		  {rows}
		</table>	
	      </body>
	    </xml>

and index () =
    u <- currUser ();
    case u of
	       None =>
	       (* <xml>
		 <body>
		   <form>
		     <table>
		       <tr><th>Name</th><td><textbox{#Nam}/></td></tr>
		       <tr><th>Password:</th><td><password{#Pass}/></td></tr>
		       <tr><td><submit action={logon}/></td></tr>
		       <tr></tr>
		     </table>
		   </form>
		 </body>
		 </xml> *)
	       index_login ()
	       
	     | Some u => 
	       (* <xml>
		 <body>
		   <h1>Welcome to the turtle corner! {[u.Nam]}</h1>
		   <a link={index ()}>index</a>
		   <a link={invites ()}>my invites</a>
		   <a link={me ()}>my profile</a>
		   <a link={myPosts ()}>my posts</a>
		   <a link={logoff ()}>logoff</a>
		   <a link={createPost ()}>create post</a>
		   <a link={allPosts  ()}>all posts</a>
		 </body>
		 </xml> *)
	       index_on ()

and handleTestUpload r =
    return <xml>
      <body>
(*	      {
	       List.foldr
		   (fn i acc => <xml><div>{

				List.foldr (fn i2 acc2 => <xml><div>{[i2]}</div> {acc2}</xml>) <xml></xml> i
				
				} </div> <div>+</div> {acc}</xml>) <xml></xml> (pgnsToStrs (Filetext_FFI.blobAsText (fileData r.Fil))) }
*)
	      {
	       List.foldr
		   (fn i acc => <xml><div>{[show i]} </div> {acc}</xml>) <xml></xml> (pgnsToGames (Filetext_FFI.blobAsText (fileData r.Fil))) }

(*
	      {
	       List.foldr
		   (fn i acc => <xml><div>{

				let
				    val d = Nregexpgn.decomposePgnL i
				in
				    List.foldr (fn i2 acc2 =>
						   <xml>
						     <div>
						     {
						      List.foldr (fn i3 acc3 => <xml>
							<div>
							{[case i3 of
							      (grp,tag) => (show grp) ^" " ^ (show tag)]}
							</div>
							{acc3}</xml>) <xml></xml> i2
						     }
						     </div>

						     {acc2}
						   </xml>
					       ) <xml></xml> d
				    
				end
				
				} </div> <div>+</div> {acc}</xml>) <xml></xml> (pgnsToStrs (Filetext_FFI.blobAsText (fileData r.Fil))) }*)
	      
      </body>
    </xml>
    
and testUpload () =
	return <xml>
	  <body>
	    <form>
	      <upload{#Fil} />
	      <submit action={handleTestUpload} value="upload" />
	    </form>
	  </body>
	  </xml>

and userProfile id =
    me <- currUserId ();
    case me of
	None => error <xml>Not authenticated</xml>
      | Some u' =>
	let
	    val isMe = u' = id
	in
	    return <xml>
	      
		<h2>{[case isMe of
			  True => "My Profile"
			| False => "User " ^ (show id)]}</h2>

		Username: ... <br />
		
		Change password ...

		Describe yourself ...

		My Posts ...
		
	    </xml>
	end

and allTurtles () =
     rows <- query (SELECT user.Nam FROM user)		
		  (fn data acc =>
		      return <xml>{acc}
			<tr>
			  <td>{[data.User.Nam]}</td>
			</tr>
		      </xml>)
		  <xml/>;    
    return <xml>
      <body>
      <table border=1>
	<tr><th>Name</th></tr>
	{rows}
      </table>
      </body>
    </xml>
	
and turtle id =
    c <- userProfile id;
    genPage c

and me () =
    u <- currUserId ();
    case u of
	None => redirect (url (index ())) (* error <xml>Not authenticated</xml> *)
      | Some u' =>
	c <- userProfile u';
	genPage c

and myPosts () =
    return <xml>
      <body>
	<h2>My posts</h2>
	
      </body>
    </xml>
		   
and allPosts () =  
    rows <- query (SELECT post.Id, post.Nam, post.Room, post.RootPositionId, Position.Fen, PositionR.Fen
		       FROM post
			 JOIN position AS Position ON post.CurrentPositionId = Position.Id
			 JOIN position AS PositionR ON post.RootPositionId = PositionR.Id
		       WHERE {eqNullable' (SQL post.ParentPostId) None})
		
		  (fn data acc =>		      
		      cid <- fresh;
		      ch <- Room.subscribe data.Post.Room;
		      (board, _, _) <- generate_board data.Position.Fen cid 20 False
						      (fn _ => getTree data.Post.Id)
						      (fn _ => return [])
						      (fn s => doSpeak data.Post.Id s) ch;
		      return <xml>{acc}<tr>
			<td>{[data.Post.Nam]}</td>
			<td>{board}</td>
			<td>
			  (*
		       <form>
			 <submit action={postPage data.Post.Id} value="Enter"/>
		       </form>*)
		       <form>
			 <submit action={postPage2 data.Post.Id} value="Enter Room"/>
		       </form>
		     </td>
		      </tr>
		      </xml>)
		  <xml></xml>;
		  
    genPage <xml>
      <table border=1>
	<tr><th>Name</th><th>Board</th><th>Actions</th></tr>
	{rows}
      </table>
      <a link={createPost ()}>Create Post</a>
    </xml>
 
and createPost () = return <xml>
  <body>
    <form>
      <table>	
	<tr><th>Name:</th><td><textbox{#Nam}/></td></tr>
	<tr><th>File (optional):</th><td><upload{#Fil}/></td></tr>
	<tr><td><textarea{#Pgn}/></td></tr>
	<tr><th/><td><submit action={addPost} value="Create" /></td></tr>
      </table>
    </form>
  </body>
</xml>


and addPost newPost =
    let

	fun importChildren id idP fen children =
	    let
		val state = fen_to_state fen
		fun importChildrenAux children =
		    case children of
			[] => return ()
		      | h :: t =>
			
			case h of
			    Node (_, _, move, _, children2) =>
			    let
				val rmove = str_to_move move
			    in
				case (doMove state rmove) of
				    None => return ()
				  | Some newState => 
				    nidP <- nextval positionSeq;
				    let					
					val nfen = state_to_fen newState
					val nmove = moveStr rmove
					val alg = moveToAlgebraicClean state rmove newState
				    in
					dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId )
					     VALUES ({[nidP]}, {[id]}, {[nfen]}, {[Some nmove]}, {[Some alg]}, {[Some idP]} ));
					importChildren id nidP nfen children2;
					importChildrenAux t
				    end
			    end
		    
	    in
		importChildrenAux children
	    end
	    
	fun importTree id idP root =
	    case root of
		Root (_, fen, children) => 
		dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[fen]},
												    {[None]}, {[None]}, {[None]} ));
		importChildren id idP fen children
		
	fun insertPost tree =
	    id <- nextval postSeq;    
	    idP <- nextval positionSeq;
	    sharedboard <- Room.create;
    
	    dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId)
		 VALUES ({[id]}, {[newPost.Nam]}, {[idP]}, {[idP]}, {[sharedboard]}, {[None]}));
	    
	    importTree id idP tree
    
    in
    
	if blobSize (fileData newPost.Fil) > 10000 then
	    return <xml>too big</xml>
	else
	    let
		val tree = pgnToGame newPost.Pgn
	    in
		insertPost tree;	    
		redirect (url (allPosts ()))
	    end
	end
    
and genPage content =
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/exp.css" />
	<link rel="stylesheet" type="text/css" href="/bodyn.css" />
      </head>
      <body>
	<nav class="navbar navbar-expand-md navbar-dark bg-dark fixed-top">
	  { generateMenu () }
	</nav>
	<main class="container">
	  <div>
	    { content }
	  </div>
	</main>
      </body>
    </xml>

and generateMenu () =
    <xml>
      <ul class="navbar-nav mr-auto">
	<li class="nav-item"><a class="nav-link" link={index ()}>Home</a></li>
	<li class="nav-item"><a class="nav-link" link={allPosts ()}>All Posts</a></li>
	<li class="nav-item"><a class="nav-link" link={me ()}>My Profile</a></li>
      </ul>
    </xml>

and index_on () =
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/exp.css" />
	<link rel="stylesheet" type="text/css" href="/bodyj.css" />
      </head>
      <body>
	<nav class="navbar navbar-expand-md navbar-dark bg-dark fixed-top">
	    { generateMenu () }
	</nav>
	<main>
	  <div class="jumbotron">
	    <div class="container">
	      <h1>test</h1>
	      <p class="lead">blah blah blah</p>
	    </div>
	  </div>
	  <div class="container">
	    <div class="row">
	      <div class="col-md-4">
		<h2>do this</h2>
		<p>you can do this</p>
	      </div>
	      <div class="col-md-4">
		<h2>do that</h2>
		<p>you can also do that!</p>		
	      </div>
	      <div class="col-md-4">
		<h2>or procrastinate</h2>
		<p>procrastinate!</p>
	      </div>
	    </div>
	  </div>
	</main>
      </body>
    </xml>

and index_login () =
    userid <- fresh;
    passid <- fresh;
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/auth.css" />
      </head>
      <body class="text-center">
	<form class="form-signin">
	  <label class="sr-only" for={userid}>User</label>
	  <textbox{#Nam} id={userid} class="form-control" placeholder="User" />
	  <label class="sr-only" for={passid}>Pass</label>
	  <password{#Pass} id={passid} class="form-control" placeholder="Password" />
	  <submit class="btn btn-lg btn-primary btn-block" action={logon} value="Sign In" />
	</form>
      </body>
    </xml>

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
