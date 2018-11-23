open Chess
open Canvas_FFI
     
style move_clickable
style wrapping_span
style comments_span

type position = { Id: int, Previous : int, State: gamestate, Old : gamestate, Move : string, MoveAlg: string, Highlight: list square }
		
datatype boardmsg =
	 Highlight of square
       | Position of position
       | Comment of string
       | ChangeName of string

type renderCtx = { BK : img, BQ : img, BR : img, BB : img, BN : img, BP : img,
		   WK : img, WQ : img, WR : img, WB : img, WN : img, WP : img,
		   C2D: canvas2d }
	       
type rawPoint = { RawX: int, RawY : int}

type draggingPiece = { Src: rawPoint, Current: rawPoint, Piece: piece }

type promstate = { Src: square, Dest: square }
	      
type boardstate = { Highlight: list square, Pieces: list piecerec, DragPiece: option draggingPiece,
		    Full : gamestate, Prom: option promstate  }
		  
datatype serverboardmsg =
	 SMovePiece of square * square * option kind
       | SHighlight of square
       | SBack 
       | SForward
       | SPosition of int
       | SComment of string
       | SNewPost of option int * string
       | SChangeName of int * string

fun state_to_board state =
    { Highlight = [], Full = state, Pieces=state.Pieces, DragPiece = None, Prom = None}
	
fun fen_to_board fen =
    let
	val state = fen_to_state fen
    in
	state_to_board state
    end

fun board_to_state (board : boardstate) : gamestate =
    board.Full

(*
val testFen = "rnbqkbnr/ppp1ppp1/7p/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6"
 *)

fun emptyTopLevelHandler (msg : boardmsg) =
    return ()

fun emptyTree _ =
    return (Root (0, "", [], []))

fun emptyOnGameState _ =
    return ()

(* TODO this structure will need to know more things to render the tree properly. 
some info comes from the parent in the old renderPgn functions! *)
    
datatype mutableTree =
	 Move of {Id:int, Move: string, MoveAlg: string, Position: gamestate, Children: source (list mutableTree)}

datatype mutableTreeRoot =
	 StartP of {Id:int, Position:gamestate, Children: source (list mutableTree) }

fun ltreeToMtree (ls : list pgnTree) : transaction (list mutableTree) =
    case ls of
	[] => return []
      | h :: t =>
	case h of
	    Node (id, position, move, alg, _, children) =>
	    lsCh <- ltreeToMtree children;
	    ch <- source lsCh;
	    rest <- ltreeToMtree t;
	    return ((Move {Id = id, Position = fen_to_state position, Move = move, MoveAlg = alg, Children = ch}) :: rest)
		 
fun treeToMtree (root : pgnRoot) : transaction mutableTreeRoot =
    case root of
	Root (id, position, children, _) =>
	ls <- ltreeToMtree children;
	c <- source ls;
	return (StartP {Id=id, Position = fen_to_state position, Children = c })

fun pToNode p =
    ch <- source [];
    return (Move {Id = p.Id, Move = p.Move, MoveAlg = p.MoveAlg, Position = p.Old, Children = ch})

fun chContainsId ch id =
    case (List.find (fn e => case e of
				 Move r => r.Id = id) ch) of
	None => False
      | Some _ => True
    
fun addToMtreeL p ls =
    case ls of
	[] => return False
      | h :: t =>
	case h of
	    Move r =>
	    ch <- get r.Children;
	    if r.Id = p.Previous then
		(* TODO do not change tree if the element is already there!! *)
		(if (chContainsId ch p.Id) then
		     return True
		 else
		     e <- pToNode p;
		     set r.Children (List.append ch (e :: []));
		     return True)
	    else
		rest <- addToMtreeL p t;
		if rest then
		    return True
		else
		    addToMtreeL p ch
    
fun addToMtree p mtreeSrc =
    elem <- get mtreeSrc;
    case elem of
	StartP r =>
	ls <- get r.Children;
	if r.Id = p.Previous then
	    (if (chContainsId ls p.Id) then
		 return ()
	     else
		 e <- pToNode p;
		 set r.Children (List.append ls (e :: [])))
	else
	    _ <- addToMtreeL p ls;
	    return ()
		
fun generate_board testFen c size editable getTree getComments doSpeak topLevelHandler onGameState mch =
    rctx <- source None;
(*    pgnstate <- source None;*)
    renderstate <- source None;
    mousestate <- source {RawX=0,RawY=0};
    cmm <- getComments ();
    commentsstate <- source cmm;

    tree <- getTree ();
    mtree <- treeToMtree tree;
    mtreeSrc <- source mtree;
    
    let
	(*
	val light = make_rgba 239 238 240 1.0
	val dark = make_rgba 119 138 181 1.0
	val red = make_rgba 255 0 0 1.0
	val promBg = make_rgba 244 244 244 1.0
	val promBgSel = make_rgba 211 211 211 1.0 *)
	
	(*val size = 60
	val x = 10
	val y = 10*)
	val offProm = 2
	val canvasW = if editable then size * 9 + offProm else size * 8
	val canvasH = size * 8


	fun renderPgnN pgnN siblings forceAlg =
	    case pgnN of
		Move r =>
		
		let
		    fun renderRest children = 
			case children of
			    [] => return <xml></xml>
			  | a :: siblings' => renderPgnN a siblings' (any siblings)
					      
		    fun renderSiblings siblings =
			case siblings of
			    [] => return <xml></xml>
			  | h :: t =>
			    rest <- renderSiblings t;
			    h' <- renderPgnN h [] True;
			    return <xml>
			      ( {h'} )
			      {rest}
			    </xml>			    
			    
		in
		    siblingsRender <- renderSiblings siblings;
		    return <xml>
		      <span class="move_clickable wrapping_span" onclick={fn _ => doSpeak (SPosition r.Id)}>
			{[(moveToAlgebraic r.Position (str_to_move r.Move) r.MoveAlg forceAlg)]}
		      </span>
		      {siblingsRender}
		      <dyn signal={children <- signal r.Children; renderRest children} />
		    </xml>
		end
	    (*case pgnN of
		Node (idP, fen, move, moveAlg, comments, children) =>
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
		      <span class="move_clickable wrapping_span" onclick={fn _ => doSpeak (SPosition idP)}>
			{[(moveToAlgebraic (fen_to_state fen) (str_to_move move) moveAlg forceAlg)]}
		      </span>
		      <span class="comments_span">
			{List.foldr (fn c acc => <xml>{[c]} {acc}</xml>) <xml></xml> comments}
		      </span>
		      {siblingsRender}
		      {rest}
		    </xml>
		end*)
		
	and  renderPgn pgn =
	     case pgn of
		 StartP r => 
		 let
		     fun renderPgn' children =
			 case children of
			     [] => return <xml> * </xml>
			   | a :: siblings =>
			     b <- renderPgnN a siblings False;
			     return <xml> {b} </xml>
		 in
		     return <xml><dyn signal={c <- signal r.Children; renderPgn' c} /></xml>
		 end
	     
	     (*
	     case pgn of
		 None => return <xml></xml>
	       | Some pgn' =>
		 case pgn' of
		     Root (_, _, [], _) =>
		     return <xml> * </xml>
		   | Root (_, _, (a :: siblings), _) => 
		     return <xml> {renderPgnN a siblings False} </xml>	*)	
		 
	and renderComments comments =
	    return (List.foldl (fn i acc => <xml>{[i]} {acc}</xml>) <xml></xml> comments)
		 
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
		    onGameState newState;
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
	
	and clampToPromSq rawX rawY =
	    if rawX >= (size * 8) + offProm && rawX <= (size * 9) + offProm then		    
		yPromToKind (clampToBoardCoordinateY rawY)
	    else
		None

	and insideQuad (rawX : int) (rawY : int) (srcX : int) (srcY : int) (size : int) : bool =
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

	and getLight () =
	    make_rgba 239 238 240 1.0

	and getDark () =
	    make_rgba 119 138 181 1.0

	and getRed () =
	    make_rgba 255 0 0 1.0

	and getPromBg () =
	    make_rgba 244 244 244 1.0

	and getPromBgSel () =
	    make_rgba 211 211 211 1.0
		     
	and drawTest () =
	    c' <- get rctx;
	    case c' of
		None => return ()
	      | Some c =>
		setFillStyle c.C2D (getLight ());
		fillRect c.C2D 0 0 60 60
(*
	and initTree () =
	    let
		fun initTree' () =
		    tree <- rpc (getTree ());
		    set pgnstate (Some tree);
		    return ()
	    in
		spawn (initTree' ());
		return <xml></xml>
	    end *)
	    
	and init () =

	    bk <- make_img(bless("/BlackKing.svg"));
	    bq <- make_img(bless("/BlackQueen.svg"));
	    br <- make_img(bless("/BlackRook.svg"));
	    bb <- make_img(bless("/BlackBishop.svg"));
	    bn <- make_img(bless("/BlackKnight.svg"));
	    bp <- make_img(bless("/BlackPawn.svg"));
	    
	    
	    wk <- make_img(bless("/WhiteKing.svg"));
	    wq <- make_img(bless("/WhiteQueen.svg"));
	    wr <- make_img(bless("/WhiteRook.svg"));
	    wb <- make_img(bless("/WhiteBishop.svg"));
	    wn <- make_img(bless("/WhiteKnight.svg"));
	    wp <- make_img(bless("/WhitePawn.svg"));
	    	    
	    ctx <- getContext2d c;

	    set rctx (Some {BK= bk, BQ= bq, BR = br, BB = bb, BN = bn, BP = bp,
			    WK= wk, WQ= wq, WR = wr, WB = wb, WN = wn, WP = wp,
			    C2D= ctx});
	    
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
			 setFillStyle ctx (getPromBgSel ())
		     else
			 setFillStyle ctx (getPromBg ())); 
		    fillRect ctx (size * 8 + offProm) (row * size) size size;
		    draw_piece_dl ctx piece (float (size * 8 + offProm)) (float (row * size))

		and paint_prom ctx pr x' =
		    if editable then
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
		    else
			return ()
				
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
			setFillStyle ctx (getLight ());
			paint_row0 ctx 0;
			paint_row1 ctx 1;
			paint_row0 ctx 2;
			paint_row1 ctx 3;
			paint_row0 ctx 4;
			paint_row1 ctx 5;
			paint_row0 ctx 6;
			paint_row1 ctx 7;
			
			setFillStyle ctx (getDark ());
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

			setFillStyle ctx (getRed ());
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
			Comment s =>
			s' <- get commentsstate;
			set commentsstate (s :: s')
			
		      | Highlight(sq) =>
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
			      onGameState p.State;
			      addToMtree p mtreeSrc;
			      return () 
			    | None => return ())
		      | _ =>
			topLevelHandler s

		and mlistener _ =
		    case mch of
			None => return ()
		      | Some ch => spawn (listener ch)
				   
		and listener ch =
		    s <- recv ch;
		    handle_boardmsg s;
		    listener ch
		    
(**)
		val startPos = fen_to_board testFen
	    in
		set renderstate (Some startPos);
		onGameState startPos.Full;
		requestAnimationFrame2 drawBoard3;
		mlistener ();		
		return <xml></xml>
	    end	    
	    

    in
	return (
	(if editable then
	     <xml>
	       <canvas id={c} height={canvasH} width={canvasW} onmousemove={mousemove} onmouseup={mouseup} onmousedown={mousedown}>      
	       </canvas>
	       <active code={init ()}>
	       </active>
	     </xml>
	 else
	     <xml>
	       <canvas id={c} height={canvasH} width={canvasW}>      
	       </canvas>
	       <active code={init ()}>
	       </active>
	     </xml>),
	<xml>
	 <dyn signal={m <- signal mtreeSrc; renderPgn m} />	   
	</xml>,
	<xml>
	  <dyn signal={m <- signal commentsstate; renderComments m } />
	</xml>,
     fn _ =>
	st <- get renderstate;
	return (case st of
		    None => None
		  | Some st' => Some (board_to_state st')))
    end
    
