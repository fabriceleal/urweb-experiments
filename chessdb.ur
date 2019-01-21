open Database
open Chess
open Sharedboard
open Session
     
fun optS2S o =
    case o of
	None => ""
      | Some s => s

fun optI2I o =
    case o of
	None => 0
      | Some i => i

fun optI [t] (o : t) : option t =
    Some o


fun getRoom id =
    r <- oneRow (SELECT post.Room FROM post WHERE post.Id = {[id]});
    return r.Post.Room
    
fun addPostF idUser idPostParent txt =
    let

	fun importChildren id idP (state : gamestate) children =
	    let
		fun importChildrenAux children =
		    case children of
			[] => return ()
		      | h :: t =>
			
			case h of
			    Node (_, _, move, _, comments, children2) =>
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
					importChildren id nidP newState children2;
					importChildrenAux t
				    end
			    end
		    
	    in
		importChildrenAux children
	    end
	    
	fun importTree id idP root =
	    case root of
		Root (_, state, children, hdrs) => 
		dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[state_to_fen state]},
												    {[None]}, {[None]}, {[None]} ));
		importChildren id idP state children
		
	fun insertPost tree =
	    id <- nextval postSeq;    
	    idP <- nextval positionSeq;
	    sharedboard <- ChessRoom.create;
    
	    dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId, UserId, PostType)
		 VALUES ({[id]}, {[txt]}, {[idP]}, {[idP]}, {[sharedboard]}, {[idPostParent]}, {[idUser]}, {[ptChess]}));
	    
	    importTree id idP tree

	val tree = Root (0, fen_to_state startingFen, [], [])
    in
	insertPost tree
    end

fun speak id line =
    mUserId <- currUserId ();
    case mUserId of
	None => return ()
      | Some userId => 
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
		     debug "alg:";
		     debug newMoveAlg;
		     ChessRoom.send room (MPosition {State = (fen_to_state newFen),
					       Old = state, Id = idP,
					       Move = newMove, MoveAlg = newMoveAlg,
					       Previous = row.Post.CurrentPositionId,
					       Highlight = []})
		 end
	    end
	  | SBack =>
	    return ()
	    (* row <- oneRow (SELECT post.CurrentPositionId FROM post WHERE post.Id = {[id]});
	    row2 <- oneRow (SELECT position.Id, position.Fen, position.PreviousPositionId, position.Move, position.MoveAlg
			    FROM position
			    WHERE position.PostId = {[id]} AND position.Id < {[row.Post.CurrentPositionId]}
			    ORDER BY position.Id DESC LIMIT 1);
	    let
		val idP = row2.Position.Id
	    in
		dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});		
		room <- getRoom id;
		Room.send room (Position {State = (fen_to_state row2.Position.Fen), Id = idP,
					  Move = optS2S row2.Position.Move, MoveAlg = optS2S row2.Position.MoveAlg,
					  Previous = optI2I row2.Position.PreviousPositionId, Highlight = []})
	    end *)
	  | SForward =>
	    return ()
	    (* row <- oneRow (SELECT post.CurrentPositionId FROM post WHERE post.Id = {[id]});
	    row2 <- oneRow (SELECT position.Id, position.Fen, position.PreviousPositionId, position.Move, position.MoveAlg
			    FROM position
			    WHERE position.PostId = {[id]} AND position.Id > {[row.Post.CurrentPositionId]}
			    ORDER BY position.Id ASC LIMIT 1);
	    let
		val idP = row2.Position.Id
	    in
		dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});		
		room <- getRoom id;
		Room.send room (Position {State = (fen_to_state row2.Position.Fen), Id = idP,
					  Move = optS2S row2.Position.Move, MoveAlg = optS2S row2.Position.MoveAlg,
					  Previous = optI2I row2.Position.PreviousPositionId, Highlight = []})
	    end *)
	  | SPosition idP =>
	    row2 <- oneRow (SELECT position.Id, position.Fen, position.PreviousPositionId, position.Move, position.MoveAlg
			    FROM position			     
			    WHERE position.PostId = {[id]} AND position.Id = {[idP]});

	    row3 <- oneRow (SELECT position.Id, position.Fen
			    FROM position
			    WHERE position.Id = {[case (row2.Position.PreviousPositionId) of
			      None => 0
			      | Some i => i]} );
	    
	    dml (UPDATE post SET CurrentPositionId = {[idP]} WHERE Id = {[id]});		
	    room <- getRoom id;
	    ChessRoom.send room (MPosition {State = (fen_to_state row2.Position.Fen),
				      Old = (fen_to_state row3.Position.Fen), Id = idP,
				      Move = optS2S row2.Position.Move, MoveAlg = optS2S row2.Position.MoveAlg,
				      Previous = optI2I row2.Position.PreviousPositionId, Highlight = []})
	    
	  | SHighlight sq =>
	    room <- getRoom id;
	    ChessRoom.send room (MHighlight sq)
	  | SComment txt =>
	    idC <- nextval commentSeq;
	    t <- now;
	    dml (INSERT INTO comment (Id, PositionId, Content, UserId, Sent) VALUES({[idC]}, {[id]}, {[txt]}, {[userId]}, {[t]} ));
	    room <- getRoom id;
	    ChessRoom.send room (MComment txt)
	  | SNewPost (optP, txt) =>
	    addPostF userId optP txt;
	    return ()
	  | SChangeName (id, txt) =>
	    dml (UPDATE post SET Nam = {[txt]} WHERE Id = {[id]});
	    room <- getRoom id;
	    ChessRoom.send room (MChangeName txt)

(*    tree4 id *)
fun doSpeak id line =	 
    rpc (speak id line)
	 
