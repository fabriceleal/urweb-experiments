open Database
open Chess
open Sharedboard

  
fun getComments (id : int) : transaction (list string) =
    List.mapQuery (SELECT comment.Content FROM comment WHERE comment.PositionId = {[id]} ORDER BY comment.Id)
		  (fn i => i.Comment.Content)
    
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
				  cs <- getComments r.Position.Id;
				  return (Node (r.Position.Id, fen, move, alg, cs, ch))
				| (_, _) =>
				  return (Node (r.Position.Id, "", "", "", [], []))
			  )
    in
	case root of
	    None =>
	    return (Root (0, "", [], []))
	  | Some root' => 
	    ch <- recurse root parentFen;
	    return (Root (root', parentFen, ch, []))
    end

fun treeOn (root : option int) parentFen rows =
    let
	fun recurse root fen =
	    let 
		val children = List.filter (fn e =>
			    case (e.Position.PreviousPositionId, root) of
				(None, None) => True
			      | (Some a, Some b) => a = b
			      | (_, _) => False) rows

	    in
		List.mp (fn r =>
			      case (r.Position.Move, r.Position.MoveAlg) of
				  (Some move, Some alg) =>
				  Node (r.Position.Id, fen, move, alg, [], recurse (Some r.Position.Id) r.Position.Fen)
				| (_, _) =>
				  Node (r.Position.Id, "", "", "", [], [])
			) children
	    end
    in
	case root of
	    None =>
	    Root (0, "", [], [])
	  | Some root' => 
	    Root (root', parentFen, recurse root parentFen, [])
    end
	     
     
fun tree4 (id: int) =
    current <- oneRow (SELECT post.RootPositionId, position.Fen FROM post JOIN position ON post.RootPositionId = position.Id WHERE post.Id = {[id]});
    tree3 (Some current.Post.RootPositionId) current.Position.Fen

fun treeAtOnce (id:int) =
    root <- oneRow (SELECT post.RootPositionId, position.Fen
		    FROM post JOIN position ON post.RootPositionId = position.Id WHERE post.Id = {[id]});
    rows <- queryL (SELECT position.PostId, position.Id, position.PreviousPositionId, position.Fen, position.Move, position.MoveAlg
		    FROM position WHERE position.PostId = {[id]});
    return (treeOn (Some root.Post.RootPositionId) root.Position.Fen rows)

fun getTree id =
    treeAtOnce id

fun renderChessPost id boilerplate renderPostTree doSpeak fnCb = 
    current <- oneRow (SELECT post.Id, post.Nam, post.Room, post.RootPositionId, Position.Fen, PositionR.Fen
		       FROM post
			 JOIN position AS Position ON post.CurrentPositionId = Position.Id
			 JOIN position AS PositionR ON post.RootPositionId = PositionR.Id
		       WHERE post.Id = {[id]});
    cid <- fresh;
    postTree <- renderPostTree id False;
    ch <- Sharedboard.ChessRoom.subscribe current.Post.Room;

    pgnTree <- getTree current.Post.Id;
    mutTree <- treeToMtree pgnTree;

    (boardy, pgnviewer, commentviewer, _) <- generate_board current.Position.Fen cid 60 True
							    (fn _ => getTree current.Post.Id)
							    (fn _ => getComments current.Post.Id )
							    (fn s => doSpeak current.Post.Id s)
							    fnCb
							    emptyOnGameState
							    (Some ch);
    boilerplate postTree boardy pgnviewer commentviewer

fun renderWeiqiPost id boilerplate renderPostTree _ _ =
    postTree <- renderPostTree id False;
    boardy <- Weiqi.generate_board (Weiqi.Root (0, Weiqi.startingPosition, [], [])) (fn _ => return ());
    boilerplate postTree boardy <xml></xml> <xml></xml>
    
fun renderPost p boilerplate renderPostTree doSpeak fnCb =
    let
	val id = p.Post.Id
	val typ = p.Post.PostType
    in
	if typ = ptChess then
	    renderChessPost id boilerplate renderPostTree doSpeak fnCb
	else
	    if typ = ptWeiqi then
		renderWeiqiPost id boilerplate renderPostTree doSpeak fnCb
	    else
		error <xml>Unknown post type</xml>
    end
