
open Canvas_FFI
open Chess
open Bootstrap4
open Pgnparse
open Canvasboard
open Nmarkdown
     
(* login page *)
style form_signin
style form_signin_sep

(* clock page *)
style white_player
style black_player
style half
style full
style content
style commands
style flip
style cmd_button
      
structure Room = Sharedboard.Make(struct
				      type t = boardmsg
				  end)


		 
sequence postSeq
sequence positionSeq
sequence commentSeq
sequence inviteSeq

type userId = int


datatype clockSide =
	 CWhite
       | CBlack


datatype clockStatus =
	 Pend
       | Run of clockSide
       | Lost of clockSide

fun otherC c =
    case c of
	CWhite => CBlack
      | CBlack => CWhite

fun eqC a b =
    case (a, b) of
	(CWhite, CWhite) => True
      | (CBlack, CBlack) => True
      | (_, _) => False
		  
type timecontrol =
     {BaseTime : int, Increment: int, ResetIfUnexpired: bool}

type lsTimecontrol = list timecontrol

table position : {Id: int, PostId: int, Fen : string, Move: option string, MoveAlg: option string, PreviousPositionId: option int }
		     PRIMARY KEY Id,
		     CONSTRAINT CPreviousPositionId FOREIGN KEY PreviousPositionId REFERENCES position (Id) ON DELETE CASCADE

table post : { Id : int, Nam : string, RootPositionId: int, UserId: userId, CurrentPositionId : int, ParentPostId : option int, Room : Room.topic }
		 PRIMARY KEY Id (*,
		 CONSTRAINT CRootPositionId FOREIGN KEY RootPositionId REFERENCES position (Id) ON DELETE CASCADE,
		 CONSTRAINT CCurrentPositionId FOREIGN KEY CurrentPositionId REFERENCES position (Id) ON DELETE CASCADE
*)
table comment : {Id: int, PositionId: int, Content: string, UserId: userId, Sent: time  }
		    PRIMARY KEY Id

open Pgn.Make(struct
		  con id = #Id
		  con parent = #PreviousPositionId
		  val tab = position
	      end)
		
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
(*	debug ("saltEncoded:" ^ saltEncoded);*)
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
val size = 55
val x = 10
val y = 10
val offProm = 2
val canvasW = size * 9 + offProm
val canvasH = size * 8

datatype pageKind =
	 Active of string * url
       | Current of string
       | Inactive of string

datatype menuKind =
	 NMenu
       | Home
       | Posts
       | Profile
       | Invites
     
fun getComments (id : int) : transaction (list string) =
    List.mapQuery (SELECT comment.Content FROM comment WHERE comment.PositionId = {[id]} ORDER BY comment.Id)
		  (fn i => i.Comment.Content)
    

fun optS2S o =
    case o of
	None => ""
      | Some s => s

fun optI2I o =
    case o of
	None => 0
      | Some i => i
		  
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
		     

fun getRoom id =
    r <- oneRow (SELECT post.Room FROM post WHERE post.Id = {[id]});
    return r.Post.Room

fun addPostF idUser idPostParent txt =
    let

	fun importChildren id idP fen children =
	    let
		val state = fen_to_state fen
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
					importChildren id nidP nfen children2;
					importChildrenAux t
				    end
			    end
		    
	    in
		importChildrenAux children
	    end
	    
	fun importTree id idP root =
	    case root of
		Root (_, fen, children, hdrs) => 
		dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[fen]},
												    {[None]}, {[None]}, {[None]} ));
		importChildren id idP fen children
		
	fun insertPost tree =
	    id <- nextval postSeq;    
	    idP <- nextval positionSeq;
	    sharedboard <- Room.create;
    
	    dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId, UserId)
		 VALUES ({[id]}, {[txt]}, {[idP]}, {[idP]}, {[sharedboard]}, {[idPostParent]}, {[idUser]}));
	    
	    importTree id idP tree

	val tree = Root (0, startingFen, [], [])
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
		     
		     Room.send room (Position {State = (fen_to_state newFen),
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
	    Room.send room (Position {State = (fen_to_state row2.Position.Fen),
				      Old = (fen_to_state row3.Position.Fen), Id = idP,
				      Move = optS2S row2.Position.Move, MoveAlg = optS2S row2.Position.MoveAlg,
				      Previous = optI2I row2.Position.PreviousPositionId, Highlight = []})
	    
	  | SHighlight sq =>
	    room <- getRoom id;
	    Room.send room (Highlight sq)
	  | SComment txt =>
	    idC <- nextval commentSeq;
	    t <- now;
	    dml (INSERT INTO comment (Id, PositionId, Content, UserId, Sent) VALUES({[idC]}, {[id]}, {[txt]}, {[userId]}, {[t]} ));
	    room <- getRoom id;
	    Room.send room (Comment txt)
	  | SNewPost (optP, txt) =>
	    addPostF userId optP txt;
	    return ()
	  | SChangeName (id, txt) =>
	    dml (UPDATE post SET Nam = {[txt]} WHERE Id = {[id]});
	    room <- getRoom id;
	    Room.send room (ChangeName txt)

fun getTree id =
    treeAtOnce id
(*    tree4 id *)

fun doSpeak id line =	 
    rpc (speak id line)
		 
fun renderPostTree (id : int) (recTree : bool) : transaction xbody =
    let
	fun clean () =
	    return <xml></xml>
	    
        fun recurse (root : option int) =
            queryX' (SELECT * FROM post WHERE {eqNullable' (SQL post.ParentPostId) root} ORDER BY post.Id)
                    (fn r =>			
                        children <- (if recTree then
					 clean ()
				     else
					 recurse (Some r.Post.Id));
                        return <xml>
                          <tr><td><a link={postPage2 r.Post.Id ()}>{[show r.Post.Nam]}</a></td></tr>

                          {children}
                        </xml>)
    in
	content <- recurse (Some id);
	return <xml>
	  <div class="table-responsive">
	    <table class="bs-table table-striped table-sm">
	      <tr><th>Chapter</th></tr>
              { content }
	    </table>
	  </div>
	</xml>
    end

and postPage2 id () =
    current <- oneRow (SELECT post.Id, post.Nam, post.Room, post.RootPositionId, Position.Fen, PositionR.Fen
		       FROM post
			 JOIN position AS Position ON post.CurrentPositionId = Position.Id
			 JOIN position AS PositionR ON post.RootPositionId = PositionR.Id
		       WHERE post.Id = {[id]});
    postTree <- renderPostTree id False;
    cid <- fresh;
    ch <- Room.subscribe current.Post.Room;
    pname <- source current.Post.Nam;

    pgnTree <- getTree current.Post.Id;
    mutTree <- treeToMtree pgnTree;
    
    (boardy, pgnviewer, commentviewer) <- generate_board current.Position.Fen cid 60 True
							 (fn _ => getTree current.Post.Id)
							 (fn _ => getComments current.Post.Id )
							 (fn s => doSpeak current.Post.Id s)
							 (fn c => case c of
								      ChangeName t => set pname t
								    | _ => return ()) ch;
    commenttxt <- source "";
    newpostname <- source "";

    genPageU <xml>
	<div class={container}>
	  <div class={row}>
	    <ctextbox source={pname} />
	    <button value="Send" onclick={fn _ =>
					     txt <- get pname;
					     doSpeak id (SChangeName(id, txt))} />
	    </div>
	  <div class={row}>
	    
	    <div class={col_sm_2}>
		
	      <button value="Back" onclick={fn _ => doSpeak id SBack } />
		<button value="Fw" onclick={fn _ => doSpeak id SForward } />
		  <a link={downloadPost id}>download</a>

		  { postTree }

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
	  </xml> NMenu
   
and downloadPost id =
    let
	fun renderPgnN pgnN siblings forceAlg =
	    case pgnN of
		Node (idP, fen, move, moveAlg, comments, children) =>
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
		 Root (_, _, [], _) =>
		 "*"
	       | Root (_, _, (a :: siblings), _) => 
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

    idinvitecode <-fresh;
    idnam <- fresh;
    idpass <- fresh;
    idpassconf <- fresh;
	
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/auth.css" />
      </head>
      <body class="text-center">
	<div class="form-signin">
	  
	  <dyn signal={m <- signal cerr; return <xml>{[m]}</xml>}></dyn>

	  <dyn signal={i <- signal cinvitecode;
		       n <- signal cnam;
		       p <- signal cpass;
		       p2 <- signal cpassconf;
		       return <xml>
			 <active code={set cerr ""; return <xml/>} />
								   </xml>}></dyn>

	  <label class="sr-only" for={idinvitecode}>Invite Code</label>
	  <ctextbox id={idinvitecode} class="form-control form-signin-sep" placeholder="Invite Code" source={cinvitecode} />
	  <label class="sr-only" for={idnam}>Username</label>
	  <ctextbox source={cnam} class="form-control form-signin-sep" placeholder="Username" />
	  <label class="sr-only" for={idpass}>Password</label>
	  <cpassword source={cpass}  class="form-control form-signin-sep" placeholder="Password" />
	  <label class="sr-only" for={idpassconf}>Confirm Password</label>
	  <cpassword source={cpassconf}  class="form-control form-signin-sep" placeholder="Confirm Password" />

	  <button class="btn btn-lg btn-primary btn-block" onclick={fn _ =>
			      set cerr "";
			      invitecode <- get cinvitecode;
			      nam <- get cnam;
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
    u <- currUser ();
    case u of
	None => error <xml>Not authenticated</xml>
      | Some u' =>	
	id <- nextval inviteSeq;
	sent <- now;
	bytes <- Random.bytes 4;
	dml (INSERT INTO invite (Id, Email, UserId, InvitedId, Code, Sent, Status)
	     VALUES ({[id]}, {[r.To]}, {[u'.Id]}, {[None]}, {[Base64_FFI.encode bytes]}, {[sent]}, {[0]}));

	minv <- oneOrNoRows (SELECT invite.Code FROM invite WHERE invite.Id = {[id]});

	case minv of
	    None => error <xml>Unable to create invite, please try again</xml>
	  | Some inv => 
	    genPage <xml>
		<h2>Invite</h2>

		Code: <b>{[inv.Invite.Code]}</b> <br />

		Copy (right click on link - Copy Link Location) and send this link:
		
		<a link={createAccount inv.Invite.Code}>Link</a>
	    </xml> u' NMenu
    
and invites () =
    u <- currUser ();
    case u of
	None => redirect (url (index ()))
      | Some u' =>
	
	rows <- query (SELECT invite.Code, invite.InvitedId, invite.Email, invite.Status FROM invite WHERE invite.UserId = {[u'.Id]})
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
			  </xml>) <xml></xml>; (**)
	genPage
	    <xml>	      
		Send invite: you have x left
		<form>
		  <textbox{#To} />
		  <submit action={generateInvite} />
		</form>

		<h3>Invites sent</h3>
		<div class="table-responsive">
		  <table class="bs-table table-striped table-sm">
		    <tr>
		      <th>Invite Code</th>
		      <th>Email</th>
		      <th>Status</th>
		    </tr>
		    {rows}
		  </table>
		</div>
	    </xml> u' Invites

and handleTestUpload r =
    return <xml>
      <body>
	content
	{[Filetext_FFI.blobAsText (fileData r.Fil)]}
	<br />
	(*
	split:
	<br />
	{(case (split (Filetext_FFI.blobAsText (fileData r.Fil))) of
	      (a, b) =>
	      <xml>
		{List.foldr (fn i acc => <xml>ST {[i]} END<br /> {acc}</xml>) <xml></xml> a }
		<br />
		Rest
		<br />
		ST {[b]} END
		<br />

	      </xml>
	)}
	<br />
	 *)

	
	split2:
	<br />
	{
	 let
	     val a = pgnsToStrs (Filetext_FFI.blobAsText (fileData r.Fil))
	 in
	      <xml>
		{List.foldr
		     (fn i acc =>
			 <xml>ELEM ST {
			 List.foldr
			     (fn i acc => <xml>ST {[i]} END<br /> {acc}</xml>)
			     <xml></xml> i }
		       ELEM END<br /> {acc}</xml>)
		     <xml></xml> a }		
	      </xml>
	 end
	}
	<br />
	 

(*
	<br />
	test1
	{[show (strsub (Filetext_FFI.blobAsText (fileData r.Fil)) 0)]}

	<br />
	split
	{case (Pgnparse.split (Filetext_FFI.blobAsText (fileData r.Fil))) of
	      (lstr, str) =>
	      <xml><div>l str: {[show lstr]}</div><div>rest: {[str]}</div></xml>}
	
	<br />
	game test

	{case (Pgnparse.split (Filetext_FFI.blobAsText (fileData r.Fil))) of
	     (lstr, str) =>
	     <xml>
	       Decomposed:
	       {List.foldr (fn i2 acc2 =>
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
					       ) <xml></xml> (Nregexpgn.matchMoves lstr)}
	       <br/>
	       Game:
	       {[show (Pgnparse.stringLToGame lstr)]}
	     </xml>} *)
	<br />
	
	lines
	      {
	       List.foldr
		   (fn i acc => <xml><div>{

				List.foldr (fn i2 acc2 => <xml><div>{[i2]}</div> {acc2}</xml>) <xml></xml> i
				
				} </div> <div>+</div> {acc}</xml>) <xml></xml> (pgnsToStrs (Filetext_FFI.blobAsText (fileData r.Fil))) }
	      
	      <br />
	      lexems
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
				
				} </div> <div>+</div> {acc}</xml>) <xml></xml> (pgnsToStrs (Filetext_FFI.blobAsText (fileData r.Fil))) }

	      <br />
	      games
	      
	      {
	       List.foldr
		   (fn i acc => <xml><div>{[show i]} </div> {acc}</xml>) <xml></xml> (pgnsToGames (Filetext_FFI.blobAsText (fileData r.Fil))) }
	      
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
    genPageU c NMenu

and me () =
    u <- currUser ();
    case u of
	None => redirect (url (index ())) (* error <xml>Not authenticated</xml> *)
      | Some u' =>
	c <- userProfile u'.Id;
	genPage c u' Profile

and myPosts () =
    return <xml>
      <body>
	<h2>My posts</h2>
	
      </body>
    </xml>

and postsPage page =
    let
	val itemsPage = 10
	val offsetPage = page * itemsPage

	fun calc_max_pages total_items itemsPage =
	    ceil(float(total_items) / float(itemsPage))
	    
	fun make_first_link page =
	    if page = 0 then
		Inactive("First")
	    else
		Active("First", url (postsPage 0))
		
	fun make_previous_link page =
	    if page = 0 then
		Inactive("Previous")
	    else
		Active("Previous", url (postsPage (page - 1)))

	fun make_next_link page total_items =
	    let
		val max_pages = calc_max_pages total_items itemsPage
	    in
		if page = max_pages - 1 then
		    Inactive("Next")
		else
		    Active("Next", url (postsPage (page + 1)))	    
	    end

	fun make_last_link page total_items =
	    let
		val max_pages = calc_max_pages total_items itemsPage
	    in
		if page = max_pages - 1 then
		    Inactive("Last")
		else
		    Active("Last", url (postsPage (max_pages - 1)))	    
	    end

	fun make_page_links' minPage maxPages expected =
	    if expected = 0 then
		[]
	    else
		if expected > 0 then
		    if minPage >= maxPages then
			[]
		    else
			if minPage >= 0 then
			    Active (show (minPage + 1), url (postsPage minPage)) :: make_page_links' (minPage + 1) maxPages (expected - 1)
			else
			    make_page_links' (minPage + 1) maxPages (expected - 1)
		else
		    []
		    
		    
	fun make_page_links maxp page total_items =
	    let
		val max_pages = calc_max_pages total_items itemsPage
		val current = Current (show (page + 1))
	    in
		if page = 0 then
		    current :: (make_page_links' 1 max_pages (maxp + maxp))
		else
		    if page = max_pages - 1 then
			List.append (make_page_links' (page - (maxp + maxp)) page (maxp + maxp)) (current :: [])
		    else
			List.append (make_page_links' (page - maxp) page maxp)
				    (current :: make_page_links' (page + 1) max_pages maxp)
	    end

	fun make_link kind =
	    case kind of
		Active (text, u) => 
		<xml>
		  <li class="page-item">
		    <a class="page-link" href={u}>{[text]}</a>
		  </li>
		</xml>
	      | Current (text) =>
		<xml>
		  <li class="page-item bs-active">
		    <span class="page-link">{[text]}</span>
		  </li>
		</xml>
	      | Inactive (text) =>
		<xml>
		  <li class="page-item disabled">
		    <span class="page-link">{[text]}</span>
		  </li>
		</xml>	    

	fun make_links ls =
	    case ls of
		[] => <xml></xml>
	      | h :: t =>
		<xml>
		  {make_link h}
		  {make_links t}
		</xml>

		fun make_paginator total_items =
		    if total_items > itemsPage then
			<xml>
			  <ul class="pagination pagination-sm">
			    {make_link (make_first_link page)}
			    {make_link (make_previous_link page)}
			    {make_links (make_page_links 1 page total_items)}
			    {make_link (make_next_link page total_items)}
			    {make_link (make_last_link page total_items)}
			  </ul>
			</xml>
		    else
			<xml></xml>
			
    in
	muserId <- currUserId ();
	case muserId of
	    None => return (error <xml>not authenticated</xml>)
	  | Some userId =>
	    cc <- oneRow (SELECT COUNT( * ) AS N FROM post WHERE post.UserId = {[userId]} );
	    rows <- query (SELECT post.Id, post.Nam, post.Room, post.RootPositionId, Position.Fen, PositionR.Fen
			   FROM post
			     JOIN position AS Position ON post.CurrentPositionId = Position.Id
			     JOIN position AS PositionR ON post.RootPositionId = PositionR.Id
			   LIMIT {itemsPage} OFFSET {offsetPage} 
			  )
			  
			  (fn data acc =>		      
			      cid <- fresh;
			      ch <- Room.subscribe data.Post.Room;
			      (board, _, _) <- generate_board data.Position.Fen cid 20 False
							      emptyTree
							      (fn _ => return [])
							      (fn s => doSpeak data.Post.Id s)
							      emptyTopLevelHandler
							      ch;
			      return <xml>{acc}<tr>
				<td>{[data.Post.Nam]}</td>
				<td>{board}</td>
				<td>
				(*
				<form>
				 <submit action={postPage data.Post.Id} value="Enter"/>
										     </form>*)
				<form>
				(*<submit class="btn btn-success" action={postPage2 data.Post.Id} value="Enter Room"/> *)
				<a class="btn btn-success" link={postPage2 data.Post.Id ()}>Enter Room</a>
				</form>
				</td>
			      </tr>
			      </xml>)
			  <xml></xml>;
			  
			  genPageU <xml>
			    You have {[cc.N]} post{[if cc.N = 1 then
							""
						    else
							"s"]}
			    <a class="btn btn-primary" link={createPost ()}>Create</a>
			    <div class="table-responsive">
			      
			      {make_paginator cc.N}
			      
			      <table class="bs-table table-striped table-sm">
				<tr><th>Name</th><th>Board</th><th>Actions</th></tr>
				{rows}
			      </table>

			      {make_paginator cc.N}
			      
			    </div>
			  </xml> Posts
    end
		   
and allPosts () =
    postsPage 0
        
and createPost () =
    inPgn <- fresh;
    inNam <- fresh;
    inFile <- fresh;
    genPageU <xml>
      <form>
	<div class="form-row">
	  <div class="form-group col-md-6">
	    <label for={inNam}>Name</label>
	    <textbox{#Nam} id={inNam} class="form-control"/>
	  </div>
	</div>
	<div class="form-group">
	  <label for={inFile}>Upload pgn file</label>
	  <upload{#Fil} id={inFile} class="form-control-file"/>
	</div>
	<div class="form-group">
	  <label for={inPgn}>Paste pgn</label>
	  <textarea{#Pgn} id={inPgn} class="form-control"/>
	</div>
	<submit action={addPost} value="Create" />
	
      </form>
    </xml> NMenu


and addPost newPost =
    mIdUser <- currUserId ();
    case mIdUser of
	None => return (error <xml>not authenticated</xml>)
      | Some idUser =>
	let
	    fun importComments nidP (comments : list string) =
		case comments of
		    [] => return ()
		  | c :: t => 
		    cid <- nextval commentSeq;
		    n <- now;
		    dml (INSERT INTO comment (Id, PositionId, Content, UserId, Sent) VALUES ({[cid]}, {[nidP]}, {[c]}, {[idUser]}, {[n]}));
		    importComments nidP t

	    fun importChildren id idP fen children =
		let
		    val state = fen_to_state fen
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

					    importComments nidP comments;
					    importChildren id nidP nfen children2;
					    importChildrenAux t
					end
				end
				
		in
		    importChildrenAux children
		end
		
	    fun importTree id idP root =
		case root of
		    Root (_, fen, children, _) => 
		    dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[fen]},
													{[None]}, {[None]}, {[None]} ));
		    importChildren id idP fen children
		    
	    fun insertPost nam idUser parent tree =
		id <- nextval postSeq;    
		idP <- nextval positionSeq;
		sharedboard <- Room.create;
		
		dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId, UserId)
		     VALUES ({[id]}, {[nam]}, {[idP]}, {[idP]}, {[sharedboard]}, {[parent]}, {[idUser]}));
		
		importTree id idP tree;
		return id

	    fun getTitle tree =
		case tree of
		    Root (_, _, _, hdrs) =>
		    ((getH hdrs "White") ^ " vs " ^ (getH hdrs "Black"))

	    fun insertPosts idUser ls =
		case ls of
		    [] => insertPost newPost.Nam idUser None (Root (0, startingFen, [], []))
		  | h :: [] => insertPost newPost.Nam idUser None h
		  | h :: t =>
		    (* insert base post. *)
		    id <- nextval postSeq;    
		    idP <- nextval positionSeq;
		    sharedboard <- Room.create;
		    
		    dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId, UserId)
			 VALUES ({[id]}, {[newPost.Nam]}, {[idP]}, {[idP]}, {[sharedboard]}, {[None]}, {[idUser]}));

		    dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[startingFen]},
													{[None]}, {[None]}, {[None]} ));
		    
		    _ <- List.mapM (fn e => insertPost (getTitle e) idUser (Some id) e) ls;
		    
		    return id
		    
	    val szF = blobSize (fileData newPost.Fil)	   
		      
	in

	    if szF > 1000000 then
		return (error <xml>too big</xml>)
	    else
		if szF > 0 then
(*		    debug "file"; *)
		    id <- insertPosts idUser (pgnsToGames (Filetext_FFI.blobAsText (fileData newPost.Fil)));	    
		    redirect (url (postPage2 id ()))
		else
(*		    debug "raw text";
		    debug (case (split newPost.Pgn) of
			       (a, b) => "ls: " ^ (show a) ^ "rest: " ^ b); *)
		    (*			debug (show (pgnsToGames newPost.Pgn));*)
		    id <- insertPosts idUser (pgnsToGames newPost.Pgn);	    
		    redirect (url (postPage2 id ()))

	end

and genPageU content cur =
    u' <- currUser ();
    case u' of
	None => redirect (url (index ()))
      | Some u => genPage content u cur
    
and genPage content u cur =
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/exp.css" />
	<link rel="stylesheet" type="text/css" href="/bodyn.css" />
      </head>
      <body>
	<nav class="navbar navbar-expand-md navbar-dark bg-dark fixed-top">
	  { generateMenu u cur }
	</nav>
	<main class="container">
	  <div>
	    { content }
	  </div>
	</main>
      </body>
    </xml>

and generateMenu u current =
    <xml>
      <ul class="navbar-nav mr-auto">
	<li class="nav-item">
	  {case current of
	       Home => <xml><span class="nav-link bs-active">Home</span></xml>
	     | _ => <xml><a class="nav-link" link={index ()}>Home</a></xml> }
	</li>
	<li class="nav-item">
	  {case current of
	       Posts => <xml><span class="nav-link bs-active">Posts</span></xml>
	     | _ => <xml><a class="nav-link" link={allPosts ()}>Posts</a></xml> }
	</li>
	<li class="nav-item">
	  {case current of
	       Profile => <xml><span class="nav-link bs-active">Profile</span></xml>
	     | _ => <xml><a class="nav-link" link={me ()}>Profile</a></xml> }
	</li>
	<li class="nav-item">
	  {case current of
	       Invites => <xml><span class="nav-link bs-active">Invites</span></xml>
	     | _ => <xml><a class="nav-link" link={invites ()}>Invites</a></xml> }
	</li>
      </ul>

      <ul class="navbar-nav">
	<li class="nav-item"><span class="navbar-brand">{[u.Nam]}</span></li>
	<li class="nav-item">
	  <a class="nav-link" link={logoff()}>Logoff</a>	     
	</li>
      </ul>
    </xml>

and index_on u =
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/exp.css" />
	<link rel="stylesheet" type="text/css" href="/bodyj.css" />
      </head>
      <body>
	<nav class="navbar navbar-expand-md navbar-dark bg-dark fixed-top">
	    { generateMenu u Home }
	</nav>
	<main>
	  <div class="jumbotron">
	    <div class="container">
	      <h1>Turtle corner</h1>
	      <p class="lead">In construction</p>
	    </div>
	  </div>
	  <div class="container">
	    <div class="row">
	      <div class="col-md-4">
		<h2>Share games</h2>
		<a link={createPost ()} class="btn btn-primary">Upload a Game</a>
	      </div>
	      <div class="col-md-4">
		<h2>Study</h2>
		<a link={allPosts ()} class="btn btn-primary">All Games</a>
	      </div>
	      <div class="col-md-4">
		<h2>Relax</h2>
		<p>Yeah ... just relax</p>
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
	  <textbox{#Nam} id={userid} class="form-control form-signin-sep" placeholder="User" />
	  <label class="sr-only" for={passid}>Pass</label>
	  <password{#Pass} id={passid} class="form-control form-signin-sep" placeholder="Password" />
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

and handleClockF r =
    let
	fun validate r : option (int * int)=	    
	    baseTime <- read r.BaseTime;
	    increment <- read r.Increment;
	    Some (baseTime, increment)
    in
	case (validate r) of
	    None => error <xml>Invalid data</xml>
	  | Some (bt, i) => redirect (url (clock ({BaseTime =bt, Increment = i, ResetIfUnexpired = False} :: []) ))
    end

and handleClockB r =
    let
	fun validate r : option (int * int) =
	    baseTime <- read r.BaseTime;
	    periods <- read r.Periods;
	    Some (baseTime, periods)

	fun genByoyomi t s =
	    case t of
		0 => []
	      | _ => s :: (genByoyomi (t - 1) s)
    in
	case (validate r) of
	    None => error <xml>Invalid data</xml>
	  | Some (bt, p) => redirect (url (clock (genByoyomi p {BaseTime =bt, Increment = 0, ResetIfUnexpired = True}) ))
    end
    
and clockSetup () =
    return <xml>
      <body>
	<div>
	  fischer
	  <form>
	    <textbox{#BaseTime} />
	    <textbox{#Increment} />
	    <submit action={handleClockF} value="Clock F" />
	  </form>
	</div>

	<div>
	  byo yomi
	  <form>
	    <textbox{#BaseTime} />
	    <textbox{#Periods} />
	    <submit action={handleClockB} value="Clock B" />
	  </form>
	</div>
      </body>
    </xml>
    
and clock ltc =
    case ltc of
	[] => error <xml>No control setup</xml>
      | tc :: tcTail => 
	let
	    fun minToTiny m =
		m * 60

	    fun tinyToStr sec =
		let
		    val min = sec / 60
		    val secP = sec - (min * 60)
		in
		    (show min) ^ ":" ^ (if secP < 10 then "0" else "") ^ (show secP)
		end

	    fun renderP t =		
		let
		    val l = 1 + (List.length t)
		    val str =  if (l > 1) then
				   (show l)
			       else
				   ""
		in
		    return <xml>
		      {[ str ]}
		    </xml>
		end
		
	    fun renderT t =
		return <xml>
		  {[tinyToStr t]}
		</xml>
	in
	    wtime <- source (minToTiny tc.BaseTime);
	    wrest <- source tcTail;
	    btime <- source (minToTiny tc.BaseTime);
	    brest <- source tcTail;
	    turn <- source Pend;

	    let

		fun reset () =
		    set wtime (minToTiny tc.BaseTime);
		    set wrest tcTail;
		    set btime (minToTiny tc.BaseTime);
		    set brest tcTail;
		    set turn Pend
		    
		fun plToSig c =
		    case c of
			CWhite => return (wtime, wrest)
		      | CBlack => return (btime, brest)

		fun tap (src : clockSide) =
		    let
			fun tapTail tc =
			    current <- get turn;
			    case current of
				Pend =>
				set turn (Run (otherC src))
			      | Run c' =>
				if (eqC c' src) then
				    (pl, ps) <- plToSig c';
				    t <- get pl;
				    (if (t = 0) then (* consume time slot or lose on time *)
					 ls <- get ps;
					 case ls of
					     [] => 
					     set turn (Lost c')
					   | h :: tl =>
					     set ps tl;
					     set pl (minToTiny h.BaseTime)
				     else 
					 if tc.ResetIfUnexpired then (* reset time slot, if appliable *)
					     set pl (minToTiny (tc.BaseTime))
					 else
					     set pl (t + tc.Increment)); 
				    set turn (Run (otherC src))
				else
				    return ()
			      | Lost _ =>
				return ()
		    in
			tapTail tc
		    end 

		fun tickInner () =
		    current <- get turn;
		    case current of
			Pend => setTimeout tickInner 1000
		      | Run c' =>
			(sg, ps) <- plToSig c';
			t <- get sg;
			set sg (t - 1);
			t <- get sg;
			if (t = 0) then (* consume time slot or lose on time *)
			    ls <- get ps;
			    case ls of
				[] =>
				set turn (Lost c');
				setTimeout tickInner 1000
			      | h :: t  =>
				set ps t;
				set sg (minToTiny h.BaseTime);
				setTimeout tickInner 1000
			else
			    setTimeout tickInner 1000			    
			    
		      | Lost _ => setTimeout tickInner 1000

		fun tick () =
		    tickInner ();
		    return <xml></xml>
		    
	    in
		return <xml>
		  <head>
		    <link rel="stylesheet" type="text/css" href="/clock.css" />
		  </head>
		  <body class="full">
		    <div class="half black-player flip">
		      <div class="content" onclick={fn _ => tap CBlack}>
			<dyn signal={b <- signal btime; renderT b}></dyn>
			<dyn signal={br <- signal brest; renderP br}></dyn>
		      </div>
		    </div>
		    <div class="commands">
		      <div class="content">
			<div class="cmd-button">
			  P
			</div>
			<div class="cmd-button" onclick={fn _ => reset ()}>
			  R
			</div>
			<div class="cmd-button" onclick={fn _ => redirect (url (clockSetup ()))}>
			  S
			</div>
		      </div>
		    </div>
		    <div class="half white-player">
		      <div class="content" onclick={fn _ => tap CWhite}>
			<dyn signal={w <- signal wtime; renderT w}></dyn>
			<dyn signal={wr <- signal wrest; renderP wr}></dyn>
		      </div>
		    </div>
		    <active code={tick ()}></active>
		  </body>
		</xml>
	    end
	end

and index () =
    u <- currUser ();
    case u of
	None =>
	index_login ()	
      | Some u => 
	index_on u
