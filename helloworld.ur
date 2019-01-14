open Types
open Posts
open Database
open Session
open Canvas_FFI
open Chess
open Chessdb
open Weiqi
open Bootstrap4
open Pgnparse
open Nmarkdown
open Game
open Sharedboard
     
val maxFileSize = 1000000
		  
(* login page *)
style form_signin
style form_signin_sep
(*
open Pgn.Make(struct
		  con id = #Id
		  con parent = #PreviousPositionId
		  val tab = position
	      end)
*)
datatype inviteStatus =
	 Sent
       | Accepted
       | Cancelled

fun cryptPass passS =
    salt <- Random.bytes 64;
    let
	val saltEncoded = Base64_FFI.encode(salt)
	val passraw = passS ^ saltEncoded
	val pass = Hash.sha512 (textBlob passraw)
    in
	return (pass, saltEncoded)
    end
      
fun insertUserWithId userid nam passS =
    (pass, saltEncoded) <- cryptPass passS;
    dml (INSERT INTO user (Id, Nam, Pass, Salt) VALUES ({[userid]}, {[nam]}, {[pass]}, {[saltEncoded]}))

fun updateUserWithPass userid passS =
    (pass, saltEncoded) <- cryptPass passS;
    dml (UPDATE user SET Pass = {[pass]}, Salt = {[saltEncoded]} WHERE Id = {[userid]})

task initialize = fn () =>
    b <- nonempty rootAdmin;
    if b then
        return ()
    else
	insertUserWithId 0 "root" "root";
	dml (INSERT INTO rootAdmin (Id) VALUES (0))

datatype pageKind =
	 Active of string * url
       | Current of string
       | Inactive of string

datatype menuKind =
	 NMenu
       | Home
       | ChessPage
       | ShogiPage
       | WeiqiPage
       | MathPage
       | Posts
       | Profile
       | AllUsers
       | Invites
     
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
    p <- oneRow (SELECT post.Id, post.Nam, post.PostType FROM post WHERE post.Id = {[id]});
    pname <- source p.Post.Nam;
    renderPost p (fn postTree boardy viewer commentviewer =>		     
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
			       {viewer}
			       
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
			 </xml> NMenu) renderPostTree doSpeak (fn c => case c of
									     MChangeName t => set pname t
									   | _ => return ())

   
and downloadPost id =
    let
	fun renderPgnN pgnN siblings forceAlg =
	    case pgnN of
		Node (idP, state, move, moveAlg, comments, children) =>
		let
		    val rest = case children of
				   [] => ""
				 | a :: siblings' =>  renderPgnN a siblings' (any siblings)

		    val siblingsRender = case siblings of
					     [] => ""
					   | _ :: _ =>
					     (List.foldl (fn rc acc => acc ^ " (" ^ (renderPgnN rc [] True) ^ ") ") "" siblings)
		in
		    (moveToAlgebraic state (str_to_move move) moveAlg forceAlg) ^ siblingsRender ^ " " ^ rest
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

and logonUserRow r' passw =
    let
	val hashed = Hash.sha512(textBlob(passw ^ r'.User.Salt))
    in
	hashed = r'.User.Pass
    end

and logon r =    
    ro <- oneOrNoRows (SELECT user.Id, user.Pass, user.Salt FROM user WHERE user.Nam = {[r.Nam]});
    case ro of
	None => error <xml>Wrong user or pass!</xml>
      | Some r' =>
	if (logonUserRow r' r.Pass) then
	    setCookie login {Value = {Id=r'.User.Id}, Secure=False, Expires = None};
	    redirect (url (index ()))
	else
	    redirect (url (index_login "Wrong user or pass!"))
	
and index_login err =
    userid <- fresh;
    passid <- fresh;
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/auth.css" />
      </head>
      <body class="text-center">			
	<form class="form-signin">
	  {if err <> "" then
	     <xml><div class="row">Error: {[err]}</div></xml>
	 else
	     <xml></xml>}
	  <label class="sr-only" for={userid}>User</label>
	  <textbox{#Nam} id={userid} class="form-control form-signin-sep" placeholder="User" />
	  <label class="sr-only" for={passid}>Pass</label>
	  <password{#Pass} id={passid} class="form-control form-signin-sep" placeholder="Password" />
	  <submit class="btn btn-lg btn-primary btn-block" action={logon} value="Sign In" />
	</form>
      </body>
    </xml>

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

and validateAndUpdatePass u =
    muserId <- currUserId ();
    case muserId of
	None => return (Some "session expired")
      | Some userId =>
	ro <- oneOrNoRows (SELECT user.Id, user.Pass, user.Salt FROM user WHERE user.Id = {[userId]});
	(case ro of
	    None => return (Some "user not found?!")
	  | Some r' =>
	    if (logonUserRow r' u.OldPass) then
		if (validatePasswordPolicy u.NewPass) then
		    (case (u.NewPass = u.ConfPass) of
			True => 
			updateUserWithPass userId u.NewPass;
			return None
		      | False => 
			return (Some "passwords font match"))
		else
		    return (Some "password doesnt match policy: at least 6 characters long")
	    else
		return (Some "old password doesnt match"))

and validateAndCreate u =
    vRes <- validateNewUser u;
    case vRes of
	None =>
	idU <- nextval userSeq;
	dml (UPDATE invite SET InvitedId = {[Some idU]}, Status = 1 WHERE Code = {[u.InviteCode]} );
	insertUserWithId idU u.Bleh u.Blah;
	return None
      | _ => return vRes
	
and createAccount invCode =    	
    cinvitecode <- source invCode;
    cnam <- source "";
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
	    </xml> u NMenu

and allTurtles () =
    u <- currUser ();
    case u of
	None => redirect (url (index ()))
      | Some u' =>
	rows <- query (SELECT User.Id, User.Nam
		       FROM user AS User 
		      )
		      (fn data acc =>
			  inviter <- oneOrNoRows (SELECT user.Id, user.Nam
						  FROM invite JOIN user ON invite.UserId = user.Id
						  WHERE invite.InvitedId = {[Some data.User.Id]});
			  return <xml>
			    {acc}
			    <tr>
			      <td><a link={turtle data.User.Id}>{[data.User.Nam]}</a></td>
			      <td>{case inviter of
				       None => <xml></xml>
				     | Some r => <xml><a link={turtle r.User.Id}>{[r.User.Nam]}</a></xml>}</td>
			      <td><a link={postsPage data.User.Id 0}>Posts</a></td>
			    </tr>			    
			    </xml>) <xml></xml>;
		      genPage <xml>
			<div class="table-responsive">
			  <table class="bs-table table-striped table-sm">

			    <tr><th>Name</th><th>Inviter</th><th>Posts</th></tr>
			    {rows}
			  </table>
			</div>
		      </xml> u AllUsers
		
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
			  </xml>) <xml></xml>;
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
	    </xml> u Invites

and handleTestUpload r =
    return <xml>
      <body>
	content
	{[Filetext_FFI.blobAsText (fileData r.Fil)]}
	<br />
	
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
	
and profileAdminSection id =
    me <- currUserIsAdmin ();
    cc <- oneRow (SELECT COUNT( * ) AS N FROM post WHERE {eqNullable' (SQL post.ParentPostId) None} AND post.UserId = {[id]} );
    if me then
	return <xml>
	  <div class="row">
	    <div class="col-md-4">
	      
	      <h4>Admin section</h4>
	      <div>{[show cc.N]} post{[if cc.N = 1 then "" else "s"]}</div>
	      <div><a link={postsPage id 0}>Posts</a></div>
	    </div>
	  </div>
	</xml>
    else
	return <xml></xml>
	
and userProfile id =
    me <- currUserId ();
    currUser <- oneRow (SELECT user.Id, user.Nam FROM user WHERE user.Id = {[id]});
    case me of
	None => error <xml>Not authenticated</xml>
      | Some u' =>
	let
	    val isMe = u' = id
	in
	    admSection <- profileAdminSection id;
	    isAdmin <- userIsAdmin id;
	    c_oldpass <- source "";
	    c_newpass <- source "";
	    c_confpass <- source "";
	    c_err <- source "";
	    c_ok <- source "";
	    return <xml>	      
	      <div class="container">	
		<h3>{[case isMe of
			  True => currUser.User.Nam ^ " (Me)"
			| False => currUser.User.Nam]}</h3>

		{if isAdmin then
		     <xml><div><b>Administrator</b></div></xml>
		 else
		     <xml></xml>}

		<div class="row">
		  {if isMe then
		       <xml>
			 <div class="col-md-3">

			     <h4>Change password</h4>
			     <dyn signal={m <- signal c_ok; return <xml>{[m]}</xml>}></dyn>
			     <dyn signal={m <- signal c_err; return <xml>{[m]}</xml>}></dyn>

			     <div class="form-group">
			       <label>Old Password</label>
			       <cpassword class="form-control form-control-sm" source={c_oldpass} />
			     </div>
			     <div class="form-group">
			       <label>New Password</label>
			       <cpassword class="form-control form-control-sm" source={c_newpass} />
			     </div>
			     <div class="form-group">
			       <label>Confirm Password</label>
			       <cpassword class="form-control form-control-sm" source={c_confpass} />
			     </div>
			     <button class="btn btn-sm btn-primary" value="Submit" onclick={fn _ =>
											       set c_err "";
											       oldpass <- get c_oldpass;
											       newpass <- get c_newpass;
											       confpass <- get c_confpass;
											       res <- rpc (validateAndUpdatePass {OldPass=oldpass, NewPass=newpass, ConfPass =confpass});						       
											       case res of
												   None => set c_ok "Password changed"
												 | Some err => set c_err err
											   } />
											     
		       </div>
		</xml>
		   else
		       <xml></xml>
		  }
		  
		  </div>
		  
		  {admSection}
	    </div>
    </xml>
	end
	
and turtle id =    
    c <- userProfile id;
    genPageU c NMenu

and me () =
    u <- currUser ();
    case u of
	None => redirect (url (index ())) (* error <xml>Not authenticated</xml> *)
      | Some u' =>
	c <- userProfile u'.Id;
	genPage c u Profile

and myPosts () =
    return <xml>
      <body>
	<h2>My posts</h2>
	
      </body>
    </xml>

and pageableView page header sql_itemscounter generate_rows mkUrl =
    let
	val itemsPage = 10
	val offsetPage = page * itemsPage

	fun calc_max_pages total_items itemsPage =
	    ceil(float(total_items) / float(itemsPage))
	    
	fun make_first_link page =
	    if page = 0 then
		Inactive("First")
	    else
		Active("First", mkUrl 0) (*url (postsPage 0) *)
		
	fun make_previous_link page =
	    if page = 0 then
		Inactive("Previous")
	    else
		Active("Previous", mkUrl (page - 1))
		
	fun make_next_link page total_items =
	    let
		val max_pages = calc_max_pages total_items itemsPage
	    in
		if page = max_pages - 1 then
		    Inactive("Next")
		else
		    Active("Next", mkUrl (page + 1))
	    end

	fun make_last_link page total_items =
	    let
		val max_pages = calc_max_pages total_items itemsPage
	    in
		if page = max_pages - 1 then
		    Inactive("Last")
		else
		    Active("Last", mkUrl (max_pages - 1))	    
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
			    Active (show (minPage + 1), mkUrl minPage) :: make_page_links' (minPage + 1) maxPages (expected - 1)
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
	    cc <- oneRow sql_itemscounter;
	    rows <- generate_rows itemsPage offsetPage;
	    
	    return (<xml>
	      
	      <div class="table-responsive">
		
		{make_paginator cc.N}
		
		<table class="bs-table table-striped table-sm">
		  {header}
		  {rows}
		</table>

		{make_paginator cc.N}
		
	      </div>
	    </xml>, cc)
    end

and pageablePosts userId page =
    pageableView page <xml><tr><th>Name</th><th>Board</th><th>Actions</th></tr></xml>
		 (SELECT COUNT( * ) AS N
		  FROM post WHERE {eqNullable' (SQL post.ParentPostId) None} AND post.UserId = {[userId]} )
		 (fn itemsPage offsetPage => query (SELECT post.Id, post.Nam, post.Room, post.RootPositionId, Position.Fen, PositionR.Fen
						    FROM post
						      JOIN position AS Position ON post.CurrentPositionId = Position.Id
						      JOIN position AS PositionR ON post.RootPositionId = PositionR.Id
						    WHERE {eqNullable' (SQL post.ParentPostId) None} AND post.UserId = {[userId]}
									LIMIT {itemsPage} OFFSET {offsetPage} 
						   )
						   
						   (fn data acc =>		      
						       cid <- fresh;
						       ch <- ChessRoom.subscribe data.Post.Room;
						       (board, _, _, _) <- generate_board data.Position.Fen cid 20 False
											  emptyTree
											  (fn _ => return [])
											  (fn s => doSpeak data.Post.Id s)
											  emptyTopLevelHandler
											  emptyOnGameState
											  (Some ch);
						       return <xml>{acc}<tr>
							 <td>{[data.Post.Nam]}</td>
							 <td>{board}</td>
							 <td>
							   <form>
							     <a class="btn btn-success" link={postPage2 data.Post.Id ()}>Enter Room</a>
							   </form>
							 </td>
						       </tr>
						       </xml>)
						   <xml></xml>)
		 (fn page => url (postsPage userId page))
    
and postsPage userId page =
    muserId <- currUserId ();
    case muserId of
	None => return (error <xml>not authenticated</xml>)
      | Some _ =>
	(widget, cc) <- pageablePosts userId page;
	genPageU <xml>
	  You have {[cc.N]} post{[if cc.N = 1 then
				      ""
				  else
				      "s"]}
	  
	  <a class="btn btn-primary" link={createPost ()}>Create</a>

	  {widget}
	</xml> Posts
	
    
and allPosts () =
    muserId <- currUserId ();
    case muserId of
	None => return (error <xml>not authenticated</xml>)
      | Some userId =>
	postsPage userId 0

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
	<div>
	  <radio{#Typ}>
	    <li><radioOption value="0" />Chess</li>
	    <li><radioOption value="1" />Weiqi</li>
	  </radio>
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
		    Root (_, st, children, _) =>
		    let
			val fen = state_to_fen st
		    in
			dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[fen]},
													    {[None]}, {[None]}, {[None]} ));
			importChildren id idP fen children
		    end
		    
	    fun insertPost nam typ idUser parent tree =
		id <- nextval postSeq;    
		idP <- nextval positionSeq;
		sharedboard <- ChessRoom.create;
		
		dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId, UserId, PostType)
		     VALUES ({[id]}, {[nam]}, {[idP]}, {[idP]}, {[sharedboard]}, {[parent]}, {[idUser]}, {[typ]}));
		
		importTree id idP tree;
		return id

	    fun getTitle tree =
		case tree of
		    Root (_, _, _, hdrs) =>
		    ((getH hdrs "White") ^ " vs " ^ (getH hdrs "Black"))

	    fun insertPosts idUser ls =
		case ls of
		    [] => insertPost newPost.Nam (optStrToTyp newPost.Typ) idUser None (Root (0, fen_to_state startingFen, [], []))
		  | h :: [] => insertPost newPost.Nam (optStrToTyp newPost.Typ) idUser None h
		  | h :: t =>
		    (* insert base post. *)
		    id <- nextval postSeq;    
		    idP <- nextval positionSeq;
		    sharedboard <- ChessRoom.create;
		    
		    dml (INSERT INTO post (Id, Nam, RootPositionId, CurrentPositionId, Room, ParentPostId, UserId, PostType)
			 VALUES ({[id]}, {[newPost.Nam]}, {[idP]}, {[idP]}, {[sharedboard]}, {[None]}, {[idUser]}, {[optStrToTyp newPost.Typ]}));

		    dml (INSERT INTO position (Id, PostId, Fen, Move, MoveAlg, PreviousPositionId ) VALUES ({[idP]}, {[id]}, {[startingFen]},
													{[None]}, {[None]}, {[None]} ));
		    
		    _ <- List.mapM (fn e => insertPost (getTitle e) (optStrToTyp newPost.Typ) idUser (Some id) e) ls;
		    
		    return id
		    
	    val szF = blobSize (fileData newPost.Fil)	   
		      
	in

	    if szF > maxFileSize then
		return (error <xml>too big</xml>)
	    else
		if szF > 0 then
		    id <- insertPosts idUser (pgnsToGames (Filetext_FFI.blobAsText (fileData newPost.Fil)));	    
		    redirect (url (postPage2 id ()))
		else
		    id <- insertPosts idUser (pgnsToGames newPost.Pgn);	    
		    redirect (url (postPage2 id ()))

	end

and genPageU content cur =
    u' <- currUser ();
    genPage content u' cur
    
and genPage content u cur =
    genPageT (fn _ => return content) u cur []

and genPageT contentT u cur extraLinks =
    let
	fun expandLinks links =
	    case links of
		[] => <xml></xml>
	      | h :: t =>
		<xml>
		  <link rel="stylesheet" type="text/css" href={h} />
		  { expandLinks t }
		</xml>
    in
	content <- contentT ();
	return <xml>
	  <head>
	    <link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	    <link rel="stylesheet" type="text/css" href="/exp.css" />
	    <link rel="stylesheet" type="text/css" href="/bodyn.css" />
	    { expandLinks extraLinks }
	    { generateTitle cur }
	  </head>
	  <body>
	    <nav class="navbar navbar-expand-md navbar-dark bg-dark fixed-top">
	      { generateMenu u cur }
	    </nav>
	    <main class="container">	  
	      { content }
	    </main>
	  </body>
	</xml>
    end
and generateTitle _ =
    <xml>
      <title>Turtle Corner</title>
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
	  <a class="nav-link" link={chess ""}>Chess</a>
	</li>
	<li class="nav-item">
	  {case current of
	       ShogiPage => <xml><span class="nav-link bs-active">Shogi</span></xml>
	     | _ => <xml><a class="nav-link" link={shogi ""}>Shogi</a></xml> }
	</li>
	<li class="nav-item">
	  {case current of
	       WeiqiPage => <xml><span class="nav-link bs-active">Weiqi</span></xml>
	     | _ => <xml><a class="nav-link" link={weiqi ""}>Weiqi</a></xml> }
	</li>
	<li class="nav-item">
	  {case current of
	       MathPage => <xml><span class="nav-link bs-active">Math</span></xml>
	     | _ => <xml><a class="nav-link" link={math ""}>Math</a></xml> }
	</li>
	{case u of
	     None => <xml></xml>
	   | Some _ =>
	     <xml>
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
		      AllUsers => <xml><span class="nav-link bs-active">Turtles</span></xml>
		    | _ => <xml><a class="nav-link" link={allTurtles ()}>Turtles</a></xml> }
	       </li>
	       <li class="nav-item">
		 {case current of
		      Invites => <xml><span class="nav-link bs-active">Invites</span></xml>
		    | _ => <xml><a class="nav-link" link={invites ()}>Invites</a></xml> }
	       </li>
	     </xml>}
      </ul>

      {case u of
	   None => <xml></xml>
	 | Some u' =>
	   <xml>
	     <ul class="navbar-nav">
	       <li class="nav-item"><span class="navbar-brand">{[u'.Nam]}</span></li>
	       <li class="nav-item">
		 <a class="nav-link" link={logoff()}>Logoff</a>	     
	       </li>
	     </ul>
	   </xml>
      }
    </xml>

and index_on u =
    return <xml>
      <head>
	<link rel="stylesheet" type="text/css" href="/bootstrap.min.css" />
	<link rel="stylesheet" type="text/css" href="/exp.css" />
	<link rel="stylesheet" type="text/css" href="/bodyj.css" />
	{ generateTitle Home }
      </head>
      <body>
	<nav class="navbar navbar-expand-md navbar-dark bg-dark fixed-top">
	    { generateMenu u Home }
	</nav>
	<main>
	  <div class="jumbotron">
	    <div class="container">
	      <p class="lead"></p>
	    </div>
	  </div>

	  <div class="container">
	    <div class="row">
	      {case u of
		   None => <xml>
		     <div class="col-md-4">
		       <a link={index_login ""} class="btn btn-primary">Reserved Area</a>
		     </div>
		   </xml>
		 | Some _ =>
		   <xml>

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
		     
		   </xml>}
	    </div>
	  </div>
	</main>
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

and index () =
    u <- currUser ();
    index_on u

and chess f =
    u <- currUser ();
    genPageT (fn _ =>
		 let
		     val start = (if f = "" then
				      startingFen
				  else
				      f)
		 in
		     cid <- fresh;
		     inputFen <- source start;
		     
		     (board, _, _, _) <- generate_board start cid 67 True
						     emptyTree
						     (fn _ => return [])
						     (fn s => return ())
						     emptyTopLevelHandler
						     (fn st => set inputFen (state_to_fen st))
						     None;
		     return <xml>
		       <div class={row}>
			 <div class={col_sm_6}>
			   {board}
			 </div>
			 <div class={col_sm_6}>
			   <div class={form_group}>
			     <ctextbox class={form_control} source={inputFen} />
			     <button class="btn btn-primary btn-sm" value="Set fen" onclick={fn _ => str <- get inputFen; redirect (url (chess str))} />
			     </div>
			   </div>
			 </div>
		       </xml>
		 end) u ChessPage []
    
and shogi _ =
    u <- currUser ();
    editor <- SShogi.editor {Tree = SShogi.emptyGame (SShogi.startingPosition ()), OnPositionChanged = (fn _ => return ())};
    genPageT (fn _ => return editor.Ed) u ShogiPage []
    
and weiqi f =
    u <- currUser ();
    genPageT (fn _ =>
		 let
		     val start = (if f = "" then
				      SWeiqi.startingPosition ()
				  else
				      SWeiqi.sToP f)
		 in
		     inputFen <- source (SWeiqi.pToS start);
		     editor <- SWeiqi.editor {
			       Tree = SWeiqi.emptyGame start,
			       OnPositionChanged = (fn p => set inputFen (SWeiqi.pToS p) (*;
			       alert (SWeiqi.test p*))};
		     return <xml>
		       <div class={row}>
			 <div class={col_sm_6}>
			   {editor.Ed}
			 </div>
			 <div class={col_sm_6}>
			   <div class={form_group}>
			     <ctextbox class={form_control} source={inputFen} />
			     <button class="btn btn-primary btn-sm" value="Set fen" onclick={fn _ => str <- get inputFen;
												redirect (url (weiqi str))} />
			     </div>
			   </div>
			 </div>		   
		       </xml>
		 end) u WeiqiPage []

and math (e : string) =
    u <- currUser ();
    genPageT (fn _ =>
		 
		 inp <- source e;
		 ddid <- fresh;
		 let
		     fun tick mj =
			 let
			     fun inner _ =
				 t <- get inp;
				 Mathjax.typesetcontent mj t ddid;
				 setTimeout inner 1000;
				 return ()
			 in
			     inner ()
			 end
		 in
		     return <xml>
		       <div class="row">
			 <div class={col_sm_6}>
			   <div id={ddid}>
			   </div>
			 </div>
			 <div class={col_sm_6}>
			     <active code={mj <- Mathjax.load ();
					   tick mj;
					   return <xml><ctextarea class="form-control" source={inp} /></xml>}>
			   </active>		   

		       </div>
		     </div>		   
    </xml>
		 end
	     ) u MathPage ((bless "/math.css") :: [])

    
(*** clock ***)
	      
(* clock page *)
style white_player
style black_player
style half
style full
style content
style commands
style flip
style cmd_button

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


fun handleClockF r : transaction page =
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

and handleClockB r : transaction page =
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
    
and clockSetup () : transaction page =
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
    
and clock ltc : transaction page =
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
