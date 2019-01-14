open Database

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
 
fun userIsAdmin id =
    row <- oneRow (SELECT COUNT( * ) AS N FROM rootAdmin WHERE rootAdmin.Id = {[id]});
    return (row.N > 0)
		  
fun currUserIsAdmin () =
    ro <- getCookie login;
    case ro of
	None => return False
      | Some r =>
	userIsAdmin r.Id
