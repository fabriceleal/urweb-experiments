val index : unit -> transaction page
		    
val invites: unit -> transaction page
 
(*
val createPost : unit -> transaction page
*)			 
val allPosts : unit -> transaction page

val downloadPost: int -> transaction page

val testResponsive : unit -> transaction page

(*
val fullBootstrap : unit -> transaction page

val fullBootstrap2 : unit -> transaction page

val fullBootstrap3 : unit -> transaction page *)
			     
val testUpload : unit -> transaction page
			 
val testMk : unit -> transaction page
		     
val createAccount : string -> transaction page
			    
val turtle : int -> transaction page

val me : unit -> transaction page
		 
val allTurtles : unit -> transaction page
			 
val myPosts : unit -> transaction page

val logoff : unit -> transaction page
		     
val clockSetup : unit -> transaction page

type timecontrol =
     {BaseTime : int, Increment: int, ResetIfUnexpired: bool}

type lsTimecontrol = list timecontrol
			 
val clock : lsTimecontrol -> transaction page

val chess : string -> transaction page
		    
val shogi : unit -> transaction page

val weiqi : unit -> transaction page
