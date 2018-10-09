
val main : unit -> transaction page(**)

val invites: unit -> transaction page
		     
val index : unit -> transaction page

val createPost : unit -> transaction page
			 
val allPosts : unit -> transaction page

val downloadPost: int -> transaction page

val testResponsive : unit -> transaction page
			     
val testUpload : unit -> transaction page
			 
val testMk : unit -> transaction page
		     
val createAccount : string -> transaction page
			    
val turtle : int -> transaction page

val me : unit -> transaction page
		 
val allTurtles : unit -> transaction page
			 
val myPosts : unit -> transaction page
		      
