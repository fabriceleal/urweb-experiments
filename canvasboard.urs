
functor Make (M: sig
		  val postId : option int
		  val canvasW: int
		  val canvasH : int
	      end) : sig
    
    val generateBoard : id -> xml
end
