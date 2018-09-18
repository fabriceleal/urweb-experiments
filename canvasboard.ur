
functor Make (M: sig
		  val postId : option int
		  val canvasW: int
		  val canvasH : int
	      end) = struct

    
    
    fun generateBoard c =
	
	
	<xml>
	  <canvas id={c} width={M.canvasW} height={M.canvasH}>
	  </canvas>
	</xml>
end
