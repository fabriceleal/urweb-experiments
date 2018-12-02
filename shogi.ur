
datatype player =
Sente (* black *)
| Gote  (* white *)

datatype piece = SenteKing | SenteRook | SenteBishop | SenteGold | SenteSilver | SenteKnight | SenteLance | SentePawn
	       | GoteKing | GoteRook | GoteBishop | GoteGold | GoteSilver | GoteKnight | GoteLance | GotePawn

datatype kind = King | Rook | Bishop | Gold | Silver | Knight | Lance | Pawn

type piecerec = { X: int, Y : int, Piece : piece  }

type pieceinhand = { Piece: piece }

type gamestate = {
     Pieces : list piecerec,
     Player : player,
     Inhand : list pieceinhand
}

fun char_to_piece (c : char) : option piece =
    case c of
	#"k" => Some GoteKing
      | #"r" => Some GoteRook
      | #"b" => Some GoteRook
      | #"g" => Some GoteGold
      | #"s" => Some GoteSilver
      | #"n" => Some GoteKnight
      | #"l" => Some GoteLance
      |	#"K" => Some SenteKing
      | #"R" => Some SenteRook
      | #"B" => Some SenteRook
      | #"G" => Some SenteGold
      | #"S" => Some SenteSilver
      | #"N" => Some SenteKnight
      | #"L" => Some SenteLance
      | _ => None

fun fen_to_state (fen : string) : gamestate =
    let
	fun parts_pieces_aux pieces player inhand =
	    { Pieces = pieces, Player = player, Inhand = inhand}
	and fromplayer s pieces =
	    (* let
		val l = strlen s
	    in
		if l = 0 then
		    parts_pieces_aux pieces White False False False False (-1) (-1) 0 1 (* trigger error ? *)
		else
		    let
			val fst = (strsub s 0)
		    in
			case (fenToPlayer fst) of
			    None => parts_pieces_aux pieces White False False False False (-1) (-1) 0 1 (* trigger error? *)
			  | Some player => fromcastling (substring s 2 (l -2)) pieces player False False False False
		    end
	     end *)
	    parts_pieces_aux pieces Sente []
	    
	and fen_to_pieces_aux s row col pieces =
	    let
		val l = strlen s
	    in
		if l = 0 then
		    parts_pieces_aux pieces Sente [] (* trigger error ? *)
		else
		    let
			val fst = (strsub s 0)
		    in
			case fst of
			    #"/" => fen_to_pieces_aux (substring s 1 (l -1)) (row + 1) 0 pieces
			  | #" " => fromplayer (substring s 1 (l -1)) pieces
			  | _ => 			
			    if isdigit fst then
				case (read (show fst) : option int) of
				    Some i => fen_to_pieces_aux (substring s 1 (l -1)) row (col + i) pieces
				  | None => parts_pieces_aux pieces Sente [] (* trigger error ? *)
			    else
				case (char_to_piece fst) of
				    None => parts_pieces_aux pieces Sente [] (* trigger error? *)
				  | Some p => fen_to_pieces_aux (substring s 1 (l - 1)) row (col + 1)
								({X = col, Y = row, Piece= p} :: pieces)
		    end
	    end
	    
    in
	fen_to_pieces_aux fen 0 0 []
    end
