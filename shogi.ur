
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
