open Types

sequence postSeq
sequence positionSeq
sequence commentSeq
sequence inviteSeq

type userId = int

table position : {Id: int, PostId: int, Fen : string, Move: option string, MoveAlg: option string, PreviousPositionId: option int }
		     PRIMARY KEY Id,
		     CONSTRAINT CPreviousPositionId FOREIGN KEY PreviousPositionId REFERENCES position (Id) ON DELETE CASCADE

table post : { Id : int, Nam : string, RootPositionId: int, UserId: userId, CurrentPositionId : int, ParentPostId : option int, Room : topic }
		 PRIMARY KEY Id (*,
		 CONSTRAINT CRootPositionId FOREIGN KEY RootPositionId REFERENCES position (Id) ON DELETE CASCADE,
		 CONSTRAINT CCurrentPositionId FOREIGN KEY CurrentPositionId REFERENCES position (Id) ON DELETE CASCADE
*)
table comment : {Id: int, PositionId: int, Content: string, UserId: userId, Sent: time  }
		    PRIMARY KEY Id

				
sequence userSeq

table user: {Id: userId, Nam: string, Pass: Hash.digest, Salt: string }
		PRIMARY KEY Id
		CONSTRAINT Nam UNIQUE Nam

(* , Status: inviteStatus *)
table invite : {Id: int, UserId: userId, InvitedId: option userId, Code: string, Email: string, Sent: time, Status: int}
		   PRIMARY KEY Id,
		   CONSTRAINT Code UNIQUE Code,
		   CONSTRAINT Email UNIQUE Email

table rootAdmin : { Id : userId }
		      PRIMARY KEY Id,
      CONSTRAINT Id FOREIGN KEY Id REFERENCES user(Id)

