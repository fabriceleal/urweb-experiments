open Types
open Chess

functor Make(M : sig type t end) = struct
    sequence s
    table t : {Id : int, Client : client, Channel : channel M.t}
      PRIMARY KEY (Id, Client)

    val create : transaction topic = nextval s

    fun subscribe (id : topic) : transaction (channel M.t) =
        cli <- self;
        ro <- oneOrNoRows (SELECT t.Channel FROM t WHERE t.Id = {[id]} AND t.Client = {[cli]});
        case ro of
            None =>
            ch <- channel;
            dml (INSERT INTO t (Id, Client, Channel) VALUES ({[id]}, {[cli]}, {[ch]}));
            return ch
          | Some r => return r.T.Channel

    fun send (id : topic) (msg : M.t) : transaction unit =
        queryI (SELECT t.Channel FROM t WHERE t.Id = {[id]})
        (fn r => Basis.send r.T.Channel msg)

    fun subscribers (id: topic) : transaction int =
        r <- oneRow (SELECT COUNT( * ) AS N FROM t WHERE t.Id = {[id]});
        return r.N
end

structure ChessRoom = Make(struct
			       type t = chessboardmsg
			   end)
(*
structure WeiqiRoom = Make(struct
			       type t = weiqiboardmsg
			  end)
*)
