open String

val testPgn = "[Event \"Reykjavik Open\"]
[Site \"Reykjavik, Iceland\"]
[Date \"????.??.??\"]
[Round \"8.1\"]
[White \"Adhiban, Baskaran\"]
[Black \"Rapport, Richard\"]
[Result \"1-0\"]
[ECO \"D15\"]
[PlyCount \"54\"]

1. d4 {[%emt 0:00:27]} d5 {[%emt 0:01:11]} 2. c4 {[%emt 0:00:42]} c6 {[%emt 0:
00:06]} 3. Nf3 {[%emt 0:02:33]} Nf6 {[%emt 0:00:30]} 4. Nc3 {[%emt 0:00:05]}
dxc4 {[%emt 0:00:05]} 5. e4 {[%emt 0:02:39]} b5 {[%emt 0:00:26]} 6. Be2 {
[%emt 0:00:01]} e6 {[%emt 0:01:31]} 7. O-O {[%emt 0:00:46]} Be7 {[%emt 0:03:04]
} 8. a4 {[%emt 0:03:34]} b4 {[%emt 0:00:44]} 9. e5 {[%emt 0:00:08]} bxc3 {
[%emt 0:02:06]} 10. exf6 {[%emt 0:00:05]} Bxf6 {[%emt 0:00:14]} 11. bxc3 {
[%emt 0:00:07]} Ba6 {[%emt 0:00:37]} 12. Ne5 {[%emt 0:17:30]} Bxe5 {[%emt 0:04:
41]} 13. dxe5 {[%emt 0:00:02]} Qxd1 {[%emt 0:00:25]} 14. Rxd1 {[%emt 0:00:02]}
Nd7 {[%emt 0:00:07]} 15. f4 {[%emt 0:04:04]} Nb6 {[%emt 0:04:54]} 16. Rd6 {
[%emt 0:02:24]} O-O {[%emt 0:13:59]} 17. Bf3 {[%emt 0:08:12]} Nc8 {[%emt 0:07:
22]} 18. Rxc6 {[%emt 0:02:30]} Bb7 {[%emt 0:00:29]} 19. Rxc8 {[%emt 0:00:02]}
Raxc8 {[%emt 0:00:05]} 20. Bxb7 {[%emt 0:00:05]} Rb8 {[%emt 0:00:12]} 21. Ba6 {
[%emt 0:02:05]} Rb3 {[%emt 0:00:15]} 22. Ba3 {[%emt 0:00:48]} Rd8 {[%emt 0:00:
11]} 23. Bb4 {[%emt 0:00:47]} Rd2 {[%emt 0:00:39]} 24. Bxc4 {[%emt 0:00:11]}
Rbb2 {[%emt 0:00:02]} 25. Bf1 {[%emt 0:00:30]} h5 {[%emt 0:00:10]} 26. a5 {
[%emt 0:02:24]} a6 {[%emt 0:00:16]} 27. Bc5 {[%emt 0:03:33]} g6 {[%emt 0:00:52]
} 1-0
"

fun parsePgnR s =
    ssplit {Haystack=s, Needle="\n"}


fun parsePgn s =
    (case (ssplit {Haystack=s, Needle="\n"}) of
	None => None
      | Some (l, rest) =>
	(case (Regex.match "Event " l) of
	    None => None
	  | Some m =>
	    (case (List.nth m.Groups 0) of
		None => None
	      | Some v => Some (v, ""))))
    
fun testParse () =
    let
	val r = parsePgn testPgn
	val comp = case r of
		       None => <xml>none</xml>
		     | Some (s1, s2) =>
		       <xml>
			 <div>head: {[s1]}</div>
			 <div>rest: {[s2]}</div>
		       </xml>
    in
	return <xml>
	  <body>
	    {comp}
	  </body>
	</xml>
    end
