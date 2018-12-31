
(* clock page *)
style white_player
style black_player
style half
style full
style content
style commands
style flip
style cmd_button

datatype clockSide =
	 CWhite
       | CBlack


datatype clockStatus =
	 Pend
       | Run of clockSide
       | Lost of clockSide


fun otherC c =
    case c of
	CWhite => CBlack
      | CBlack => CWhite

fun eqC a b =
    case (a, b) of
	(CWhite, CWhite) => True
      | (CBlack, CBlack) => True
      | (_, _) => False
		  
type timecontrol =
     {BaseTime : int, Increment: int, ResetIfUnexpired: bool}

type lsTimecontrol = list timecontrol


fun handleClockF r : transaction page =
    let
	fun validate r : option (int * int)=	    
	    baseTime <- read r.BaseTime;
	    increment <- read r.Increment;
	    Some (baseTime, increment)
    in
	case (validate r) of
	    None => error <xml>Invalid data</xml>
	  | Some (bt, i) => redirect (url (clock ({BaseTime =bt, Increment = i, ResetIfUnexpired = False} :: []) ))
    end

and handleClockB r : transaction page =
    let
	fun validate r : option (int * int) =
	    baseTime <- read r.BaseTime;
	    periods <- read r.Periods;
	    Some (baseTime, periods)

	fun genByoyomi t s =
	    case t of
		0 => []
	      | _ => s :: (genByoyomi (t - 1) s)
    in
	case (validate r) of
	    None => error <xml>Invalid data</xml>
	  | Some (bt, p) => redirect (url (clock (genByoyomi p {BaseTime =bt, Increment = 0, ResetIfUnexpired = True}) ))
    end
    
and clockSetup () : transaction page =
    return <xml>
      <body>
	<div>
	  fischer
	  <form>
	    <textbox{#BaseTime} />
	    <textbox{#Increment} />
	    <submit action={handleClockF} value="Clock F" />
	  </form>
	</div>

	<div>
	  byo yomi
	  <form>
	    <textbox{#BaseTime} />
	    <textbox{#Periods} />
	    <submit action={handleClockB} value="Clock B" />
	  </form>
	</div>
      </body>
    </xml>
    
and clock ltc : transaction page =
    case ltc of
	[] => error <xml>No control setup</xml>
      | tc :: tcTail => 
	let
	    fun minToTiny m =
		m * 60

	    fun tinyToStr sec =
		let
		    val min = sec / 60
		    val secP = sec - (min * 60)
		in
		    (show min) ^ ":" ^ (if secP < 10 then "0" else "") ^ (show secP)
		end

	    fun renderP t =		
		let
		    val l = 1 + (List.length t)
		    val str =  if (l > 1) then
				   (show l)
			       else
				   ""
		in
		    return <xml>
		      {[ str ]}
		    </xml>
		end
		
	    fun renderT t =
		return <xml>
		  {[tinyToStr t]}
		</xml>
	in
	    wtime <- source (minToTiny tc.BaseTime);
	    wrest <- source tcTail;
	    btime <- source (minToTiny tc.BaseTime);
	    brest <- source tcTail;
	    turn <- source Pend;

	    let

		fun reset () =
		    set wtime (minToTiny tc.BaseTime);
		    set wrest tcTail;
		    set btime (minToTiny tc.BaseTime);
		    set brest tcTail;
		    set turn Pend
		    
		fun plToSig c =
		    case c of
			CWhite => return (wtime, wrest)
		      | CBlack => return (btime, brest)

		fun tap (src : clockSide) =
		    let
			fun tapTail tc =
			    current <- get turn;
			    case current of
				Pend =>
				set turn (Run (otherC src))
			      | Run c' =>
				if (eqC c' src) then
				    (pl, ps) <- plToSig c';
				    t <- get pl;
				    (if (t = 0) then (* consume time slot or lose on time *)
					 ls <- get ps;
					 case ls of
					     [] => 
					     set turn (Lost c')
					   | h :: tl =>
					     set ps tl;
					     set pl (minToTiny h.BaseTime)
				     else 
					 if tc.ResetIfUnexpired then (* reset time slot, if appliable *)
					     set pl (minToTiny (tc.BaseTime))
					 else
					     set pl (t + tc.Increment)); 
				    set turn (Run (otherC src))
				else
				    return ()
			      | Lost _ =>
				return ()
		    in
			tapTail tc
		    end 

		fun tickInner () =
		    current <- get turn;
		    case current of
			Pend => setTimeout tickInner 1000
		      | Run c' =>
			(sg, ps) <- plToSig c';
			t <- get sg;
			set sg (t - 1);
			t <- get sg;
			if (t = 0) then (* consume time slot or lose on time *)
			    ls <- get ps;
			    case ls of
				[] =>
				set turn (Lost c');
				setTimeout tickInner 1000
			      | h :: t  =>
				set ps t;
				set sg (minToTiny h.BaseTime);
				setTimeout tickInner 1000
			else
			    setTimeout tickInner 1000			    
			    
		      | Lost _ => setTimeout tickInner 1000

		fun tick () =
		    tickInner ();
		    return <xml></xml>
		    
	    in
		return <xml>
		  <head>
		    <link rel="stylesheet" type="text/css" href="/clock.css" />
		  </head>
		  <body class="full">
		    <div class="half black-player flip">
		      <div class="content" onclick={fn _ => tap CBlack}>
			<dyn signal={b <- signal btime; renderT b}></dyn>
			<dyn signal={br <- signal brest; renderP br}></dyn>
		      </div>
		    </div>
		    <div class="commands">
		      <div class="content">
			<div class="cmd-button">
			  P
			</div>
			<div class="cmd-button" onclick={fn _ => reset ()}>
			  R
			</div>
			<div class="cmd-button" onclick={fn _ => redirect (url (clockSetup ()))}>
			  S
			</div>
		      </div>
		    </div>
		    <div class="half white-player">
		      <div class="content" onclick={fn _ => tap CWhite}>
			<dyn signal={w <- signal wtime; renderT w}></dyn>
			<dyn signal={wr <- signal wrest; renderP wr}></dyn>
		      </div>
		    </div>
		    <active code={tick ()}></active>
		  </body>
		</xml>
	    end
	end
