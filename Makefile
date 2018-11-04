
helloworldLone.exe: *.ur *.urs *.urp
	urweb -dbms postgres -output helloworldLone.exe helloworld

fcgi: *.ur *.urs *.urp
	urweb -protocol fastcgi -dbms postgres helloworld

static: *.ur *.urs *.urp
	urweb -static -protocol fastcgi -dbms postgres helloworld

dumpS:
	urweb -dumpSource -dbms sqlite -db helloworld.db helloworld 2> dumpSource.txt

dumpV:
	urweb -dumpVerboseSource -dbms sqlite -db helloworld.db helloworld 2> dumpVerboseSource.txt

helloworld.db: helloworld.sql
	dropdb --if-exists helloworld
	createdb helloworld
	psql -f helloworld.sql helloworld
	psql -f	helloworldPost.sql helloworld

run: helloworldLone.exe
	./helloworldLone.exe

clean:
	rm -f helloworld.exe
	rm -f helloworldLone.exe
