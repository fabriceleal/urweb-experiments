
helloworld.exe: *.ur *.urs *.urp
	urweb -dbms sqlite -db helloworld.db helloworld

dumpS:
	urweb -dumpSource -dbms sqlite -db helloworld.db helloworld 2> dumpSource.txt

dumpV:
	urweb -dumpVerboseSource -dbms sqlite -db helloworld.db helloworld 2> dumpVerboseSource.txt

helloworld.db: helloworld.sql
	rm -f helloworld.db
	sqlite3 helloworld.db < helloworld.sql

run: helloworld.exe
	./helloworld.exe

clean:
	rm helloworld.exe
