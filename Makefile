

helloworld.exe: *.ur *.urs *.urp
	urweb -dbms sqlite -db helloworld.db helloworld

helloworld.db: 
	rm -f helloworld.db
	sqlite3 helloworld.db < helloworld.sql

run: helloworld.exe helloworld.db
	./helloworld.exe
