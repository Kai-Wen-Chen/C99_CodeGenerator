all:
	flex hw3.l
	byacc -d -v hw3.y
	gcc -o codegen lex.yy.c y.tab.c -lfl SymbolTable.c SymbolTable.h
clean:
	rm -f lex.yy.c parser y.tab.h y.tab.c y.output
