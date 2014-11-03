ML := poly --script

all: hashira

clean:
	$(RM) hashira hashira.o

hashira: hashira.sml
	$(ML) compile.sml
	$(CC) -o $@ hashira.o -lpolymain -lpolyml
