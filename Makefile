ML := poly --script

all: hashira

clean:
	$(RM) hashira hashira.o

hashira: hashira.ML
	$(ML) compile.ML
	$(CC) -o $@ hashira.o -lpolymain -lpolyml
