NAME=Database
OBJS=Source.o HttpServer.o Errors.o Database.o Storage.o Query.o Parser.o libjson/libjson.a

.SUFFIXES: .cpp
.SUFFIXES: .o

all: $(NAME)
	
clean:
	rm -rf *.o

cleanall: clean
	rm -rf *.d map dep *~ $(NAME)

rebuild: cleanall all
	@echo Rebuild done.

$(NAME): $(OBJS)
	@echo Compiling $(NAME).
	g++ -m32 -lmongoose -lcryptopp -o $(NAME) $(OBJS)

%.o : %.cpp
	g++ -m32 -std=c++0x -DNODEBUG -O2 -c $*.cpp
