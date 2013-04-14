NAME=Database
OBJS=Source.o HttpServer.o Errors.o Database.o Storage.o Query.o Parser.o libjson/libjson.a

.SUFFIXES: .cpp
.SUFFIXES: .o

all: $(NAME) encode
	
clean:
	rm -rf *.o

cleanall: clean
	rm -rf *.d map dep *~ $(NAME)

rebuild: cleanall all
	@echo Rebuild done.

$(NAME): $(OBJS)
	@echo Compiling $(NAME).
	g++ -m32 -lmongoose -lcryptopp -lcurl -shared-libgcc -s -o $(NAME) $(OBJS)

encode: Encode.o
	@echo Compiling Encode.
	g++ -m32 -lcryptopp -lpthread -o encode Encode.o Database.o Storage.o libjson/libjson.a

%.o : %.cpp
	g++ -m32 -std=c++0x -DNODEBUG -O2 -c $*.cpp
