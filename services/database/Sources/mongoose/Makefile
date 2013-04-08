NAME = mongoose
OBJS = mongoose.o
inst_path = /usr/lib
include_path = /usr/include

.SUFFIXES: .c
.SUFFIXES: .o

all: install
	
clean:
	rm -rf *.o $(NAME).so

cleanall: clean
	rm -rf *.d map dep *~ $(NAME).so

rebuild: cleanall all
	@echo Rebuild done.

$(NAME): $(OBJS)
	@echo Compiling $(NAME).
	gcc -m32 -shared -o $(NAME).so $(OBJS) -lpthread -ldl

%.o : %.c
	gcc -m32 -c $*.c

install: install_headers $(NAME)
	@echo "Install shared library"
	cp -f ./$(NAME).so $(inst_path)/lib$(NAME).so
	ldconfig
	@echo "Install shared library: Done."

install_headers:
	@echo "Install header files"
	mkdir -p $(include_path)/$(NAME)
	cp -f ./*.h $(include_path)/$(NAME)
	@echo "Install header files: Done."
