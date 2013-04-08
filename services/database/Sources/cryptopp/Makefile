NAME = cryptopp
OBJS = 3way.o adler32.o algebra.o algparam.o arc4.o asn.o \
authenc.o base32.o base64.o basecode.o bench.o bench2.o bfinit.o blowfish.o \
blumshub.o camellia.o cast.o casts.o cbcmac.o ccm.o channels.o cmac.o cpu.o \
crc.o cryptlib.o cryptlib_bds.o datatest.o default.o des.o dessp.o dh.o \
dh2.o dll.o dlltest.o dsa.o eax.o ec2n.o eccrypto.o ecp.o elgamal.o emsa2.o \
eprecomp.o esign.o files.o filters.o fips140.o fipsalgt.o fipstest.o gcm.o \
gf256.o gf2_32.o gf2n.o gfpcrypt.o gost.o gzip.o hex.o hmac.o hrtimer.o \
ida.o idea.o integer.o iterhash.o luc.o mars.o marss.o md2.o md4.o md5.o \
misc.o modes.o mqueue.o mqv.o nbtheory.o network.o oaep.o osrng.o panama.o \
pch.o pkcspad.o polynomi.o pssr.o pubkey.o queue.o rabin.o randpool.o rc2.o \
rc5.o rc6.o rdtables.o regtest.o rijndael.o ripemd.o rng.o rsa.o rw.o \
safer.o salsa.o seal.o seed.o serpent.o sha.o sha3.o shacal2.o shark.o \
sharkbox.o simple.o skipjack.o socketft.o sosemanuk.o square.o squaretb.o \
stdcpp.h strciphr.o tea.o test.o tftables.o tiger.o tigertab.o trdlocal.o \
ttmac.o twofish.o validat1.o validat2.o validat3.o vmac.o wait.o wake.o \
whrlpool.o winpipes.o xtr.o xtrcrypt.o zdeflate.o zinflate.o zlib.o

inst_path = /usr/lib
include_path = /usr/include

.SUFFIXES: .cpp
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
	g++ -m32 -shared -o $(NAME).so $(OBJS)

%.o : %.cpp
	g++ -m32 -c $*.cpp

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
