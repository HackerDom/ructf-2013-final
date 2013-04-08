# JSON source files to build
objects        = internalJSONNode.o JSONAllocator.o JSONChildren.o \
                 JSONDebug.o JSONIterators.o JSONMemory.o JSONNode.o \
                 JSONNode_Mutex.o JSONPreparse.o JSONStream.o JSONValidator.o \
                 JSONWorker.o JSONWriter.o libjson.o 
# Defaults
cxxflags_default = -m32 -O3 -ffast-math -fexpensive-optimizations -DNDEBUG
cxxflags_shared  = -f$(PIC)

libname          = libjson
libname_hdr      := $(libname)
suffix_shared    = so
suffix_static    = a
major_version    = 7
minor_version    = 6.1
objdir           = Objects

# Variables
prefix          ?= /usr
exec_prefix     ?= $(prefix)
libdir          ?= lib
includedir      ?= include
srcdir          ?= _internal/Source
CXX             ?= c++
AR              ?= ar
PIC             ?= PIC
BUILD_TYPE      ?= "default"
SHARED          ?= "1"

# Internal Variables
inst_path        = $(exec_prefix)/$(libdir)
include_path     = $(prefix)/$(includedir)

# Usage check
ifdef CXXFLAGS
ifdef BUILD_TYPE
	$(error CXXFLAGS and BUILD_TYPE are mutually exclusive)
endif
endif

CXXFLAGS    ?= $(cxxflags_default)

# SHARED specific settings
ifeq ($(SHARED), 1)
	libname_shared               = $(libname).$(suffix_shared)
	libname_shared_major_version = $(libname_shared).$(major_version)
	lib_target                   = $(libname_shared_major_version).$(minor_version)
	objdir                      := $(objdir)_shared
	CXXFLAGS                    := $(CXXFLAGS) $(cxxflags_shared)
else
	lib_target                   = $(libname).$(suffix_static)
	objdir                      := $(objdir)_static
endif

# Phony targets
.PHONY: all banner installdirs install install_headers clean uninstall \
        uninstall_headers

# Targets
all: $(lib_target)
	@echo "============================================================"
	@echo "Done"
	@echo "============================================================"

banner:
	@echo "============================================================"
	@echo "libjson version: "$(major_version).$(minor_version) "target: "$(target)
	@echo "============================================================"

installdirs: banner
	mkdir -p $(objdir)

# Libraries
ifeq ($(SHARED),1)
$(lib_target): banner installdirs $(addprefix $(objdir)/, $(objects))
	@echo "Link "
	cd $(objdir) ; \
	$(CXX) -m32 -shared -Wl,-soname,$(libname_shared_major_version) -o $@ $(objects) ; \
	mv -f $@ ../
	@echo "Link: Done"
else
$(lib_target): banner installdirs $(addprefix $(objdir)/, $(objects))
	@echo "Archive"
	cd $(objdir) ; \
	$(AR) -cvq $@ $(objects) ; \
	mv -f $@ ../
	@echo "Archive: Done"
endif

# Compile object files
$(objdir)/%.o: $(srcdir)/%.cpp
	$(CXX) $< -o $@ -c $(CXXFLAGS)

ifeq ($(SHARED),1)
install: banner install_headers $(lib_target)
	@echo "Install shared library"
	cp -f ./$(lib_target) $(inst_path)
	cd $(inst_path) ; \
	ln -sf $(lib_target) $(libname_shared_major_version) ; \
	ln -sf $(libname_shared_major_version) $(libname_shared)
	ldconfig
	@echo "Install shared library: Done."
else
install: banner install_headers $(lib_target)
	@echo "Install static library"
	cp -f ./$(lib_target) $(inst_path)
	@echo "Install static library: Done."
endif

install_headers: banner
	@echo "Install header files"
	mkdir -p $(include_path)/$(libname_hdr)
	cp -f ./*.h $(include_path)/$(libname_hdr)
	mkdir -p $(include_path)/$(libname_hdr)/$(srcdir)
	cp -f ./$(srcdir)/*.h $(include_path)/$(libname_hdr)/$(srcdir)
	cp -r ./$(srcdir)/JSONDefs $(include_path)/$(libname_hdr)/$(srcdir)
	chmod -R a+r $(include_path)/$(libname_hdr)
	find  $(include_path)/$(libname_hdr) -type d -exec chmod a+x {} \;
	cp -rv $(srcdir)/../Dependencies/ $(include_path)/$(libname_hdr)/$(srcdir)/..
	@echo "Install header files: Done."

clean: banner
	@echo "Clean library and object folder"
	rm -rf $(objdir)
	rm -f $(lib_target)
	@echo "Clean library and object folder: Done"

ifeq ($(SHARED),1)
uninstall: banner uninstall_headers
	@echo "Uninstall shared library"
	rm -f $(inst_path)/$(libname_shared)
	rm -f $(inst_path)/$(libname_shared_major_version)
	rm -f $(inst_path)/$(lib_target)
ifneq ($(OS),Darwin)
	ldconfig
endif
	@echo "Uninstall shared library: Done"
else
uninstall: banner uninstall_headers
	@echo "Uninstall static library"
	rm -f $(inst_path)/$(lib_target)
	@echo "Uninstall static library: Done"
endif

uninstall_headers: banner
	@echo "Uninstall header files"
	rm -rf $(include_path)/$(libname_hdr)
	@echo "Uninstall header files: Done"
