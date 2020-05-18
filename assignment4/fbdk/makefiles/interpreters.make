# This file contains the instructions to compile the existing interpreters in
# the FbDK.

# Global aliases
srcdir = src
builddir = _build

BUILD_FLAGS = 

# Determine a list of interpreters.  Note that, below, we strip the trailing
# slash from each directory name.  This is necessary because, on Mac OS X,
# cp treats "foo/bar/" as "foo/bar/*" rather than as "foo/bar".
interpreter_dirs = \
	$(foreach interpreter_dir,\
						$(sort $(dir $(wildcard $(srcdir)/*/*))),\
						$(patsubst %/,%,$(interpreter_dir))\
	 )
interpreters = \
	$(subst /,,\
		$(subst $(srcdir)/,,$(interpreter_dirs))\
	 )

# The version file used by the toploop for display purposes.
$(srcdir)/version.ml: version.txt
	echo "let version = \""$(shell cat version.txt)"\"" > $(srcdir)/version.ml
	echo "let date = \""$(shell date)"\"" >> $(srcdir)/version.ml

# A build for a single interpreter
%.byte: phony $(srcdir)/version.ml
	ocamlbuild -use-menhir -I $(srcdir) -Is "$(interpreter_dirs)" -r $(BUILD_FLAGS) $@

# Build all interpreters
.PHONY: all
all: $(addsuffix .byte,$(call lowercase,$(interpreters)))

# Clean all interpreters
.PHONY: clean
clean:
	ocamlbuild -clean
