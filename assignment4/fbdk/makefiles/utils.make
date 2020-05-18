# This utility file contains some functions for use in the main Makefile.  They
# are stored here because they are general utility functions and not directly
# connected to the build process.

# This function lowercases the English characters in a string.
# Example:
#   $(call lowercase,"AbC")
#     ==>
#   "abc"
lowercase = $(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst G,g,$(subst H,h,$(subst I,i,$(subst J,j,$(subst K,k,$(subst L,l,$(subst M,m,$(subst N,n,$(subst O,o,$(subst P,p,$(subst Q,q,$(subst R,r,$(subst S,s,$(subst T,t,$(subst U,u,$(subst V,v,$(subst W,w,$(subst X,x,$(subst Y,y,$(subst Z,z,$1))))))))))))))))))))))))))

# A trick to make phony wildcards possible
.PHONY: phony
phony:

print-%: phony
	@echo "$(info $* is a $(flavor $*) variable set to: [$($*)])"
