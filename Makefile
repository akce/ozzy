# ozzy Makefile.
# Written by Akce 2020.
# SPDX-License-Identifier: Unlicense

# Install destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
BINDIR = ~/usr/bin

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

# Path to rm executable.
RM = /bin/rm

## Should be no need to edit anything below here.

# Scheme sources need to be in order of dependencies first, library last.
# That should avoid compile/load errors.
SSRC = bin/ozzy.ss

SOBJ = bin/ozzy.so

all: $(SOBJ)

bin/ozzy.so: bin/ozzy.ss
	echo '(reset-handler abort) (compile-program "'$<'")' | $(SCHEME) $(SFLAGS)

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

# Default install target is for binary only.
install: install-bin

install-bin: all
	$(INSTALL) -p $(SOBJ) $(BINDIR)/ozzy

install-src: all
	$(INSTALL) -D -p -t $(BINDIR) $(SSRC)

clean:
	$(RM) -f $(SOBJ)
