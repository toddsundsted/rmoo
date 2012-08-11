# Makefile.am: File used by automake to generate Makefile.in
#
# Original Author: Matthew Campbell <mattcampbell@pobox.com>
# $Author: mattcampbell $
# $Date: 2000/10/18 21:24:35 $
# $Revision: 1.6 $

lisp_LISP = emacspeak-rmoo.el prefix.el moocode-mode.el \
  rmoo-display-jtext.el rmoo-extras.el rmoo-list.el rmoo-local-edit.el \
  rmoo-mail.el rmoo-mcp.el rmoo-menus.el rmoo-objects.el rmoo-rmail.el \
  rmoo-text.el rmoo.el rmoo-autoload.el
BUILT_SOURCES = README
SOURCES = $(lisp_LISP) $(BUILT_SOURCES) README.html
AUTOMAKE_OPTIONS = foreign
sound_DATA = cow.wav cow5.au
sounddir = /usr/share/emacs/site-lisp/emacspeak/sounds
EXTRA_DIST = LICENSE $(sound_DATA)

.el.elc:
	if test $(EMACS) != no; then \
	  srcdir=$(srcdir) EMACS=$(EMACS) $(SHELL) $(srcdir)/elisp-comp $<; \
	fi

README: README.html
	lynx -dump README.html >README

# $Log: Makefile.am,v $
# Revision 1.6  2000/10/18 21:24:35  mattcampbell
# Updated my email address and got rid of the Latte stuff.
#
# Revision 1.5  2000/01/17 17:13:40  mattcamp
# Updated EXTRA_DIST declaration to use sound_DATA variable instead of listing the sound files themselves.
#
# Revision 1.4  2000/01/17 17:12:45  mattcamp
# Fixed bug that was preventing sound files from being included in the distribution.
#
# Revision 1.3  2000/01/17 16:37:09  mattcamp
# Added code to automatically build the documentation (HTML and text versions), install the mooing sounds, and include the LICENSE file in the distribution.
#
# Revision 1.2  1999/11/24 19:18:41  mattcamp
# Added new elisp compilation rule that sets the srcdir environment variable before calling my hacked elisp-comp script.
#
# Revision 1.1  1999/06/13 17:50:40  mattcamp
# Initial revision
#
