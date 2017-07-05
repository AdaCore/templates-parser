############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2017, AdaCore                     #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

.SILENT:

-include ../makefile.setup

build:
	$(GPRBUILD) -q -p -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) \
		$(foreach v,PRJ_BUILD TARGET VERSION,"-X$(v)=$($(v))") \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) -Pregtests

test:  build
	PRJ_BUILD="$(PRJ_BUILD)" PATH=.:${PATH} ./testsuite.py --jobs=4

clean:
	-$(GPRCLEAN) -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) \
		$(foreach v,PRJ_BUILD TARGET VERSION,"-X$(v)=$($(v))") \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) -Pregtests
	rm -fr out
	rm -f *.pyc
