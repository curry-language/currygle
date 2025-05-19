# Installation of the web interface for CASS:

# tar file to be generated with the complete web application
TARFILE := $(CURDIR)/CURRYGLE.tgz

# Definition of the root of the Curry system used to compile the webapp:
#CURRYHOME=/opt/pakcs/pakcs
CURRYHOME=/opt/kics2/kics2

# Curry bin directory to be used:
export CURRYBIN=$(CURRYHOME)/bin

CURRYOPTIONS=:set -time

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored, e.g.: $(HOME)/public_html
WEBDIR = $(HOME)/public_html/currygle2

# Executable of the Curry Package Manager to instal the webapp:
CPM := $(CURRYBIN)/cypm

# Executable of the curry2cgi:
CURRY2CGI := $(shell which curry2cgi)

# The indexer executable:
CURRYGLE = currygle

# The directory containing '.cdoc' files for indexing:
DOCDIR = CDOC

# Log file of the search server:
LOGFILE = SERVER.LOG

PIDFILE = server.pid
PID := $(shell if [ -f $(PIDFILE) ] ; then cat $(PIDFILE) ; else echo "" ; fi)

# The main module of the webapp:
WEBAPPMAIN = WebInterface

############################################################################

.PHONY: all
all:
	@echo "make: index start stop restart install indexer deploy tar clean?"

# Create index:
.PHONY: index
index:
	bin/$(CURRYGLE) --index --docdir $(DOCDIR)

# start the search server
.PHONY: start
start:
ifeq ($(PID),)
	@echo Starting search server at `date`... >> $(LOGFILE)
	nohup bin/$(CURRYGLE) --server >> $(LOGFILE) 2>&1 &
	@sleep 1
	@/bin/rm -f TMPOUT
	@ps x | grep $(CURRYGLE) > TMPOUT
	@grep server TMPOUT | awk '{ print $$1 }' > $(PIDFILE)
	@/bin/rm -f TMPOUT
	@echo "'$(CURRYGLE)' started..."
else
	@echo "$(CURRYGLE) is already running with PID $(PID)."
endif

# stop the search server
.PHONY: stop
stop:
ifeq ($(PID),)
	@echo "$(CURRYGLE) is not running."
else
	/bin/rm -f $(PIDFILE)
	bin/$(CURRYGLE) --stop
	@echo Currygle server stopped
endif

# stop the search server
.PHONY: restart
restart:
	$(MAKE) --ignore-errors stop
	sleep 2 # to avoid busy port
	$(MAKE) start

# show status of search server
.PHONY: status
status:
ifeq ($(PID),)
	@echo "$(CURRYGLE) is not running."
else
	@echo "$(CURRYGLE) is running with PID $(PID):"
	ps -p $(PID) -o size
endif

############# Installation #########################################
# Install the packages required by the application:
.PHONY: install
install:
	$(CPM) install

$(WEBDIR):
	mkdir -p $(WEBDIR)

# Install indexer and search tool:
.PHONY: indexer
indexer: $(WEBDIR)/bin/$(CURRYGLE)

$(WEBDIR)/bin/$(CURRYGLE): src/*.curry src/*/*.curry | $(WEBDIR)
	mkdir -p $(WEBDIR)/bin
	$(CPM) --define BIN_INSTALL_PATH=$(WEBDIR)/bin install

# check presence of tools required for deployment and install them:
.PHONY: checkdeploy
checkdeploy:
	@if [ ! -x "$(CURRY2CGI)" ] ; then \
	   echo "Installing required executable 'curry2cgi'..." ; \
           $(CPM) install html2 ; fi

# create/install web interface script
.PHONY: deploy
deploy: checkdeploy $(WEBDIR)/bin/$(CURRYGLE) | $(WEBDIR)
	$(MAKE) $(WEBDIR)/main.cgi
	# copy other files (style sheets, images,...)
	cp -r public/* $(WEBDIR)
	chmod -R go+rX $(WEBDIR)
	cp Makefile $(WEBDIR)

$(WEBDIR)/main.cgi: src/*.curry
	$(CPM) exec $(CURRY2CGI) --cpm=$(CPM) --system="$(CURRYHOME)" \
	  --ulimit=\"-t 300\" -i $(WEBAPPMAIN) -o $@ $(WEBAPPMAIN)

# create tar file with complete web app
.PHONY: tar
tar:
	/bin/rm -f $(TARFILE)
	$(MAKE) $(TARFILE)

$(TARFILE): $(WEBDIR)/main.cgi
	cd $(WEBDIR) && tar czvf $(TARFILE) .
	chmod 644 $(TARFILE)
	@echo "tar file with web app generated:"
	@echo "$(TARFILE)"
	@echo "Copy and unpack it in the desired directory of the web server"

.PHONY: clean
clean:
	$(CPM) clean
	rm -f $(WEBDIR)/*.cgi*
	rm -f $(WEBDIR)/bin/$(CURRYGLE)
	rm -f $(WEBDIR)/LOGFILE
