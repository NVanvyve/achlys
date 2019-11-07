# Auto-generated by https://github.com/Pouriya-Jahanbakhsh/estuff

CFG_DIR              = $(CURDIR)/config
GRISP_FILES_DIR      = $(CURDIR)/grisp/grisp_base/files
TOOLS_DIR            = $(CURDIR)/tools
# HOST_REBAR           = rebar3
# REBAR                = $(TOOLS_DIR)/rebar3
REBAR                = rebar3
# LIB_CACHE_DIR          = $(LOCAL_REBAR_DIR)/lib/*/ebin
# PLUGIN_CACHE_DIR          = $(LOCAL_REBAR_DIR)/plguins/*/ebin
LIB_CACHE_DIR          = $(LOCAL_REBAR_DIR)/lib
PLUGIN_CACHE_DIR          = $(LOCAL_REBAR_DIR)/plugins
ERL                  := $(shell command -v erl 2> /dev/null)
RELEASE_DIR          = $(CURDIR)/_build/default/rel/achlys
VERSION              := $(shell cat VERSION | tr -ds \n \r)
RELEASE_NAME         = achlys-$(VERSION)
NAME_UPPER           := $(shell echo achlys | awk '{print toupper($$1)}')
GRISPAPP             ?= $(shell basename `find src -name "*.app.src"` .app.src)
REBAR_CONFIG         = $(CURDIR)/rebar.config
COOKIE               ?= MyCookie
NAME 				 := $(shell hostname -s)
PEER_IP 	 		 := $(ifconfig | grep 'inet ' | grep -m 1 -Fv 127.0.0.1 | awk '{print $2}' | sed 's/\./,/g')
IP 	 				 := $(ifconfig | grep 'inet ' | grep -m 1 -Fv 127.0.0.1 | awk '{print $2}')

PRE         = @
POST        =
REBAR_DEBUG =

v = 1
ifeq ($(v),0)
POST = > /dev/null
endif

ifeq ($(v),2)
PRE         =
REBAR_DEBUG = 1
endif

coverage = 0
ifeq ($(coverage),0)
coverage = /dev/null
endif

ifndef ERL
$(error Could not find Erlang/OTP ('erl' command) installed on this system.)
endif

.PHONY: all compile checkrebar3 shell erlshell docs test dialyzer cover release package tar clean relclean push upbar addemu deploy cacheclean build upgrade tree



all: test docs package

checkrebar3:
	@ echo Checking host Rebar3 version
	$(PRE) \
			$(HOST_REBAR) version \
		$(POST)
	@ echo Adding locally installed Rebar3 to tools directory
	$(PRE) (cp -r $(HOST_REBAR) $(REBAR)) && ($(REBAR) version) $(POST)


compile:
	@ echo Compiling code
	$(PRE)                                         \
			export $(NAME_UPPER)_BUILD=COMPILE      && \
			export DEBUG=$(REBAR_DEBUG)             && \
			export $(NAME_UPPER)_VERSION=$(VERSION) && \
			$(REBAR) compile                           \
		$(POST)
	$(PRE) cp -r $(CURDIR)/_build/default/lib/achlys/ebin $(CURDIR)

shell:
	@ echo Launching shell
	$(PRE) \
	    NAME=$(NAME) PEER_IP=$(PEER_IP) IP=$(IP) $(REBAR) as test shell --sname $(GRISPAPP)$(n) --setcookie $(COOKIE) --apps $(GRISPAPP) $(POST)

deploy:
	@ echo Deploying
	$(PRE) \
        export NAME=$(echo hostname -s) && \
        export PEER_IP=$(ifconfig | grep "inet " | grep -m 1 -Fv 127.0.0.1 | awk '{print $2}' | sed 's/\./,/g') && \
        echo "PEER_IP=$(PEER_IP)" > $(CURDIR)/default.env && \
	    $(REBAR) grisp deploy -n $(GRISPAPP) -v $(VERSION)

erlshell:
	@ echo Compiling user_default module
	$(PRE) erlc -o $(TOOLS_DIR) $(TOOLS_DIR)/user_default.erl $(POST)
	$(PRE) \
			export $(NAME_UPPER)_BUILD=SHELL && \
			export DEBUG=$(REBAR_DEBUG) && \
			export $(NAME_UPPER)_VERSION=$(VERSION) && \
			$(REBAR) compile \
		$(POST) && \
		erl -pa `ls -d _build/default/lib/*/ebin` \
				-pz $(TOOLS_DIR) \
				-config $(CFG_DIR)/sys.config \
				-args_file $(CFG_DIR)/vm.args \
				-eval "begin application:load('achlys'), catch code:load_file('achlys') end" \

addemu:
	@ if [ -f $(REBAR_CONFIG) ]; then \
		if [ $$(grep -c 'extra_src_dirs' rebar.config) -eq 0 ]; then \
			echo $(REBAR_APPEND) >> $(REBAR_CONFIG); \
		fi; \
	else \
		echo "ERROR: no rebar"; \
	fi \

docs:
	@ echo Building documentation
	$(PRE) \
			export $(NAME_UPPER)_BUILD=DOC && \
			export DEBUG=$(REBAR_DEBUG) && \
			export $(NAME_UPPER)_VERSION=$(VERSION) && \
			$(REBAR) edoc \
		$(POST)


test: cover


dialyzer: compile
	@ echo Running dialyzer
	$(PRE) \
			export $(NAME_UPPER)_BUILD=DIALYZER && \
			export DEBUG=$(REBAR_DEBUG) && \
			export $(NAME_UPPER)_VERSION=$(VERSION) && \
			$(REBAR) dialyzer \
		$(POST)


cover: compile
	@ echo Running tests
	$(PRE) \
			export $(NAME_UPPER)_BUILD=TEST && \
			export DEBUG=$(REBAR_DEBUG) && \
			export $(NAME_UPPER)_VERSION=$(VERSION) && \
			$(REBAR) do ct, cover \
		$(POST)
	@ echo Coverage summary:
	$(PRE) \
			awk -f $(TOOLS_DIR)/coverage_summary.awk \
				-v indent="\t" \
				-v colorize=1 \
				$(CURDIR)/_build/test/cover/index.html \
			|| true
	$(PRE) \
			awk -f $(TOOLS_DIR)/coverage_summary.awk \
				   $(CURDIR)/_build/test/cover/index.html \
			> $(coverage) || true


release: compile
	@ echo Building release $(RELEASE_NAME)
	$(PRE) \
			export $(NAME_UPPER)_BUILD=RELEASE && \
			export DEBUG=$(REBAR_DEBUG) && \
			export $(NAME_UPPER)_VERSION=$(VERSION) && \
			$(REBAR) release \
		$(POST)
	$(PRE) mkdir -p $(CURDIR)/$(RELEASE_NAME) $(POST)
	$(PRE) cp -r $(RELEASE_DIR)/* $(CURDIR)/$(RELEASE_NAME) $(POST)


package: release
	$(PRE) tar -zcvf $(RELEASE_NAME).tar.gz $(CURDIR)/$(RELEASE_NAME) $(POST)

# upbar:
# 	@ echo Overwriting rebar3 with locally cached version
# 	cp $(LOCAL_REBAR) $(TOOLS_DIR)

tar:
	$(PRE) (rm -rf ./achlys.tar.gz) && (find ./ -type f > ../.achlys_archive) && (tar -zcvf achlys.tar.gz -T - < ../.achlys_archive) && rm -rf ../.achlys_archive $(POST)

upgrade:
	@ echo Upgrading dependencies
	$(PRE) \
			$(REBAR) update && \
			$(REBAR) unlock && \
			$(REBAR) upgrade \
	$(POST)

build: upgrade
	@ echo Rebuilding VM 
	$(PRE) \
			$(REBAR) grisp build --clean true --configure true $(POST) \
	$(POST)

tree: upgrade
	@ echo Dependency tree :
	$(PRE) \
			$(REBAR) tree \
	$(POST)

cacheclean:
	@ echo Cache purge
	$(PRE) rm -rdf $(LIB_CACHE_DIR)/*/ebin $(PLUGIN_CACHE_DIR)/*/ebin $(POST)

clean: relclean
	@ echo Cleaning out	
	$(PRE) $(REBAR) clean $(POST)
	$(PRE) rm -rf $(CURDIR)/ebin && rm -rdf $(CURDIR)/_build/ && rm -rdf $(CURDIR)/_checkouts/*/ebin $(POST)

relclean:
	@ echo Release clean
	$(PRE) rm -rdf $(CURDIR)/$(RELEASE_NAME)/ && rm -rdf $(CURDIR)/$(RELEASE_NAME).tar.gz $(POST)