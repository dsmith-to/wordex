DOC = doc
EBIN = ebin
PRIV = priv
SRC = src
PWD = $(shell pwd)

ERL = erl
ERLC = erlc
CFLAGS = -pa $(EBIN) -o $(EBIN)

APP = wordex
ERL_LIB = $(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' \
            -s init stop -noshell | grep '/lib')
VSN = $(shell erl -pa `pwd`/ebin -noshell -eval \
         'wordex:start(),{ok,Rev}=application:get_key(wordex,vsn),io:format("~s~n",[Rev])' \
          -s erlang halt | tail -n 1)
INSTALL_DIR =  $(ERL_LIB)/$(APP)-$(VSN)




all: src/wordex_scan.erl src/wordex_parse.erl compile
all_boot: all make_boot

src/wordex_scan.erl: src/wordex_scan.xrl
	$(ERL) -noshell -eval 'leex:file("$^")' -s erlang halt

src/wordex_parse.erl: src/wordex_parse.yrl
	$(ERL) -noshell -eval 'yecc:file("$^")' -s erlang halt


compile:
	@echo Compiling $(APP) from srcs
	$(ERLC) $(CFLAGS) $(SRC)/*.erl

edoc:
	@echo Generating $(APP) documentation from srcs
	@erl -noinput -eval \
             'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' \
             -s erlang halt

make_boot:
	@(cd $(EBIN); $(ERL) -pa $(EBIN) -noshell \
              -run make_boot write_scripts rest_app)

start: all
	@$(ERL) -pa $(PWD)/$(EBIN) -run $(APP)

test: all
	@$(ERL) -pa $(PWD)/$(EBIN) -noshell \
              -eval 'eunit:test(wordex,[verbose])' -s erlang halt

install:
	@echo Installing $(APP)-$(VSN)
	@echo Installing to $(INSTALL_DIR)
	if [ -d '$(INSTALL_DIR)' ];then rm -fr $(INSTALL_DIR); fi
	mkdir $(INSTALL_DIR)
	cp -r $(EBIN) $(INSTALL_DIR)
	cp -r $(PRIV) $(INSTALL_DIR)
	cp -r $(SRC) $(INSTALL_DIR)


uninstall: 
	if [ -d '$(INSTALL_DIR)' ];then rm -fr $(INSTALL_DIR); fi


clean:
	rm -f $(SRC)/wordex_scan.erl $(SRC)/wordex_parse.erl
	rm -f $(EBIN)/*.beam erl_crash.dump
	rm -f $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script
	rm -f $(DOC)/*.html $(DOC)/*.css $(DOC)/erlang.png doc/edoc-info

