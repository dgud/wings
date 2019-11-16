ifeq ($(WSLcross), true)
    ERL=erl.exe
    ERLC=erlc.exe
    ESCRIPT=escript.exe
else
    ERL=erl
    ERLC=erlc
    ESCRIPT=escript
endif
