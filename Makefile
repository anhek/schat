PROJECT = schat
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy jiffy
dep_cowboy = git https://github.com/ninenines/cowboy 2.6.1
dep_jiffy = git https://github.com/davisp/jiffy 0.15.2

include erlang.mk
