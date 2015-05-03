PROJECT = httpclient

# Options

COMPILE_FIRST = httpclient_http_handler
CT_SUITES = offline

# Dependencies

DEPS = poolboy gun
dep_poolboy = git https://github.com/devinus/poolboy.git 1.2.0
dep_gun = git https://github.com/unix1/gun.git

# Standard targets

include erlang.mk
