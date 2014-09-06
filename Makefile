PROJECT = httpclient

# Options

COMPILE_FIRST = httpclient_http_handler
CT_SUITES = offline

# Dependencies

DEPS = poolboy gun
dep_poolboy = https://github.com/devinus/poolboy.git 1.2.0
dep_gun = https://github.com/unix1/gun.git fix-body-no-contentlength

# Standard targets

include erlang.mk
