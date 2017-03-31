SHELL=/bin/bash -e

all: dx-api.el

dx-api.el: ../../src/api_wrappers/wrapper_table.json generateElispAPIWrappers.py
	cat ../../src/api_wrappers/wrapper_table.json | ./generateElispAPIWrappers.py > dx-api.el
