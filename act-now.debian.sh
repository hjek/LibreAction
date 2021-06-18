#!/bin/sh

if ! which swipl; then
	sudo apt install swi-prolog
fi
./source/act_now.pl
