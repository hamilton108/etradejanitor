#!/bin/bash 

docker run --user $(id -u):$(id -g) -it --rm -v $(pwd):/etradejanitor rcs/yfinance:1.0 /bin/bash


exit 0