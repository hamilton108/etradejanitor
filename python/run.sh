#!/bin/bash 

docker run --user $(id -u):$(id -g) -it --rm -v $(pwd):/etradejanitor rcs/yfinance:0.1.67 /bin/bash

exit 0

