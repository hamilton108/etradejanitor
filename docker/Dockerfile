
FROM rcs/haskell-pg:8.10.1

COPY main.sh /

COPY etradejanitor-1.6.1 /

ENTRYPOINT ["/bin/bash", "-c", "/main.sh ${*}", "--"]
