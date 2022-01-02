
FROM rcs/haskell-pg:8.10.1

COPY main.sh /

COPY etradejanitor-0.9.0 /

ENTRYPOINT ["/bin/bash", "-c", "/main.sh ${*}", "--"]
