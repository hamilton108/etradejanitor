
FROM rcs/haskell-pg:9.4.3-slim

COPY main.sh /

COPY etradejanitor-0.9.1 /

ENTRYPOINT ["/bin/bash", "-c", "/main.sh ${*}", "--"]
