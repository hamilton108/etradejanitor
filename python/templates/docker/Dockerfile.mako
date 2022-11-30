
FROM rcs/haskell-pg:9.4.3-slim

COPY main.sh /

COPY etradejanitor-${janitorVersion} /

ENTRYPOINT ["/bin/bash", "-c", "/main.sh <%text>${*}</%text>", "--"]
