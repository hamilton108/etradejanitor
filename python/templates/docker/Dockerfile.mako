
FROM rcs/haskell-pg:8.10.1

COPY main.sh /

COPY etradejanitor-${janitorVersion} /

ENTRYPOINT ["/bin/bash", "-c", "/main.sh <%text>${*}</%text>", "--"]
