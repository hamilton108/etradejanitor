
import yfinance as y

TICKERS = [
    "AKERBP",
    "AKSO",
    "BAKKA",
    "BWLPG",
    "DNB",
    "DNO",
    # "EQNR",
    "GJF",
    "GOGL",
    # "MHG",
    "NAS",
    "NHY",
    "OBX",
    "ORK",
    "PGS",
    "REC",
    "SDRL",
    "STB",
    "SUBC",
    "TEL",
    "TGS",
    "TOM",
    "YAR"
]

PERIOD = "max"

for t in TICKERS:
    print("%s" % t)
    f = open("%s.csv" % t, "w")
    t = y.Ticker("%s.OL" % t)
    h = t.history(period=PERIOD)
    csv = h.to_csv()
    f.write(csv)
    f.close()
