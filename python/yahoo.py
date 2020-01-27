
import yfinance as y

TICKERS = {
    "EQNR": "EQNR.OL"
}

PERIOD = "5d"

for t1, t2 in TICKERS.items():
    print("%s -> %s" % (t1, t2))
    f = open("%s.csv" % t1, "w")
    t = y.Ticker(t2)
    h = t.history(period=PERIOD)
    csv = h.to_csv()
    f.write(csv)
    f.close()
