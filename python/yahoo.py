
import yfinance as y

TICKERS = [
    #"AKERBP",
    "AKSO",
    "BAKKA",
    "BWLPG",
    "DNB",
    "DNO",
    "EQNR",
    "GJF",
    "GOGL",
    #"MHG",
    "NAS",
    "NHY",
    "OBX",
    "ORK",
    "PGS",
    #"REC",
    "SDRL",
    "STB",
    "SUBC",
    "TEL",
    "TGS",
    "TOM",
    "YAR"
]

# use "period" instead of start/end
# valid periods: 1d,5d,1mo,3mo,6mo,1y,2y,5y,10y,ytd,max

PERIOD = "1d"


def save_to_cvs():
    for t in TICKERS:
        try:
            print("%s" % t)
            f = open("/etradejanitor/feed/%s.csv" % t, "w")
            t = y.Ticker("%s.OL" % t)
            h = t.history(period=PERIOD)
            csv = h.to_csv()
            f.write(csv)
            f.close()
        except:
            print("Could not download: %s" % t)


if __name__ == '__main__':
    save_to_cvs()
