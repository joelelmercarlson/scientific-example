# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "flask>=3.1.2",
#     "yfinance>=1.1.0",
# ]
# ///
from flask import Flask, render_template, jsonify
import yfinance as yf

__author__  = "Joel E Carlson"
__credits__ = [ "joel.elmer.carlson@outlook.com" ]
__email__   = __credits__[0]

app = Flask(__name__)

TICKERS = ["QQQ", "SPY"] # ETF
REFRESH_SECONDS = 5      # 5 seconds

def clear():
    os.system("cls" if os.name == "nt" else "clear")

def get_price(ticker: str) -> float:
    data = yf.Ticker(ticker)
    info = data.history(period="1d", interval="1m")
    if info.empty:
        return None
    return float(info["Close"].iloc[-1])

@app.route("/")
def index():
    return render_template("ticker.html", tickers=TICKERS)

@app.route("/prices")
def prices():
    results = {}
    for t in TICKERS:
        price = get_price(t)
        results[t] = price
    return jsonify(results)

if __name__ == "__main__":
    app.run(debug=True)
