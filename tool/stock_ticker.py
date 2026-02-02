# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "yfinance>=1.1.0",
# ]
# ///
import time
import yfinance as yf
import os

TICKERS = ["QQQ", "SPY"] # ETF
REFRESH_SECONDS = 60     # 1 minute

def clear():
    os.system("cls" if os.name == "nt" else "clear")

def get_price(ticker):
    data = yf.Ticker(ticker)
    info = data.history(period="1d", interval="1m")
    if info.empty:
        return None
    return float(info["Close"].iloc[-1])

def main():
    while True:
        clear()
        print("Live Stock Ticker\n")
        for t in TICKERS:
            price = get_price(t)
            if price is None:
                print(f"{t}: N/A")
            else:
                print(f"{t}: ${price:,.2f}")
        print(f"\nUpdated every {REFRESH_SECONDS} seconds. Ctrl+C to exit.")
        time.sleep(REFRESH_SECONDS)

if __name__ == "__main__":
    main()
