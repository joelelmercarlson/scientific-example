# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
"""
prob.py
"""
import csv
from pathlib import Path

__author__  = "Joel E Carlson"
__credits__ = [ "joel.elmer.carlson@outlook.com" ]
__email__   = __credits__[0]

PATH = Path.cwd()
DB_ROOT = f"{PATH}"
DB_PROB = f"{DB_ROOT}/prob_data.csv"

HEADERS = [
    "name"
    , "time"
    , "cost"
    , "network"
    , "scale"
    , "modalities"
    ]

def _process_row(rows: list) -> object:
    """
    _process_row
    """
    arr = zip (HEADERS, rows)
    record = {}
    for (k, v) in arr:
        record[k] = v
    return record

def bayes_theorem(prior_prob_a, likelihood_b_given_a, prob_b):
    """
    Calculates the posterior probability using Bayes' Theorem.

    Args:
        prior_prob_a (float): Prior probability of event A.
        likelihood_b_given_a (float): Likelihood of event B given event A.
        prob_b (float): Probability of event B.

    Returns:
        float: Posterior probability of event A given event B.
    """
    return (likelihood_b_given_a * prior_prob_a) / prob_b

def display(data: dict):
    """
    display
    """
    arr = {}
    for k, v in data.items():
        time = float(v["time"])
        cost = float(v["cost"])
        # scale
        complexity = float(v["scale"])
        prob_positive = (cost*time) + (complexity*(1-time))
        res_scale = bayes_theorem(time, cost, prob_positive)
        # modalities
        complexity = float(v["modalities"])
        prob_positive = (cost*time) + (complexity*(1-time))
        res_modalities = bayes_theorem(time, cost, prob_positive)
        value = [res_scale, res_modalities]
        try:
            arr[k].append(value)
        except KeyError:
            arr[k] = value
    for (k, v) in arr.items():
        print (f"{k} => scale={v[0]:.4f}, modalities={v[1]:.4f}")

def run():
    """
    run
    """
    arr = {}
    for filename in [ DB_PROB ]:
        with open(filename, "r", encoding="UTF-8") as csvfile:
            reader = csv.reader(csvfile, delimiter=",", quotechar='"')
            for row in reader:
                name = row[0]
                value = _process_row(row)
                if name == "name":
                    pass
                else:
                    arr[name] = value
    display(arr)

if __name__ == "__main__":
    run()
