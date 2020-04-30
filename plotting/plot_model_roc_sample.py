# An example to plot roc curve

import argparse
from os.path import expanduser, abspath
import numpy as np
import matplotlib

matplotlib.use('Agg')
from matplotlib.backends.backend_pdf import PdfPages
import pylab as plt
import pandas as pd
from sklearn.metrics import auc, roc_auc_score


def auc(x):
    try:
        return pd.Series({"auc": roc_auc_score(x["y_test"], x["y_pred"])})
    except ValueError:
        return pd.Series({"auc": -1})


def plot_auc_scatter(pred_file, stats_file, output_file, plt_title, xaxis):
    pred = pd.read_csv(pred_file)
    stats_df = pd.read_csv(stats_file)

    pred_req_cols = ["userid", "epoch", "y_test", "y_pred"]
    stats_req_cols = ["userid", "hy_train", "nhy_train"]

    if not set(pred_req_cols) <= set(list(pred)):
        raise IndexError("Required columns: %s" % (", ".join(a for a in pred_req_cols)))

    if not set(stats_req_cols) <= set(list(stats_df)):
        raise IndexError("Required columns: %s" % (", ".join(a for a in stats_req_cols)))

    auc_df = pred.groupby(["userid"]).apply(auc).reset_index()

    sorted_auc = auc_df.sort_values(by=['auc'],ascending=True)

    for index, row in sorted_auc.iterrows():
	print "%d %f" %(row['userid'],row['auc'])

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-p', help='Name of the predictions file.', required=True)
    parser.add_argument('-s', help='Name of stats file.', required=True)
    parser.add_argument('-o', help='Name of output file', required=True)
    parser.add_argument('-t', help='Plot title', default="")
    parser.add_argument('-x', help='What to put on the x-axis', default="total", choices=["skew", "hy_train", "total"])

    opt = parser.parse_args()
    opt.p = abspath(expanduser(opt.p))
    opt.o = abspath(expanduser(opt.o))
    opt.s = abspath(expanduser(opt.s))

    plot_auc_scatter(opt.p, opt.s, opt.o, opt.t, opt.x)
