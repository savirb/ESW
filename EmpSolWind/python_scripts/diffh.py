#!/usr/bin/env python3
import numpy as np
import argparse
from psihdf import rdhdf

# Get rid of warning when nan or zero value in divisor
np.seterr(divide='ignore', invalid='ignore')

def argParsing():

    parser = argparse.ArgumentParser(description='DIFFH: Compute comparison metrics between two PSI H5 files (excluding restarts).  The metrics are:   RMSD   MAXABS   CVRMSD  MAPE  MAXAPE')

    parser.add_argument('file1',
                        type=str,
                        help="First h5 file to compare.")

    parser.add_argument('file2',
                        type=str,
                        help="Second h5 to compare (reference).")

    return parser.parse_args()


# Beginning of comparison functions ---------------------------------------------------------

def getdiff_rmsd(x,y):
    N = np.size(x)
    d = np.sqrt(np.sum((x - y) ** 2)/N)
    return d

def getdiff_maxabs(x, y):
    d = np.max(np.abs(x - y))
    return d

def getdiff_cvrmsd(x, y):
    top = np.sum((x - y) ** 2)
    bot = np.sum(y ** 2)
    if bot == 0:
        d = -1
    else:
        d = np.sqrt(top/bot)
    return d

def getdiff_mape(x, y):
    N = np.size(x)
    d = np.sum(np.abs((x - y)/y))/N
    if d == float('inf') or np.isnan(d):
        d = -1
    return d

def getdiff_maxape(x, y):
    N = np.size(x)
    d = np.max(np.abs((x - y)/y))
    if d == float('inf') or np.isnan(d):
        d = -1
    return d

# End of comparison functions ---------------------------------------------------------------

args = argParsing()

_,_,_,fieldData1 = rdhdf(args.file1)
_,_,_,fieldData2 = rdhdf(args.file2)

listOfdiff = []
listOfdiff.append(getdiff_rmsd(fieldData1, fieldData2))
listOfdiff.append(getdiff_maxabs(fieldData1, fieldData2))
listOfdiff.append(getdiff_cvrmsd(fieldData1, fieldData2))
listOfdiff.append(getdiff_mape(fieldData1, fieldData2))
listOfdiff.append(getdiff_maxape(fieldData1, fieldData2))

out = ""

for value in listOfdiff:
    if value != -1 and  value != 0:
        out += "%23.17e," % value
    else: 
        out += "%23d," % value

print(out)
