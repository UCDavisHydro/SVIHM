import pandas as pd
import argparse

#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

# GAGE file header
header = ['Time', 'Stage', 'Flow', 'Depth', 'Width',
          'Midpt-Flow', 'Precip', 'ET', 'Runoff',
          'Conductance', 'HeadDiff', 'Hyd_Grad']

# Setup parser
parser = argparse.ArgumentParser(description="Process streamflow data and compute monthly & water year sums.")
parser.add_argument("gage_file", type=str, help="Input file containing streamflow data.")
parser.add_argument("origin_date", type=str, help="Start date in YYYY-MM-DD format.")
parser.add_argument("sum_col", type=int, help="Index of the column to sum.")

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

if __name__ == "__main__":

    # Communicate
    print("GAGE2PAR.py")

    # parse args
    args = parser.parse_args()

    # process args
    sum_col = header[args.sum_col]
    origin_date = pd.to_datetime(args.origin_date)
    out_file = f"{args.gage_file.split('.')[0]}_VOL.out"

    # Read in
    print(f"Reading {args.gage_file}")
    gage = pd.read_table(args.gage_file, sep="\\s+", skiprows=1, names=header)

    # Setup dates
    gage['Date'] = [origin_date + pd.DateOffset(days=days) for days in gage['Time']]
    gage = gage.set_index('Date')

    # Sum
    wyearly = gage.resample('YS-OCT').sum()
    wyearly['WY'] = [f"WY_{d.year+1}" for d in wyearly.index]
    monthly = gage.resample('ME').sum()
    monthly['MY'] = [f"{d.year}_{d.month:02d}" for d in monthly.index]

    # Write to file (yearly, then monthly)
    print(f"Writing {out_file}")
    with open(out_file, 'w') as f:
        wyearly[['WY', sum_col]].to_csv(f, sep=" ", header=False, index=False, lineterminator='\n')
        monthly[['MY', sum_col]].to_csv(f, sep=" ", header=False, index=False, lineterminator='\n')