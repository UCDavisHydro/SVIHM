import pandas as pd
import numpy as np
import argparse
from hydroeval import evaluator, nse, rmse, kgeprime

#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

# HOB file header
header = ['Time', 'Stage', 'Flow', 'Depth', 'Width', 'MidptFlow', 'Precip', 'ET', 'Runoff', 'Conductance', 'HeadDiff', 'Grad']

# Setup parser
parser = argparse.ArgumentParser(description="Log Transform SFR Reach Output")
parser.add_argument("streamflow_file", type=str, help="SFR Output Reach file")
parser.add_argument("column", type=str, help="SFR Output Column")
parser.add_argument("--obs-csv", type=str, help="Path to obs CSV. If provided, metrics are computed.")

offset = 1e-1  # same as 05_A_Streamflow_Error_Models.py

origin_date = pd.to_datetime('1990-9-30')

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

if __name__ == "__main__":

    # Communicate
    print("LOGSTRSIM.py")

    # parse args
    args = parser.parse_args()

    # process args
    out_file = f"{args.streamflow_file.split('.')[0]}_{args.column}_LOG.out"

    # Check
    assert args.column in header

    # Read in
    print(f"Reading {args.streamflow_file}")
    sfr_out = pd.read_table(args.streamflow_file, sep="\\s+", skiprows=1, names=header)
    sfr_out[args.column] = np.log10(np.maximum(sfr_out[args.column], offset))

    # Write to file
    print(f"Writing {out_file}")
    sfr_out[['Time',args.column]].to_csv(out_file, sep=" ", header=False, index=False)

    # We got obs??
    if args.obs_csv:
        obs_df = pd.read_csv(args.obs_csv, parse_dates=['date'])

        # add date to simulated streamflow, merge
        sfr_out['date'] = origin_date + pd.to_timedelta(sfr_out['Time'], 'days')
        combined = obs_df.merge(sfr_out, how='left', on='date')

        # calc
        metrics = {'Time': ['NSE','KGE','RMSE'],
                   args.column: [
                evaluator(nse, combined[args.column], combined['obsval'])[0],
                evaluator(kgeprime, combined[args.column], combined['obsval'])[0][0],
                evaluator(rmse, combined[args.column], combined['obsval'])[0]
                           ]
        }

        # Append to output
        metrics_df = pd.DataFrame.from_dict(metrics)
        with open(out_file, 'a', newline='') as f:
            metrics_df[['Time', args.column]].to_csv(f, sep=" ", header=False, index=False)
