"""
SWBM Streamflow File Pre-processor
"""
import pandas as pd

#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

multiplier_file = 'streamflow_multipliers.txt'
streamflow_files = ['subwatershed_nonirrigation_inflows_raw.txt', 'subwatershed_irrigation_inflows_raw.txt']
out_files = ['subwatershed_nonirrigation_inflows.txt', 'subwatershed_irrigation_inflows.txt']

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

print('STREAM_ADJUSTER.py')

multipliers = pd.read_table(multiplier_file, sep='\s+', index_col=0)

print(f'Multipliers read for {multipliers.shape[0]} streams')

for i, f in enumerate(streamflow_files):
    str_ts = pd.read_table(f, sep='\s+')

    # Identify all columns that have corresponding multipliers
    cols_to_scale = [c for c in str_ts.columns if c in multipliers.index]

    # Multiply each column by its multiplier
    str_ts[cols_to_scale] = str_ts[cols_to_scale].mul(multipliers.loc[cols_to_scale, "multiplier"].to_numpy(), axis=1)

    # Write to output file
    str_ts.to_csv(out_files[i], sep='\t', index=False)

    print(f'Wrote {out_files[i]}.')

print('Done - Streamflow Adjusted.')
