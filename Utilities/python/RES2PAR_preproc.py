"""
Texture2Par "RES2PAR" Preprocessor
"""
import numpy as np
import pandas as pd
import flopy as fp
import pyemu
from tqdm import tqdm
from pathlib import Path
from scipy.stats import lognorm
import t2py
import re

#----------------------------------------------------------------------------------------------------------------------#
# Settings
#----------------------------------------------------------------------------------------------------------------------#

# Input Files
in_dir = Path('.')
tex_dist_file = in_dir / 'lognorm_dist_clustered.par'
out_dir = Path('.')

# temp
# pest_dir = Path('C:/Users/lelan/Documents/CodeProjects/PhD_SV_AEM_Uncert/04_PEST_setup')

# MODFLOW Model
mf_dir = Path('../MODFLOW')
model_name = 'svihm'
xoff = 499977
yoff = 4571330

# Res2Par related settings
max_outside_dist = 500
tailings_issue_logs = [18416, 18424, 18223, 18528, 18339]

# Include AEM/Lith KVME pilot points?
kvme_pp_flag = False

# Textures
texs = ['Fine', 'Mixed_Fine','Sand', 'Mixed_Coarse', 'Very_Coarse']

# Per-PP-set config (locations are fixed here; values come from PEST later)
PPSETS = {
    "scale_pp": {
        "targets": texs,                             # one param per texture (re-uses same factors)
        "dat_pattern": 'scale_pp_{tex}_L{lay}.dat',  # PEST will write this from tpl
        "fac_pattern": 'scale_pp_L{lay}.fac',        # pre-written factor file
        "colname": 'scale_{tex}',                    # Name it output file column
    },
    "kv_mult_pp": {
        "targets": ['kv_mult'],
        "dat_pattern": 'pp_kv_var_L{lay}.dat',
        "fac_pattern": 'kv_mult_pp_L{lay}.fac',
        "colname": 'kv_mult',
    },
}

kvme_pp = {
    "lth_var_pp": {
        "targets": ['lth_var'],
        "dat_pattern": 'pp_lth_var_L{lay}.dat',
        "fac_pattern": 'lth_var_pp_L{lay}.fac',
        "target_file": 'lth_var_pp_L{lay}_targets.csv',
    },
    "aem_var_pp": {
        "targets": ['aem_var'],
        "dat_pattern": 'pp_aem_var_L{lay}.dat',
        "fac_pattern": 'aem_var_pp_L{lay}.fac',
        "target_file": 'aem_var_pp_L{lay}_targets.csv',
    }
}

if kvme_pp_flag:
    PPSETS = PPSETS | kvme_pp

#----------------------------------------------------------------------------------------------------------------------#
# Functions/Classes
#----------------------------------------------------------------------------------------------------------------------#

def tpl2dat(tpl_path, default=0.0, out_path=None):
    """
    Convert a PEST/PEST++ template file to a .dat file by filling all
    parameter tokens with the provided default value. Used to setup files to test script

    Parameters
    ----------
    tpl_path : str or Path
        Path to the .tpl file.
    default : Any
        Default value to write in place of each parameter token.
    out_path : str or Path, optional
        Output .dat path. If None, replaces '.tpl' with '.dat'.

    Returns
    -------
    Path
        The path to the written .dat file.
    """
    tpl_path = Path(tpl_path)
    if out_path is None:
        out_path = tpl_path.with_suffix('').with_suffix('.dat') if tpl_path.suffix.lower() == '.tpl' \
                   else tpl_path.with_suffix(tpl_path.suffix + '.dat')
    out_path = Path(out_path)

    with tpl_path.open('r', newline='') as f:
        lines = f.readlines()

    if not lines:
        raise ValueError(f"Empty template: {tpl_path}")

    # Parse header: e.g., "ptf $" or "ptf ~"
    header = lines[0].strip()
    parts = header.split()
    if len(parts) < 2 or parts[0].lower() != 'ptf':
        raise ValueError(f"First line must be like 'ptf $' in {tpl_path}")
    delim = parts[1]  # single-character delimiter is standard
    if len(delim) != 1:
        # Be tolerant; take first char if someone wrote 'ptf  $'
        delim = delim[0]

    # Build a regex that matches <delim>  ...  <delim>, trimming inner whitespace
    pat = re.compile(rf'{re.escape(delim)}\s*(.*?)\s*{re.escape(delim)}')

    def _repl(_m):
        # replace any token with the default value (as string)
        return str(default)

    # Process body lines
    body = lines[1:]
    out_lines = [pat.sub(_repl, line.strip()) for line in body]

    out_path.write_text('\n'.join(out_lines)+'\n')
    return out_path

#----------------------------------------------------------------------------------------------------------------------#

def node_from_lrc_cols(df, mf, lay_col='layer', row_col='row', col_col='col'):
    # Not sure why flopy can't just do this correctly
    return mf.modelgrid.get_node(list(zip(df[lay_col].to_numpy(),
                                          df[row_col].to_numpy(),
                                          df[col_col].to_numpy()
                                          )))

#----------------------------------------------------------------------------------------------------------------------#

def model_to_grid_df(mf, xoff=0.0, yoff=0.0, remove_inactive=True):
    """
    Fast vectorized export of MODFLOW grid centers.

    Parameters
    ----------
    mf : flopy.modflow.Modflow
        Already‐loaded MODFLOW‐2005 / NWT model.
    xoff, yoff : float
        Optional extra offsets to add to the model grid (m).
    remove_inactive : bool, default True
        If True, rows with ibound==0 are dropped.

    Returns
    -------
    pd.DataFrame with columns:
        ['layer', 'row', 'col', 'node', 'X', 'Y', 'ibound']
    """
    grid = mf.modelgrid                    # StructuredGrid
    nlay, nrow, ncol = mf.dis.nlay, mf.dis.nrow, mf.dis.ncol

    # Centers
    x2d, y2d = grid.xcellcenters + xoff, grid.ycellcenters + yoff
    x3d = np.broadcast_to(x2d, (nlay, nrow, ncol))
    y3d = np.broadcast_to(y2d, (nlay, nrow, ncol))

    # Assemble layer center z-elevations
    top2d  = mf.dis.top.array
    botm3d = mf.dis.botm.array
    z3d = np.empty((nlay, nrow, ncol), dtype=float)
    for k in range(nlay):
        if k == 0:
            z3d[k] = 0.5 * (top2d + botm3d[k])
        else:
            z3d[k] = 0.5 * (botm3d[k - 1] + botm3d[k])

    # Layer, row, col indices - plus node id
    lay = np.arange(nlay)[:, None, None]
    row, col = np.indices((nrow, ncol))
    lay3d = np.broadcast_to(lay, (nlay, nrow, ncol))
    row3d = np.broadcast_to(row, (nlay, nrow, ncol))
    col3d = np.broadcast_to(col, (nlay, nrow, ncol))
    node = (lay * nrow * ncol + row * ncol + col)

    # ibound from BAS6
    ibnd = mf.bas6.ibound.array

    # Flatten once and build the frame
    df = pd.DataFrame({
        "node": node.ravel(order="C"),
        "layer":  lay3d.ravel(),
        "row":    row3d.ravel(),
        "col":    col3d.ravel(),
        "X":      x3d.ravel(),
        "Y":      y3d.ravel(),
        "Z":      z3d.ravel(),
        "ibound": ibnd.ravel(),
    }).set_index('node')

    if remove_inactive:
        df = df[df.ibound != 0]

    return df

#----------------------------------------------------------------------------------------------------------------------#

def attach_scale_and_stats(litho, grid_df, tex_dists, tex_col="tex"):
    """
    Vectorized replacement for the per-row loop:
      - pulls the per-cell scale from grid_df using node + chosen texture
      - computes logrho = log(scale)
      - sets RHO_I_STD (log-std) from tex_dists[tex][0]
    """
    # --- NEW: make a view with *_scale -> texture names ---
    expected_cols = [f"{t}_scale" for t in tex_dists.keys()]
    have = [c for c in expected_cols if c in grid_df.columns]
    missing = sorted(set(expected_cols) - set(have))
    if missing:
        raise KeyError(f"Missing scale columns in grid_df: {missing}")

    rename_map = {f"{t}_scale": t for t in tex_dists.keys() if f"{t}_scale" in grid_df.columns}
    g_scales = grid_df.rename(columns=rename_map)

    # 1) Align grid rows to litho order on node (fast index lookup)
    tex_cols = [t for t in tex_dists.keys() if t in g_scales.columns]
    g_aligned = g_scales.reindex(litho["node"].to_numpy())[tex_cols]

    # 2) Vectorized pick: column per-row based on litho[tex]
    col_idx = g_aligned.columns.get_indexer(litho[tex_col].to_numpy())
    if np.any(col_idx < 0):
        missing_tex = litho.loc[col_idx < 0, tex_col].unique().tolist()
        raise KeyError(f"Textures not found in grid_df columns: {missing_tex}")

    arr = g_aligned.to_numpy()
    row_idx = np.arange(len(litho))
    scale_vals = arr[row_idx, col_idx]

    # 3) Write results
    litho["RHO_I"] = scale_vals
    litho["logrho"] = np.log(scale_vals)

    shape_map = {k: v[0] for k, v in tex_dists.items()}  # log-std per texture
    litho["RHO_I_STD"] = litho[tex_col].map(shape_map).astype(float)

    return litho

#----------------------------------------------------------------------------------------------------------------------#

def aem2texture(rho, parameters, scales=None):
    probabilities = {}
    psum = 0.0
    for tex in parameters.keys():
        if scales is None:
            probabilities[tex] = lognorm.pdf(rho, s=parameters[tex][0], loc=parameters[tex][1],
                                             scale=parameters[tex][2])
        else:
            probabilities[tex] = lognorm.pdf(rho, s=parameters[tex][0], loc=parameters[tex][1],
                                             scale=scales[tex])
        psum += probabilities[tex]
    # Normalize:
    for tex in list(parameters.keys()):
        probabilities[tex] = probabilities[tex]/ psum
    return probabilities

#----------------------------------------------------------------------------------------------------------------------#
# Main
#----------------------------------------------------------------------------------------------------------------------#

# Read in MODFLOW model discretization
gwf = fp.modflow.Modflow.load((model_name + '.nam'), version='mfnwt', load_only=['dis','bas6'], model_ws=mf_dir)
nlay = gwf.dis.nlay

# Read in texture distribution priors
tex_dists_df = pd.read_table(tex_dist_file, sep='\\s+', skiprows=1)
tex_dists = tex_dists_df.set_index("Texture")[["Shape","Location","Scale"]].T.to_dict("list")

# Read in lithology logs
litho = pd.read_csv(in_dir / 'lithologs.csv')
litho['data_type'] = 'litho'

# Read in AEM resistivity values
aem = pd.read_csv(in_dir / 'aemlogs.csv')
aem['data_type'] = 'aem'
aem['WELL_INFO_ID'] = aem['LINE_NO'].astype(int).astype(str) + "_" + aem['FID'].astype(int).astype(str)

#----------------------------------------------------------------------------------------------------------------------#
# Pilot Point Kriging (with saved factors)
#----------------------------------------------------------------------------------------------------------------------#

# Get Kriging weights ("factors") for each point
grid_df = model_to_grid_df(gwf, xoff, yoff, remove_inactive=False)
for tex in tex_dists.keys():
    grid_df[tex + '_scale'] = np.nan
lth_df = None
aem_df = None

for tag, cfg in tqdm(PPSETS.items(), 'PP Set', total=len(PPSETS.keys())):
    for k in range(nlay):
        default_value = 0.0
        factor_file = in_dir / cfg["fac_pattern"].format(lay=k+1)
        if tag=="lth_var_pp" or tag=="aem_var_pp":
            target_map = pd.read_csv(in_dir / cfg["target_file"].format(lay=k+1))
        for i, tar in enumerate(cfg['targets']):
            if tag=='scale_pp':
                default_value = 1.0
                if i>0:
                    default_value = tex_dists[tar][2] / tex_dists[texs[i-1]][2]
            dat_file = in_dir / cfg["dat_pattern"].format(lay=k+1, tex=tar)
            # tpl2dat(tpl_path=pest_dir / (cfg["dat_pattern"].format(lay=k + 1, tex=tar) + '.tpl'),
            #         default=default_value,
            #         out_path=dat_file)
            kriged = pyemu.utils.geostats.fac2real(pp_file=str(dat_file),
                                                   factors_file=str(factor_file),
                                                   out_file=None)[0]
            if tag=="lth_var_pp":
                target_map['var_value'] = kriged
                lth_df = pd.concat([lth_df, target_map])
            elif tag=="aem_var_pp":
                target_map['var_value'] = kriged
                aem_df = pd.concat([aem_df, target_map])
            elif tag=='scale_pp':
                if i<1:  # base
                    grid_df.loc[grid_df['layer'] == k, tar + '_scale'] = kriged * tex_dists[tar][2]
                else:    # mult
                    prev = grid_df.loc[grid_df['layer']==k, texs[i-1] + '_scale'].values
                    grid_df.loc[grid_df['layer']==k, tar + '_scale'] = kriged * prev
            elif tag=='kv_mult_pp':
                grid_df.loc[grid_df['layer'] == k, 'kv_mult'] = kriged

# For RES2Par, add in (uniform) shape parameters
tex_cols = []
for tex in tex_dists.keys():
    grid_df[tex + '_shp'] = tex_dists[tex][0]
    tex_cols.append(tex + '_scale')
for tex in tex_dists.keys():
    tex_cols.append(tex + '_shp')

# Write files for RES2PAR
use_cols = ['layer'] + tex_cols
grid_df_out = grid_df[use_cols].copy()
grid_df_out['node'] = grid_df_out.index + 1  # 1-index for R2P
grid_df_out['node'] = grid_df_out['node'] - grid_df_out['layer']*gwf.nrow*gwf.ncol
grid_df_out['layer'] = grid_df_out['layer'] + 1  # 1-index for R2P
grid_df_out[['node']+use_cols].to_csv(out_dir / 'interp_tex_dists.csv', index=False)
print('Wrote Texture Distribution file: interp_tex_dists.csv')

use_cols = ['layer','kv_mult']
grid_df_out = grid_df[use_cols].copy()
grid_df_out['node'] = grid_df_out.index + 1  # 1-index for R2P
grid_df_out['node'] = grid_df_out['node'] - grid_df_out['layer']*gwf.nrow*gwf.ncol
grid_df_out['layer'] = grid_df_out['layer'] + 1  # 1-index for R2P
grid_df_out[['node']+use_cols].to_csv(out_dir / 'kv_mult.csv', index=False)
print('Wrote Texture Distribution file: kv_mult.csv')

#----------------------------------------------------------------------------------------------------------------------#
# Lithology Conversion
#----------------------------------------------------------------------------------------------------------------------#

# Get resistivity values
litho['RHO_I'] = np.nan
litho['RHO_I_STD'] = np.nan

# Get node ids
litho['node'] = node_from_lrc_cols(litho, gwf)

# Convert to resistivity, using per-cell scales, and add std
litho = attach_scale_and_stats(litho, grid_df, tex_dists, tex_col="tex")

# Get PP variance
if kvme_pp_flag:
    lth_df = lth_df.rename(columns={'Layer':'layer'})
    litho = pd.merge(litho, lth_df, how='left', on=['WELL_INFO_ID','layer'])
else:
    litho['var_value'] = 0.0

# Add pp "nugget" variance
litho['var_logrho'] = litho['RHO_I_STD']**2 + litho['var_value']

# # Test conversion back to texture
# for tex in tex_dists.keys():
#     litho[f're_{tex}'] = np.nan
# for idx, row in tqdm(litho.iterrows(), 'Interval', litho.shape[0]):
#     #grid_cell = grid_df.loc[(grid_df.row == row.row) & (grid_df.col == row.col) & (grid_df.layer == row.layer),]
#     grid_cell = grid_df.iloc[row.node,:]
#     retex = aem2texture(np.exp(litho.loc[idx,'logrho']), tex_dists, scales=grid_cell)
#     for tex in tex_dists.keys():
#         litho.loc[idx, f're_{tex}'] = retex[tex]

#----------------------------------------------------------------------------------------------------------------------#
# Write T2P Log
#----------------------------------------------------------------------------------------------------------------------#

# Work with the natural log of AEM
aem['logrho'] = np.log(aem['RHO_I'])

# Get node ids
#aem['node'] = node_from_lrc_cols(aem, gwf)

# Get PP variance
if kvme_pp_flag:
    aem_df = aem_df.rename(columns={'Layer':'layer'})
    aem = pd.merge(aem, aem_df, how='left', on=['WELL_INFO_ID','layer'])
else:
    aem['var_value'] = 0.0

# Add pp "nugget" variance
aem['var_logrho'] = aem['RHO_I_STD']**2 + aem['var_value']

# plt.scatter(aem.loc[aem['layer']==0,'x'], aem.loc[aem['layer']==0,'y'], c=aem.loc[aem['layer']==0,'var_value'], s=8)
# plt.gca().set_aspect('equal')
# plt.colorbar(label='var_value')
# plt.title('AEM var_value')

# Combine dataframes
use_cols = ['WELL_INFO_ID','x','y','row','col','layer','GROUND_SURFACE_ELEVATION_m','TOP_DEPTH_m','BOT_DEPTH_m','logrho','var_logrho','data_type']
resdf = pd.concat([litho[use_cols],aem[use_cols]])

# Assemble into Texture2Par log file
log = t2py.Dataset(classes=['logrho'], variance_col=True)
log.add_wells_by_df(df=resdf,
                    name_col='WELL_INFO_ID',
                    x_col='x', y_col='y',
                    zland_col='GROUND_SURFACE_ELEVATION_m',
                    depth_col='BOT_DEPTH_m', depth_top_col='TOP_DEPTH_m')
log.write_file(out_dir / 'res_log.csv', sep=',')
print('Wrote Texture Distribution file: res_log.csv')