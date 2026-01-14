#!/usr/bin/env python3
import argparse
import numpy as np
import pandas as pd
from pathlib import Path
from typing import List, Tuple
from sklearn.metrics import r2_score

#----------------------------------------------------------------------------------------------------------------------#
# Setup
#----------------------------------------------------------------------------------------------------------------------#
vertical_well_pairs: List[Tuple[str, str]] = [
    ("ST201", "ST201_2"),
    ("ST786", "ST786_2"),
]
base_sigma = 1.0

#----------------------------------------------------------------------------------------------------------------------#
# Functions
#----------------------------------------------------------------------------------------------------------------------#
def parse_well_and_reltime(obsnme: str):
    """
    Expect original HOB names like 'WELL.1234' (before we added suffixes in Script #1).
    Returns (wellid, reltime:int or None).
    """
    parts = str(obsnme).split(".")
    if len(parts) >= 2 and parts[1].isdigit():
        return parts[0], int(parts[1])
    # Sometimes names may have other patterns; fallback to well only
    return parts[0], None

def weighted_mean(values: np.ndarray, weights: np.ndarray) -> float:
    sw = weights.sum()
    if sw <= 0:
        # fallback unweighted mean (will be written but carry zero weight in AVG group)
        return float(np.mean(values)) if len(values) > 0 else np.nan
    return float(np.average(values, weights=weights))

def build_sim_sets(hob_out: pd.DataFrame, obs_master: pd.DataFrame) -> pd.DataFrame:
    """
    Build simulated AVG/DIFF/VDIFF in the same way as Script #1 transformed observations.
    Returns a tidy dataframe with columns: ['obsnme','simval_t','group','wellid'].
    """
    # 1) Extract per-observation weights from obs_master DIFF rows.
    #    These are the weights that defined the *mask* and (if positive) contribute to the simulated mean.
    diff_mask = obs_master[obs_master["group"] == "hds_diff"].copy()
    # original obsnme = DIFF obsnme without the "_DM" suffix
    diff_mask["orig_obsnme"] = diff_mask["obsnme"].str.replace("_DM$", "", regex=True)
    obs_wt_map = diff_mask.set_index("orig_obsnme")["weight"].to_dict()

    # 2) Prepare HOB sim table with well and reltime parsed from original names
    hob = hob_out.copy()
    hob[["wellid", "reltime"]] = hob["obsnme"].apply(lambda s: pd.Series(parse_well_and_reltime(s)))
    # attach mask weights (0 where missing)
    hob["wt"] = hob["obsnme"].map(obs_wt_map).fillna(0.0)

    # 3) Per-well simulated weighted mean (wt>0); fallback to unweighted mean if sum_w==0
    sim_means = []
    for well, g in hob.groupby("wellid"):
        vals = g["simval"].to_numpy(dtype=float)
        wts  = g["wt"].to_numpy(dtype=float)
        sim_mu = weighted_mean(vals, wts)
        sim_means.append((well, sim_mu, wts.sum()))
    sim_means = pd.DataFrame(sim_means, columns=["wellid", "sim_mean", "sumw"])
    # AVG rows: name, sim value
    avg_rows = sim_means.copy()
    avg_rows["obsnme"] = avg_rows["wellid"] + "_AVG"
    avg_rows["group"]  = "hds_avg"
    avg_rows["simval_t"] = avg_rows["sim_mean"]
    avg_rows = avg_rows[["obsnme", "simval_t", "group", "wellid"]]

    # 4) DIFF rows: for each original HOB row, sim − simulated mean
    hob = hob.merge(sim_means[["wellid", "sim_mean"]], on="wellid", how="left")
    hob["simval_t"] = hob["simval"] - hob["sim_mean"]
    hob["obsnme_t"] = hob["obsnme"]  #+ "_DM"
    # drop the old obsnme before renaming to avoid duplicate label
    diff_rows = hob.drop(columns=["obsnme"]).rename(columns={"obsnme_t": "obsnme"})
    diff_rows["group"] = "hds_diff"
    diff_rows = diff_rows[["obsnme", "simval_t", "group", "wellid"]]

    # 5) VDIFF rows: top − bottom, matched by reltime
    vdiff_list = []
    for top_well, bot_well in vertical_well_pairs:
        top = hob[(hob["wellid"] == top_well)][["reltime", "simval"]].rename(columns={"simval":"top_val"})
        bot = hob[(hob["wellid"] == bot_well)][["reltime", "simval"]].rename(columns={"simval":"bot_val"})
        m = pd.merge(top, bot, on="reltime", how="inner")
        if m.empty:
            continue
        m = m.sort_values("reltime").reset_index(drop=True)
        m["simval_t"] = m["top_val"] - m["bot_val"]
        # Use the same names that exist in obs_master (don’t re-invent):
        # Find the subset of obs_master names for this pair in order
        names_expected = obs_master[(obs_master["group"]=="hds_vdiff") &
                                    (obs_master["wellid"]==top_well)]["obsnme"].tolist()
        # If lengths match, use expected names; else fallback to enumerated names
        if len(names_expected) == len(m):
            m["obsnme"] = names_expected
        else:
            m["obsnme"] = [f"{top_well}_VD.{i}" for i in m.index]
        m["group"] = "hds_vdiff"
        m["wellid"] = top_well
        vdiff_list.append(m[["obsnme", "simval_t", "group", "wellid"]])

    vdiff_rows = pd.concat(vdiff_list, ignore_index=True) if vdiff_list else pd.DataFrame(
        columns=["obsnme","simval_t","group","wellid"]
    )

    # 6) Combine
    sim_all = pd.concat([avg_rows, diff_rows, vdiff_rows], ignore_index=True)
    return sim_all

# -----------------------------
# Main
# -----------------------------
if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Post-process HOB file")
    ap.add_argument("hob_out", help="MODFLOW HOB output file with columns: simval obval obsnme")
    ap.add_argument("obs_master_csv", help="CSV of pre-processed obs")
    ap.add_argument("-o", "--out", default="head_obs_for_pest.out",
                    help="Output text file with '<obsnme> <value>' lines")
    args = ap.parse_args()

    # Read inputs
    hob_out = pd.read_table(args.hob_out, sep=r"\s+", skiprows=1, names=["simval","obval","obsnme"])
    obs_master = pd.read_csv(args.obs_master_csv)

    # Build transformed simulated sets
    sim_all = build_sim_sets(hob_out, obs_master)

    # Join with obs_master to align ordering and compute weighted R^2
    # Keep only obs names present in obs_master (STAT row handled separately)
    obs_m = obs_master[obs_master["obsnme"] != "R2_HEADS"].copy()
    merged = obs_m.merge(sim_all, on=["obsnme","group","wellid"], how="left", validate="one_to_one")

    hob_w = hob_out.merge(obs_master[['obsnme','weight']], on='obsnme')
    r2 = r2_score(hob_w['obval'], hob_w['simval'], sample_weight=hob_w['weight'])

    # Prepare final output order exactly as INS expects: same sort used in Script #1
    order = obs_master.sort_values(["group","wellid","obsnme"]).copy()

    # Map simulated values (simval_t) into that order; STAT gets R2
    sim_map = merged.set_index("obsnme")["simval_t"].to_dict()
    out_lines = []
    for _, row in order.iterrows():
        name = row["obsnme"]
        if name == "R2_HEADS":
            val = r2
        else:
            val = sim_map.get(name, np.nan)
        out_lines.append(f"{name} {val:.6f}")

    # Write
    out_path = Path(args.out)
    out_path.write_text("\n".join(out_lines) + "\n", encoding="utf-8")
    print(f"Wrote {out_path} with {len(out_lines)} lines.")
