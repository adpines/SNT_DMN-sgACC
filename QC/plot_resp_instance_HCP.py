#!/usr/bin/env python3
"""
Plot the raw HCP respiration trace (z-scored).

HCP Physio_log.txt format:
    column 0: acquisition/trigger
    column 1: respiration
    column 2: pulse oximetry / cardiac

Sampling rate: 400 Hz

Usage:
    python hcp_resp_trace_plot.py <subj> <scan_num> <direction>

Examples:
    python hcp_resp_trace_plot.py 100307 1 LR
    python hcp_resp_trace_plot.py 100307 2 LR
    python hcp_resp_trace_plot.py 100307 1 RL
    python hcp_resp_trace_plot.py 100307 2 RL
"""

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("agg")
import matplotlib.pyplot as plt

import sys
import os
import glob


# ──────────────────────────────────────────────────────────────────────────────
# Parse arguments
# ──────────────────────────────────────────────────────────────────────────────

if len(sys.argv) != 4:
    print("Usage: python hcp_resp_trace_plot.py <subj> <scan_num> <direction>")
    print("  scan_num: 1 or 2")
    print("  direction: LR or RL")
    sys.exit(1)

subj = sys.argv[1]
scan_num = int(sys.argv[2])
direction = sys.argv[3].upper()

if scan_num not in [1, 2]:
    print("ERROR: scan_num must be 1 or 2")
    sys.exit(1)

if direction not in ["LR", "RL"]:
    print("ERROR: direction must be LR or RL")
    sys.exit(1)


# HCP run name
hcp_run = f"rfMRI_REST{scan_num}_{direction}"


# ──────────────────────────────────────────────────────────────────────────────
# Paths
# ──────────────────────────────────────────────────────────────────────────────

physio_dir = (
    "/oak/stanford/groups/anishm/"
    "fMRI_datasets/HCP-YA/FullyProcessed/"
    f"sub-{subj}"
)

physio_pattern = (
    f"{physio_dir}/"
    f"{subj}_3T_{hcp_run}_Physio_log.txt"
)

physio_files = glob.glob(physio_pattern)

if not physio_files:
    print(f"ERROR: No physio file found at:")
    print(f"  {physio_pattern}")
    sys.exit(1)

physio_path = physio_files[0]

print(f"Loading respiration from:")
print(f"  {physio_path}")


# ──────────────────────────────────────────────────────────────────────────────
# Load HCP physio file
# ──────────────────────────────────────────────────────────────────────────────

physio_df = pd.read_csv(
    physio_path,
    sep=r"\s+",
    header=None
)

print(
    f"  Loaded {len(physio_df):,} rows, "
    f"{physio_df.shape[1]} columns"
)

if physio_df.shape[1] < 3:
    print("ERROR: Expected at least 3 columns in HCP Physio_log.txt")
    sys.exit(1)


# ──────────────────────────────────────────────────────────────────────────────
# Extract respiration
# ──────────────────────────────────────────────────────────────────────────────

srate = 400.0  # HCP physio sampling rate in Hz

# HCP:
#   column 0 = acquisition/trigger
#   column 1 = respiration
#   column 2 = pulse/cardiac
#
# Python is zero-indexed, so respiration = iloc[:, 1]

resp = pd.to_numeric(
    physio_df.iloc[:, 1],
    errors="coerce"
).values

# Remove NaNs
resp = resp[~np.isnan(resp)]

print(f"  Non-NaN respiration samples: {len(resp):,}")
print(f"  Sampling rate: {srate} Hz")
print(
    f"  Duration: {len(resp) / srate:.2f} seconds "
    f"({len(resp) / srate / 60:.2f} min)"
)


# ──────────────────────────────────────────────────────────────────────────────
# Z-score
# ──────────────────────────────────────────────────────────────────────────────

resp_mean = np.mean(resp)
resp_std = np.std(resp)

if resp_std == 0:
    print("ERROR: Respiration signal has zero variance")
    sys.exit(1)

resp_z = (resp - resp_mean) / resp_std


# ──────────────────────────────────────────────────────────────────────────────
# Time axis
# ──────────────────────────────────────────────────────────────────────────────

resp_time_sec = np.arange(len(resp_z)) / srate


# ──────────────────────────────────────────────────────────────────────────────
# Plot
# ──────────────────────────────────────────────────────────────────────────────

plt.rcParams["figure.dpi"] = 300

fig, ax = plt.subplots(figsize=(24, 3))

ax.plot(
    resp_time_sec,
    resp_z,
    color="darkorange",
    linewidth=0.5
)

ax.axhline(
    0,
    color="gray",
    linestyle="-",
    linewidth=0.3,
    alpha=0.5
)

ax.set_xlim(
    0,
    resp_time_sec[-1]
)

ax.set_xlabel("Time (s)")
ax.set_ylabel("RESP (z)")

ax.set_title(
    f"{subj} {hcp_run} respiration trace"
)

plt.tight_layout()


# ──────────────────────────────────────────────────────────────────────────────
# Save
# ──────────────────────────────────────────────────────────────────────────────

out_dir = f"/scratch/users/apines/{subj}"
os.makedirs(out_dir, exist_ok=True)

pofp = os.path.join(
    out_dir,
    f"{subj}_{hcp_run}_resptrace.png"
)

plt.savefig(
    pofp,
    dpi=300
)

plt.close()

print(f"Saved to:")
print(f"  {pofp}")

print("donezo")
