#!/usr/bin/env python3
"""
Plot the raw respiration trace (z-scored). No fMRI/brain data, no peak detection.

Usage:
  python resp_trace_plot.py <subj> <sesh> <scan_num>

  scan_num: 1 (rs1) or 2 (rs2)
"""

import glob
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import sys
import os

# ──────────────────────────────────────────────────────────────────────────────
# Parse arguments
# ──────────────────────────────────────────────────────────────────────────────
if len(sys.argv) != 4:
    print("Usage: python resp_trace_plot.py <subj> <sesh> <scan_num>")
    print("  scan_num: 1 or 2")
    sys.exit(1)

subj = sys.argv[1]
sesh = sys.argv[2]
scan_num = int(sys.argv[3])

if scan_num not in [1, 2]:
    print("ERROR: scan_num must be 1 or 2")
    sys.exit(1)

scan_label = f"rs{scan_num}"

matplotlib.use('agg')
plt.rcParams['figure.dpi'] = 300

# ──────────────────────────────────────────────────────────────────────────────
# Load respiration data
# ──────────────────────────────────────────────────────────────────────────────
physio_dir = f'/oak/stanford/groups/nolanw/BRAINS_August2025_DataFreeze/physio/{subj}/ses-{sesh}/func'
physio_pattern = f'{physio_dir}/{subj}_ses-{sesh}_task-Resting{scan_num}NewHB6scan_physio.tsv.gz'
physio_files = glob.glob(physio_pattern)

if not physio_files:
    print(f"ERROR: No physio file found at {physio_pattern}")
    sys.exit(1)

physio_path = physio_files[0]
print(f"Loading respiration from: {physio_path}")

physio_df = pd.read_csv(physio_path, sep='\t', header=None)
print(f"  Loaded {len(physio_df)} rows, {physio_df.shape[1]} columns")

srate = 25  # Hz, sampling rate of the respiration belt
TR = 1.3    # seconds per fMRI volume -- only so the x-axis can be in "volume" units,

# Column 2 (0-indexed) of the physio file is RESPData
resp_full = physio_df.iloc[:, 2].values
resp_full = resp_full[~np.isnan(resp_full)]
print(f"  Non-NaN RESPData samples: {len(resp_full)}")

resp_ts = resp_full

resp_ts_zscore = (resp_ts - np.mean(resp_ts)) / np.std(resp_ts)
print(f"  Respiration trace: {len(resp_ts)} samples at {srate} Hz")

# Time axis in seconds
resp_time_sec = np.arange(len(resp_ts)) / srate

# ──────────────────────────────────────────────────────────────────────────────
# Plot
# ──────────────────────────────────────────────────────────────────────────────
fig, ax = plt.subplots(figsize=(12, 3))

ax.plot(resp_time_sec, resp_ts_zscore, color='darkorange', linewidth=0.5)
ax.axhline(0, color='gray', linestyle='-', linewidth=0.3, alpha=0.5)

ax.set_xlim([0, resp_time_sec[-1]])
ax.set_xlabel('Time (s)')
ax.set_ylabel('RESP (z)')
ax.set_title(f'{subj} ses-{sesh} {scan_label} respiration trace')

plt.tight_layout()

out_dir = f'/scratch/users/apines/{subj}'
os.makedirs(out_dir, exist_ok=True)
pofp = os.path.join(out_dir, f'{subj}_{sesh}_{scan_label}_resptrace.png')
plt.savefig(pofp, dpi=300)
plt.close()
print(f"  saved to: {pofp}")

print("donezo")
