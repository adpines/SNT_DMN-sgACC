#!/usr/bin/env python3
import argparse
from pathlib import Path
import os
import numpy as np
import nibabel as nb
import matplotlib.pyplot as plt

# this is just for QC purposes. Nice to see the dervied surface ribbons in volumetric space at a few different locations.
# for Stanford's slurm cluster, Sherlock, you'd need to enter a python environment that can access the libraries above

# ---------- utils ----------
def robust_window(img, pmin=0.5, pmax=99.5):
    data = np.asanyarray(img.get_fdata(dtype=np.float32))
    finite = np.isfinite(data)
    vmin = np.percentile(data[finite], pmin)
    vmax = np.percentile(data[finite], pmax)
    return float(vmin), float(vmax)

def ribbon_bbox(mask, margin=10):
    """Axis-aligned bbox (voxel indices) around True voxels, with margin and bounds-clamp."""
    shape = mask.shape
    inds = np.argwhere(mask)
    if inds.size == 0:
        return [(0, s - 1) for s in shape]
    mins = inds.min(axis=0) + margin
    maxs = inds.max(axis=0) - margin
    #mins = np.clip(mins, 0, np.array(shape) - 1)
    #maxs = np.clip(maxs, 0, np.array(shape) - 1)
    return list(zip(mins, maxs))

def evenly_spaced_indices(lo, hi, n):
    lo = int(lo); hi = int(hi)
    if n <= 1:
        return [int((lo + hi) // 2)]
    xs = np.linspace(lo, hi, n)
    xs = np.unique(np.round(xs).astype(int))
    xs = np.clip(xs, lo, hi)
    # ensure we still return n if possible (duplicates can happen in tiny ranges)
    if len(xs) < n and hi - lo + 1 >= n:
        xs = np.linspace(lo, hi, n).round().astype(int)
    return list(xs)

# ---------- plotting ----------
def plot_plane(ax, vol, gm_mask, wm_mask, plane, index, vlim,
               lw_ctx=.8, lw_wm=.8, alpha_ctx=0.95, alpha_wm=0.95):
    """plane in {'ax','cor','sag'}; draws GM (solid) and WM (dashed) contours."""
    if plane == 'ax':       # z fixed -> show i (x) vs j (y)
        img = vol[:, :, index]
        gm  = gm_mask[:, :, index]
        wm  = wm_mask[:, :, index]
        ax.set_title(f'Coronal  z={index}')
    elif plane == 'cor':    # y fixed -> show i (x) vs k (z)
        img = vol[:, index, :]
        gm  = gm_mask[:, index, :]
        wm  = wm_mask[:, index, :]
        ax.set_title(f'Axial y={index}')
    elif plane == 'sag':    # x fixed -> show j (y) vs k (z)
        img = vol[index, :, :]
        gm  = gm_mask[index, :, :]
        wm  = wm_mask[index, :, :]
        # transpose to keep left-right visually consistent
        img = img.T; gm = gm.T; wm = wm.T
        ax.set_title(f'Sagittal x={index}')
    else:
        raise ValueError("plane must be one of {'ax','cor','sag'}")

    ax.imshow(np.rot90(img), cmap='gray', vmin=vlim[0], vmax=vlim[1], origin='lower')

    if np.any(gm):
        ax.contour(np.rot90(gm.astype(float)), levels=[0.5],
                   linewidths=lw_ctx, alpha=alpha_ctx,colors='red')
    if np.any(wm):
        ax.contour(np.rot90(wm.astype(float)), levels=[0.5],
                   linewidths=lw_wm, alpha=alpha_wm,colors='blue')

    ax.set_axis_off()

# ---------- main reporting ----------
def make_report(anat_img, ribbon_img, n_cuts, out_png, also_save_slices=False, prefix=None):
    # keep original orientation as requested
    anat = anat_img
    ribbon = ribbon_img

    vol = np.asanyarray(anat.get_fdata(dtype=np.float32))
    labels = np.asarray(ribbon.get_fdata(), dtype=np.int32)

    # FreeSurfer ribbon defaults
    wm_mask = np.isin(labels, (2, 41))   # LH/RH white
    gm_mask = np.isin(labels, (3, 42))   # LH/RH cortex

    vlim = robust_window(anat)

    # Cuts based on GM bbox
    (xmin, xmax), (ymin, ymax), (zmin, zmax) = ribbon_bbox(gm_mask, margin=10)
    xs = evenly_spaced_indices(xmin, xmax, n_cuts)
    ys = evenly_spaced_indices(ymin, ymax, n_cuts)
    zs = evenly_spaced_indices(zmin, zmax, n_cuts)

    # Montage: 3 rows (sag, cor, ax) by n_cuts
    fig, axes = plt.subplots(3, n_cuts, figsize=(3*n_cuts, 9), constrained_layout=True)
    if n_cuts == 1:
        axes = np.array([[axes[0]], [axes[1]], [axes[2]]])

    for i, x in enumerate(xs):
        plot_plane(axes[0, i], vol, gm_mask, wm_mask, 'sag', x, vlim)
    for i, y in enumerate(ys):
        plot_plane(axes[1, i], vol, gm_mask, wm_mask, 'cor', y, vlim)
    for i, z in enumerate(zs):
        plot_plane(axes[2, i], vol, gm_mask, wm_mask, 'ax', z, vlim)

    fig.suptitle('Ribbon alignments', fontsize=14)
    out_png = Path(out_png)
    out_png.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_png, dpi=200)
    plt.close(fig)

    if also_save_slices:
        # interpret prefix: directory vs file prefix
        pref_arg = prefix or out_png.with_suffix('').as_posix()
        pref = Path(pref_arg)
        # treat as directory if it exists or if user passed a trailing slash
        if pref.is_dir() or str(pref_arg).endswith(('/', '\\')):
            outdir = pref
            basename = 'slice'
        else:
            outdir = pref.parent
            basename = pref.name
        outdir.mkdir(parents=True, exist_ok=True)

        def save_one(plane, idx):
            f = outdir / f"{basename}_{plane}_{idx:04d}.png"
            fig, ax = plt.subplots(figsize=(3, 3))
            plot_plane(ax, vol, gm_mask, wm_mask, plane, idx, vlim)
            fig.savefig(f, dpi=200)
            plt.close(fig)

        for x in xs: save_one('sag', x)
        for y in ys: save_one('cor', y)
        for z in zs: save_one('ax',  z)

def main():
    ap = argparse.ArgumentParser(description="Generate a ribbon (GM/WM) alignment visual report.")
    ap.add_argument("--anat", required=True, help="Path to anatomical .mgz (e.g., brain.mgz)")
    ap.add_argument("--ribbon", required=True, help="Path to ribbon .mgz (segmentation/labels)")
    ap.add_argument("--out", required=True, help="Output PNG path")
    ap.add_argument("--cuts", type=int, default=7, help="Number of slices per plane within ribbon bbox")
    ap.add_argument("--dump_slices", action="store_true", help="Also save individual slice PNGs")
    ap.add_argument("--prefix", default=None,
                    help="Directory (trailing slash) or file prefix for slice PNGs when --dump_slices is set")
    args = ap.parse_args()

    anat_img = nb.load(args.anat)
    ribbon_img = nb.load(args.ribbon)
    make_report(anat_img, ribbon_img, args.cuts, args.out, args.dump_slices, args.prefix)

if __name__ == "__main__":
    main()

