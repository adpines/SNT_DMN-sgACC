#!/usr/bin/env python3

import glob
from PIL import Image

# third 100
files = sorted(
    glob.glob("/scratch/users/apines/*/*_rfMRI_REST*_resptrace.png")
)[400:500]

print(f"Found {len(files)} PNGs")

if not files:
    raise RuntimeError("No respiration plots found")

images = [Image.open(f).convert("RGB") for f in files]

out = "/scratch/users/apines/HCP_resp_fifth100.pdf"

images[0].save(
    out,
    save_all=True,
    append_images=images[1:]
)

print(f"Saved: {out}")
