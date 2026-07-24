#!/bin/bash

# Directory containing BIDS subjects
BIDS_DIR="/oak/stanford/groups/nolanw/BRAINS_August2025_DataFreeze/xcpd_Brains"
# Loop over all directories matching sub-*
for subj_path in "${BIDS_DIR}"/sub-s???; do
        subj=$(basename "$subj_path")    # e.g. "sub-s021"
	echo "Submitting $subj"
	sbatch /oak/stanford/groups/anishm/Adam/scripts/SNT_DMN-sgACC/NMF/Step_0_DS_ts_fs5.sh "$subj"
	sleep 1
done
