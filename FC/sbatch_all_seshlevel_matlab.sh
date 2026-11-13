BIDS_DIR=/oak/stanford/groups/nolanw/BRAINS_August2025_DataFreeze/xcpd_Brains

for subj_path in "${BIDS_DIR}"/sub-s???; do
    subj=$(basename "$subj_path")
    subj=${subj#sub-}
    #for sesh in 2 3 4 5 6 7 8 9 10; do
    for sesh in 3 4 5 6 7 8; do
        sbatch sbatch_matlab.sh "$subj" "$sesh"
	sleep 6
    done
done

