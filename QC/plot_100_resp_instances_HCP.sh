#!/bin/bash

ml python/3.12
source ~/.bashrc
source /oak/stanford/groups/anishm/Adam/envs/seqNMF/bin/activate

BASEDIR="/oak/stanford/groups/anishm/fMRI_datasets/HCP-YA/FullyProcessed"
SCRIPT="/oak/stanford/groups/anishm/Adam/scripts/SNT_DMN-sgACC/QC/plot_resp_instance_HCP.py"

# ------------------------------------------------------------
# Get first 100 physio files
# ------------------------------------------------------------

mapfile -t PHYSIOS < <(
    find "$BASEDIR" \
        -maxdepth 2 \
        -type f \
        -name '*_Physio_log.txt' |
    sort |
    # fifth 100
    tail -n +401 | head -100
    #    head -100

)

echo "Found ${#PHYSIOS[@]} physio files"

# ------------------------------------------------------------
# Run plots serially
# ------------------------------------------------------------

COUNT=0

for PHYSIO in "${PHYSIOS[@]}"; do

    COUNT=$((COUNT + 1))

    FILE=$(basename "$PHYSIO")

    SUBJ=$(echo "$FILE" | cut -d'_' -f1)

    if [[ "$FILE" == *"REST1_LR"* ]]; then
        SCAN=1
        DIR=LR

    elif [[ "$FILE" == *"REST2_LR"* ]]; then
        SCAN=2
        DIR=LR

    elif [[ "$FILE" == *"REST1_RL"* ]]; then
        SCAN=1
        DIR=RL

    elif [[ "$FILE" == *"REST2_RL"* ]]; then
        SCAN=2
        DIR=RL

    else
        echo "Could not parse: $FILE"
        continue
    fi

    echo
    echo "============================================================"
    echo "[$COUNT/${#PHYSIOS[@]}] $SUBJ REST${SCAN}_${DIR}"
    echo "============================================================"

    python3 "$SCRIPT" "$SUBJ" "$SCAN" "$DIR"

done

echo
echo "============================================================"
echo "Done. Plotted $COUNT physio files."
echo "============================================================"
