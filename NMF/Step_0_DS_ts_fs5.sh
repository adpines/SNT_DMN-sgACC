#!/bin/sh
#SBATCH -J DS_brains
#SBATCH --time=1:30:00
#SBATCH -n 1
#SBATCH --cpus-per-task=1
#SBATCH --mem=20G
#SBATCH -p anishm,normal,owners
#SBATCH --mail-user=apines@stanford.edu
#SBATCH --mail-type=ALL

# This is just to downsample fslr timeseries to fsaverage5 (92k cortical vertices to ~20k). Conversion to .mgh format also included for easier uptake into NMF.
# Called by sbatch_all_DS.sh, which sbatches (equivalent to qsub on sun grid engine clusters) this job for each subject

ml biology
ml workbench

subj=$1

# set freesurfer dir
export FREESURFER_HOME=/share/software/user/open/freesurfer/6.0.0
# set subjs dir
export SUBJECTS_DIR=/share/software/user/open/freesurfer/6.0.0/subjects
# set freesurfer license
export FS_LICENSE=/oak/stanford/groups/anishm/Adam/license.txt

# rs fp
parentfp=/oak/stanford/groups/nolanw/BRAINS_August2025_DataFreeze/xcpd_Brains/${subj}
childfp=/scratch/groups/nolanw/BrainsNMFdata

# template paths
sphereL_in="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fs_LR-deformed_to-fsaverage.L.sphere.32k_fs_LR.surf.gii"
sphereL_out="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fsaverage5_std_sphere.L.10k_fsavg_L.surf.gii"
areaL_in="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fs_LR.L.midthickness_va_avg.32k_fs_LR.shape.gii"
areaL_out="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fsaverage5.L.midthickness_va_avg.10k_fsavg_L.shape.gii"
sphereR_in="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fs_LR-deformed_to-fsaverage.R.sphere.32k_fs_LR.surf.gii"
sphereR_out="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fsaverage5_std_sphere.R.10k_fsavg_R.surf.gii"
areaR_in="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fs_LR.R.midthickness_va_avg.32k_fs_LR.shape.gii"
areaR_out="/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf/resample_fsaverage/fsaverage5.R.midthickness_va_avg.10k_fsavg_R.shape.gii"

# loop over all sessions and matching scans
scan_count=0
find "$parentfp" -type f -name "*desc-denoisedSmoothed_bold.dtseries.nii" | sort | while read -r scan; do
    scan_count=$((scan_count + 1))
    # extract session name and task identifier
    ses=$(echo "$scan" | sed -n 's|.*/ses-\([^/]*\)/func/.*|\1|p')
    task=$(basename "$scan" | sed -n 's/.*task-\([^_]*\)_space.*/\1/p')
    # define temp outputs
    tempL="${childfp}/${subj}_ses-${ses}_scan${scan_count}_tempL.func.gii"
    tempR="${childfp}/${subj}_ses-${ses}_scan${scan_count}_tempR.func.gii"
    left_tmp="${childfp}/${subj}_ses-${ses}_scan${scan_count}_L.func.gii"
    right_tmp="${childfp}/${subj}_ses-${ses}_scan${scan_count}_R.func.gii"
    # lil echo update
    echo "Processing $subj, session $ses, scan $scan_count..."
    # separate hemispheres
    wb_command -cifti-separate "$scan" COLUMN -metric CORTEX_LEFT "$tempL"
    wb_command -cifti-separate "$scan" COLUMN -metric CORTEX_RIGHT "$tempR"
    # resample to fsaverage5
    wb_command -metric-resample "$tempL" "$sphereL_in" "$sphereL_out" ADAP_BARY_AREA "$left_tmp" -area-metrics "$areaL_in" "$areaL_out"
    wb_command -metric-resample "$tempR" "$sphereR_in" "$sphereR_out" ADAP_BARY_AREA "$right_tmp" -area-metrics "$areaR_in" "$areaR_out"
    # convert to mgh
    /share/software/user/open/freesurfer/6.0.0/bin/mri_convert.bin "$left_tmp" "${left_tmp%.func.gii}.mgh"
    /share/software/user/open/freesurfer/6.0.0/bin/mri_convert.bin "$right_tmp" "${right_tmp%.func.gii}.mgh"
    # cleanup temp intermediates
    rm $tempL
    rm $tempR
    #rm $left_tmp
    #rm $right_tmp
done
