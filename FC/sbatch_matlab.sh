#!/bin/bash
#
#SBATCH --job-name=DS_lag
#SBATCH --time=1:59:00
#SBATCH -n 1
#SBATCH --mem=20G
#SBATCH -p owners,anishm,normal  # Queue names you can submit to
# Outputs ----------------------------------
#SBATCH --mail-user=dontemailmeapines@stanford.edu
#SBATCH --mail-type=ALL
# ------------------------------------------

subj=$1
sesh=$2

module load matlab
ml biology
ml freesurfer
ml workbench

# set up paths for DS check at 24p
#childfp=/scratch/users/apines/data/BRAINS24p/sub-${subj}/ses-V${sesh}

# scans to loop over
if [ "$sesh" == "2" ]; then
    scans="1 2 3 4"
else
    scans="1 2"
fi

#./DS_surf_ts_BRAINS_fs5.sh sub-${subj} ses-V${sesh}

for scan in $scans; do
#    tsL=${childfp}/sub-${subj}_ses-V${sesh}_L_rs${scan}_TS_10k.func.gii
#    tsR=${childfp}/sub-${subj}_ses-V${sesh}_R_rs${scan}_TS_10k.func.gii
#    if [ ! -f "$tsL" ] || [ ! -f "$tsR" ]; then
#        echo "10k files not found for scan ${scan}, running downsample for sub-${subj} ses-V${sesh}"
#        bash /oak/stanford/groups/anishm/Adam/scripts/BRAINS/DS_surf_ts_BRAINS_fs5_24p.sh "sub-${subj}" "ses-V${sesh}"
#    fi
#    matlab -nodisplay -r "Extract_Func('$subj','$sesh','$scan'); exit"
#     matlab -nodisplay -r "Extract_NetworkFlow_fs5_Bilat('$subj','ses-V$sesh','$scan'); exit"
#     matlab -nodisplay -r "Extract_FC_Lag('$subj','$sesh','$scan'); exit"
     matlab -nodisplay -r "Extract_FC_sgacc_fullbrain('$subj','$sesh','$scan'); exit"
done

#/oak/stanford/groups/anishm/Adam/scripts/BRAINS/Alff_fsaverage5.sh ${subj}
#matlab -nodisplay -r "GenAlffSlope('sub-$subj')"
#ml biology
#ml workbench
#/oak/stanford/groups/anishm/Adam/scripts/BRAINS/TS_STD.sh ${subj}
#/oak/stanford/groups/anishm/Adam/scripts/BRAINS/DS_surf_Networks_fs5tofs4.sh sub-${subj}
#matlab -nodisplay -r "compute_FAI('$bold_fp'); exit"
#matlab -nodisplay -r "PostProc_BRAINS('$subj')"
#matlab -nodisplay -r "plot_fixed_effects"
#matlab -nodisplay -r "Extract_Power_Simplified('$subj',$sesh,$scan)"
#matlab -nodisplay -r "plot_ranslopes('$subj')"
#matlab -nodisplay -r "Extract_Network_FuncMetrics36p('$subj')"
#matlab -nodisplay -r "Extract_Network_CT('$subj')"
#matlab -nodisplay -r "mats_to_txt"
#matlab -nodisplay -r "network_tc_subject('$subj')"
#matlab -nodisplay -r "Extract_Efield_FCvec"
#matlab -nodisplay -r "SimStreams_to_z"
#matlab -nodisplay -r "Extract_FCvec('$subj')"
#matlab -nodisplay -r "BRAINS_Stream_TaskMaster"
#matlab -nodisplay -r "Extract_SD_dkt('$subj',$sesh,$scan)"
