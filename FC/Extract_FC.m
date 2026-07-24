function Extract_FC(subj,sesh,scan)
% extract FC values for this subject

% add needed paths
ToolFolder='/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder';
addpath(genpath(ToolFolder));
% extract FC values of interest from percy networks
subjFP=['/oak/stanford/groups/anishm/Adam/data/SingleParcellationBrains/PFMs_k17/Sub_sub-' subj '/IndividualParcel_Final_sbj1_comp17_alphaS21_3_alphaL30_vxInfo1_ard0_eta0/final_UV.mat'];
ciftimat=load(subjFP);
iv=ciftimat.V;
surfaceFolder='/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf';
% load in medial wall
surfL = [surfaceFolder '/lh_fs5.inflated'];
surfR = [surfaceFolder '/rh_fs5.inflated'];
% replacing these with 0-exclude masks (full include) to retain ROIs
surfML = [surfaceFolder '/lh_fs5.Medial_wall.label'];
surfMR = [surfaceFolder '/rh_fs5.Medial_wall.label'];
[surfStru, surfMask] = getFsSurf(surfL, surfR, surfML, surfMR);
templateDscalar=read_cifti([surfaceFolder '/fsaverage5_template.dscalar.nii']);
% populate dscalar
% Extract inclusion indices
idx_L = find(surfMask.l);
idx_R = 10242 + find(surfMask.r);
all_idxs = [idx_L; idx_R];
% Create empty matrix and insert iv
emptymat = size(iv{:,:}, 2);
full_cdata = zeros(20484, emptymat);
full_cdata(all_idxs, :) = iv{:,:};
% hard parcel version
[~,initV_max]=max(full_cdata,[],2);
templateDscalar.cdata(:,1)=initV_max;
% label 13 = DMN
DMN_Inds=find(initV_max==13);
% sep. left from right
DMN_IndsL=DMN_Inds(DMN_Inds<10243);
DMN_IndsR=DMN_Inds(DMN_Inds>10242)-10242;
% load in sgacc mask
brodFP='/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/atlases/Human.Brodmann09.32k_fs5_LR.dlabel.nii'
brod=read_cifti(brodFP);
% brod.diminfo{2}.maps.table has index: 83 is 25_B05
% get sgacc Inds
BA25=find(brod.cdata==83);
BA25_IndsL=BA25(BA25<10243);
BA25_IndsR=BA25(BA25>10242)-10242;

% load in time series (fs5)
parentFP='/scratch/groups/nolanw/BrainsNMFdata';
childFP=['/oak/stanford/groups/anishm/Adam/results/BRAINS/Functional/sub-' subj ];
tsLfp=[parentFP '/sub-' subj '_ses-V' num2str(sesh) '_Resting' num2str(scan) 'NewHB6scan_L.func.gii'];
tsRfp=[parentFP '/sub-' subj '_ses-V' num2str(sesh) '_Resting' num2str(scan) 'NewHB6scan_R.func.gii'];
tsL=gifti(tsLfp).cdata;
tsR=gifti(tsRfp).cdata;
% extract TS - sgacc
sgaCC_L=tsL(BA25_IndsL,:);
sgaCC_R=tsR(BA25_IndsR,:);
% extract TS - DMN
DMNL=tsL(DMN_IndsL,:);
DMNR=tsR(DMN_IndsR,:);

% extract entries corresp to DMN-sgacc
DMN_all = [DMNL; DMNR];
sgACC_all = [sgaCC_L; sgaCC_R];
% vertex-level DMN-sgacc FC
DM_SGmat = corr(DMN_all', sgACC_all');
% mean DMN-sgacc FC
DM_sgAv = mean(mean(DM_SGmat));
% construct output format
output=table({subj}, {sesh}, {scan}, {DM_sgAv});
writetable(output,[parentFP '/sub-' subj '_ses-V' num2str(sesh) '_Resting' num2str(scan) 'NewHB6scan_FC.csv']);
