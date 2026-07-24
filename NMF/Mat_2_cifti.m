function Mat_2_cifti(mat,outFN,outFN_hp)

% convert input mat to a cifti, save to output filename. Filepaths/format designed for the group consensus .mat in particular

% add paths
ToolFolder='/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder';
addpath(genpath(ToolFolder));

% load mat
ciftimat=load(mat)
iv=ciftimat.initV;
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
emptymat = size(iv, 2);
full_cdata = zeros(20484, emptymat);
full_cdata(all_idxs, :) = iv;

% hard parcel version
[~,initV_max]=max(full_cdata,[],2);
templateDscalar.cdata(:,1)=initV_max;
% dmn.diminfo{2}.length=1
write_cifti(templateDscalar,outFN_hp)

contDscalar = templateDscalar;
n_comp = size(full_cdata, 2);
contDscalar.cdata = full_cdata;

% Set the correct type and length for multi-map dscalar
contDscalar.diminfo{2}.type = 'scalars';
contDscalar.diminfo{2}.length = n_comp;

% Use valid metadata structure from the template
valid_metadata = templateDscalar.metadata;

% Assign one map per component
contDscalar.diminfo{2}.maps = repmat(struct(), 1, n_comp);
for i = 1:n_comp
    contDscalar.diminfo{2}.maps(i).name = sprintf('Component_%d', i);
    contDscalar.diminfo{2}.maps(i).metadata = valid_metadata;
end

% Write to disk
write_cifti(contDscalar, outFN);

