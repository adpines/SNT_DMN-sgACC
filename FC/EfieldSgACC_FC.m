function EfieldSgACC_FC(subj)
% EfieldSgACC_FC.m
% Compute FC between efield ROI seed and sgACC (BA25) L and R for one subject.
% Outputs one CSV with one row per scan.
%
% Usage: EfieldSgACC_FC('sub-s001')

ToolFolder = '/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder';
addpath(genpath(ToolFolder));

surfaceFolder = '/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf';

% -------------------------
% Load surface mask (medial wall -> inclusion indices)
% -------------------------
surfL  = [surfaceFolder '/lh_fs5.inflated'];
surfR  = [surfaceFolder '/rh_fs5.inflated'];
surfML = [surfaceFolder '/lh_fs5.Medial_wall.label'];
surfMR = [surfaceFolder '/rh_fs5.Medial_wall.label'];
[~, surfMask] = getFsSurf(surfL, surfR, surfML, surfMR);
idx_L    = find(surfMask.l);
idx_R    = 10242 + find(surfMask.r);
all_idxs = [idx_L; idx_R];

% -------------------------
% Load sgACC (BA25) mask
% label 83 = BA25 per brod.diminfo{2}.maps.table
% -------------------------
brodFP = '/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/atlases/Human.Brodmann09.32k_fs5_LR.dlabel.nii';
brod   = read_cifti(brodFP);
BA25       = find(brod.cdata == 83);
BA25_IndsL = BA25(BA25 < 10243);
BA25_IndsR = BA25(BA25 > 10242) - 10242;

% -------------------------
% Load efield for this subject
% -------------------------
efieldFPL = g_ls(['/oak/stanford/groups/nolanw/BRAINS_August2025_DataFreeze/efields/' subj '/surface/' subj '_ses-V*_hemi-lh_normE_fsaverage5.func.gii']);
efieldFPR = g_ls(['/oak/stanford/groups/nolanw/BRAINS_August2025_DataFreeze/efields/' subj '/surface/' subj '_ses-V*_hemi-rh_normE_fsaverage5.func.gii']);
if isempty(efieldFPL)
    fprintf('Missing efield for %s, exiting\n', subj);
    return;
end

efL = gifti(efieldFPL{1}).cdata;   % 10242 x 1
efR = gifti(efieldFPR{1}).cdata;   % 10242 x 1
ef  = [efL; efR];                  % 20484 x 1

% top 1% across both hemispheres
ethresh    = prctile(ef, 99);
efROI_inds = find(ef > ethresh);

% -------------------------
% Find resting scans for this subject
% -------------------------
scanDir = '/scratch/users/apines/data/BRAINS36p';
sessD    = dir(fullfile(scanDir, subj, 'ses-V*'));
sessions = {sessD([sessD.isdir]).name};
if isempty(sessions)
    fprintf('No session dirs for %s, exiting\n', subj);
    return;
end

megatable = {};

for se = 1:length(sessions)
    sesh    = sessions{se};
    seshDir = fullfile(scanDir, subj, sesh);

    fns = dir(fullfile(seshDir, [subj '_' sesh '_L_rs*_TS_10k.func.gii']));
    if isempty(fns)
        fprintf('No resting scans for %s %s, skipping\n', subj, sesh);
        continue;
    end

    for f = 1:length(fns)
        fpath  = fullfile(fns(f).folder, fns(f).name);
        fRpath = strrep(fpath, '_L_rs', '_R_rs');

        scanTok = regexp(fns(f).name, '_rs(\d+)_TS', 'tokens');
        if isempty(scanTok), continue; end
        scanNum = str2double(scanTok{1}{1});

        % load timeseries
        tsL = gifti(fpath).cdata;    % 10242 x T
        tsR = gifti(fRpath).cdata;   % 10242 x T
        ts  = [tsL; tsR];            % 20484 x T

	% efROI_inds x T
	ef_ts = ts(efROI_inds, :);   % N_ef x T
	% sgACC timeseries
	sgACC_ts_L = ts(BA25_IndsL, :);   % N_sgACC_L x T
	sgACC_ts_R = ts(BA25_IndsR, :);   % N_sgACC_R x T

	cc_L = corr(ef_ts', sgACC_ts_L');   % N_ef x N_sgACC_L
	cc_R = corr(ef_ts', sgACC_ts_R');   % N_ef x N_sgACC_R

	% average across all ef-sgACC pairs
	FC_sgACC_L = mean(cc_L(:), 'omitnan');
	FC_sgACC_R = mean(cc_R(:), 'omitnan');

        % mean FC across each sgACC hemisphere
	FC_sgACC = mean([FC_sgACC_L, FC_sgACC_R]);
        megatable = [megatable; {subj, sesh, scanNum, FC_sgACC}];
    end
end

% -------------------------
% Write output
% -------------------------
if isempty(megatable)
    fprintf('No data collected for %s, no CSV written\n', subj);
    return;
end

outDir = '/scratch/users/apines/StimFC_sgACC_perSubj';
if ~exist(outDir, 'dir')
    mkdir(outDir);
end

T = cell2table(megatable, ...
    'VariableNames', {'Subject','Session','Scan','FC_sgACC'});
outfp = fullfile(outDir, [subj '_EfieldSgACC_FC.csv']);
writetable(T, outfp);
fprintf('Saved %s\n', outfp);
