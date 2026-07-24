
% Based on the group atlas, creating each subject's individual specific atlas
%

ToolFolder='/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder';
addpath(genpath(ToolFolder));
K=17
ProjectFolder = '/oak/stanford/groups/anishm/Adam/data/SingleParcellationBrains';
ResultantFolder = [ProjectFolder '/PFMs_rerun_k' num2str(K)];
mkdir(ResultantFolder);

PrepDataFile = [ProjectFolder '/CreatePrepData.mat'];
resId = 'IndividualParcel_Final';
initName = [ProjectFolder '/RobustInitialization_2S10L_' num2str(K) '/init.mat'];
% set parameters
alphaS21 = 3;
alphaL = 30;
vxI = 1;
spaR = 1;
ard = 0;
iterNum = 100;
eta = 0;
calcGrp = 0;
parforOn = 0;

surfaceFolder='/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf';
% for surface data
surfML = [surfaceFolder '/lh_fs5.Medial_wall.label'];
surfMR = [surfaceFolder '/rh_fs5.Medial_wall.label'];
% folder with fs5 mgh's
RawDataFolder = '/scratch/groups/nolanw/BrainsNMFdata';
LeftCell = g_ls([RawDataFolder '/*_L.mgh']);
RightCell = g_ls([RawDataFolder '/*_R.mgh']);
% get subject IDs
subject_ids = cell(length(LeftCell), 1);
for i = 1:length(LeftCell)
    tokens = split(LeftCell{i}, '/');
    fname = tokens{end};
    parts = split(fname, '_');  % assumes format: sub-sXXX_ses-YY_scanZZ_L.mgh
    subject_ids{i} = parts{1};  % 'sub-sXXX'
end
subjects = unique(subject_ids);
% Parcellate for each subject separately
for i = 1:length(subjects)
    subject_id = subjects{i};
    % scans for this subj
    LeftFiles = g_ls(fullfile(RawDataFolder, [subject_id '*_L.mgh']));
    RightFiles = g_ls(fullfile(RawDataFolder, [subject_id '*_R.mgh'])) 
    left_matches = sort(LeftFiles);
    right_matches = sort(RightFiles);
    % make output folder
    ResultantFolder_I = [ResultantFolder '/Sub_' subject_id];
    ResultantFile = [ResultantFolder_I '/IndividualParcel_Final_sbj1_comp' num2str(K) '_alphaS21_1_alphaL10_vxInfo1_ard0_eta0/final_UV.mat'];
    mkdir(ResultantFolder_I);
    % concatenate all L's and R's
    for s = 1:2
    	 side = ["L", "R"];
    	 if side(s) == "L"
    	        matches = left_matches;
    	 else
        	matches = right_matches;
   	 end
	    all_data = [];
	    for f = 1:length(matches)
		    mri = MRIread(matches{f});
		    all_data = cat(4, all_data, mri.vol);  % concatenate along time
	    end
	    fn = strjoin(['_' side(s) '_all.mgh'],'');
	    concat_file = strjoin([ResultantFolder_I fn],'');
	    fn = ['_' char(side(s)) '_all.mgh'];
	    concat_file = fullfile(ResultantFolder_I, [subject_id fn]);
	    % Make sure concat_file is a proper char vector (not string object!)
	    if isstring(concat_file)
	        concat_file = char(concat_file);
	    end

	    % Ensure MRIwrite knows what format you're writing
	    mri_template.vol = all_data;
	    mri_template.volsize = size(all_data);
	    mri_template.nframes = size(all_data, 4);
	    mri_template.dims = mri_template.volsize;
	    mri_template.fspec = concat_file;
	    mri_template.MRItype = 'mgh';

	    % Write to disk
	    MRIwrite(mri_template, concat_file);
	    if side(s) == "L"
   	    	final_L = concat_file;
   	    else
        	final_R = concat_file;
    	    end
	    % save out constituent file list, useful sanity check for later
	    listfile = fullfile(ResultantFolder_I, [subject_id '_' char(side(s)) '_constituent_files.txt']);
	    fid = fopen(listfile, 'w');
	    for f = 1:length(matches)
	        fprintf(fid, '%s\n', matches{f});
	    end
	    fclose(fid);
     end

    %if ~exist(ResultantFile, 'file');
        IDMatFile = [ResultantFolder_I '/ID.mat'];
	ID=subject_id;
        save(IDMatFile, 'ID');
	
        sbjListFile = [ResultantFolder_I '/sbjListAllFile_' num2str(i) '.txt'];
        system(['rm ' sbjListFile]);

	nScans = min(length(left_matches), length(right_matches));
	fid = fopen(sbjListFile, 'w');
	fprintf(fid, '%s\n', final_L);  % path to concatenated left hemisphere
	fprintf(fid, '%s\n', final_R);  % path to concatenated right hemisphere
	fclose(fid);

	% template scripts
	ScriptPathTemplate=[ResultantFolder_I '/tmp.sh'];
	% port in template script: i've copied this .sh to the current directory as well
	Script_cp = ['cp /home/users/apines/sbatchTemplate.sh ' ScriptPathTemplate];
	system(Script_cp)

        save([ResultantFolder_I '/Configuration.mat'], 'sbjListFile', 'surfML', 'surfMR', 'PrepDataFile', 'ResultantFolder_I', 'resId', 'initName', 'K', 'alphaS21', 'alphaL', 'vxI', 'spaR', 'ard', 'eta', 'iterNum', 'calcGrp', 'parforOn');
        ScriptPath = [ResultantFolder_I '/tmpCmd'];
	logpath = [ResultantFolder_I '/ParcelFinal.log']
        cmd = ['matlab -nodisplay -r ' ...
	  '"addpath(genpath(''/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder'')),' ...
          'load(''' ResultantFolder_I '/Configuration.mat''),' ...
          'deployFuncMvnmfL21p1_func_surf_fs(sbjListFile,surfML,surfMR,' ...
          'PrepDataFile,ResultantFolder_I,resId,initName,K,alphaS21,' ...
          'alphaL,vxI,spaR,ard,eta,iterNum,calcGrp,parforOn),exit(1)">"'...
	   ResultantFolder_I '/ParcelFinal.log" 2>&1'];
        fid = fopen(strcat(ScriptPath, '.m'), 'w');
        fprintf(fid, cmd);
	command=['cat ' ScriptPath '.m >>' ScriptPathTemplate];
	system(command);
	system(['sbatch ' ScriptPathTemplate]);
	pause(3)
end

