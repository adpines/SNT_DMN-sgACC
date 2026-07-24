% This is the most computationally intensive portion of NMF. It concatenates a bunch of scans over time to run a network decomposition across individuals rather than within them
% After repeating this process many times, the winning solution will be selected as the group consensus decomposition, and used to intialize individualized solutions

% Each time resample 100 subjects, and repeat 50 times
ParcellationFolder = '/oak/stanford/groups/anishm/Adam/data/SingleParcellationBrains';
% add tools
ToolFolder='/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder';
addpath(genpath(ToolFolder));
% make initialization folder
InitializationFolder = [ParcellationFolder '/Initialization'];
mkdir(InitializationFolder);
mkdir([InitializationFolder '/Input']);
ScansQuantity = 400; % resampling 400 scans
RawDataFolder = '/scratch/groups/nolanw/BrainsNMFdata';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
LeftCell = g_ls([RawDataFolder '/*L.mgh']);
RightCell = g_ls([RawDataFolder '/*R.mgh']);
prepDataFile = [ParcellationFolder '/CreatePrepData.mat'];

% surface directory
surfaceFolder='/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf';
% load in inflated surfaces
surfL = [surfaceFolder '/lh_fs5.inflated'];
surfR = [surfaceFolder '/rh_fs5.inflated'];
% replacing these with 0-exclude masks (full include) to retain ROIs
surfML = [surfaceFolder '/lh_fs5.Medial_wall.label'];
surfMR = [surfaceFolder '/rh_fs5.Medial_wall.label'];
% uncomment to implement without SNR mask.
%surfMask.l=ones(length(surfMask.l),1);
%surfMask.r=ones(length(surfMask.r),1);

spaR = 1;
vxI = 1;
ard = 0;
iterNum = 3000;
tNum = 300; % number of time points
alpha = 2;
beta = 10;
resId = 'Initialization';
K = 17; % numer of networks


% calc penalty terms here for ResFile_path search

% nM seems to be 12 for this SNR mask
nM=12;
numUsed=ScansQuantity;

pL = round((beta*tNum*numUsed)/(K*nM));
pS = round((alpha*tNum*numUsed)/(K));

% Repeat 50 times
for i = 1:50
  i
  ResultantFile_Path = [InitializationFolder '/InitializationRes_' num2str(i) '/Initialization_num400_comp' num2str(K) '_S1_' num2str(pS) '_L_' num2str(pL) '_spaR_1_vxInfo_1_ard_0/init.mat'];
  if ~exist(ResultantFile_Path, 'file')
    SubjectsIDs = randperm(length(LeftCell), ScansQuantity);
    sbjListFile = [InitializationFolder '/Input/sbjListFile_' num2str(i) '.txt'];
    % remove old subject list file if it exists
    system(['rm ' sbjListFile]);
    % create subject list file
    for j = 1:length(SubjectsIDs)
      cmd = ['echo ' LeftCell{SubjectsIDs(j)} ' >> ' sbjListFile];
      system(cmd);
      cmd = ['echo ' RightCell{SubjectsIDs(j)} ' >> ' sbjListFile];
      system(cmd);
    end
    outDir = [InitializationFolder '/InitializationRes_' num2str(i)];
    ScriptPathTemplate=[InitializationFolder '/tmp' num2str(i) '.sh'];
    Script_cp = ['cp /home/users/apines/sbatchTemplate.sh ' ScriptPathTemplate];
    system(Script_cp)
    ScriptPath = [InitializationFolder '/temp_' num2str(K) '_' num2str(i) '.sh'];
     
    save([InitializationFolder '/Configuration_' num2str(i) '.mat'], 'sbjListFile', 'surfL', 'surfR', 'surfML', 'surfMR', 'prepDataFile', 'outDir', ...
          'spaR', 'vxI', 'ard', 'iterNum', 'K', 'tNum', 'alpha', 'beta', 'resId');
    cmd = ['matlab -nosplash -nodesktop -r ' ...
          '"addpath(genpath(''' ToolFolder ''')),load(''' ...
          InitializationFolder '/Configuration_' num2str(i) '.mat''),deployFuncInit_surf_fs(sbjListFile, surfL, surfR, ' ...
          'surfML, surfMR, prepDataFile, outDir, spaR, vxI, ard, iterNum, K, tNum, alpha, beta, resId),exit(1)">"' ...
          InitializationFolder '/ParcelInit' num2str(i) '.log" 2>&1'];
    fid = fopen(ScriptPath, 'w');
    fprintf(fid, cmd);
    % copy matlab command into sbatch template
    command=['cat ' ScriptPath '>>' ScriptPathTemplate];
    system(command);
    system(['sbatch ' ScriptPathTemplate]);
  	pause(1)
	end
end

