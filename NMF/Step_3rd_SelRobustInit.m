%
% The second step of single brain parcellation, clustering of 50 group atlas to create the final group atlas
%
addpath('/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder/PANDA_1.3.1_64')
addpath(genpath('/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder/Code_mvNMF_l21_ard_v3_release'))

K=17
projectFolder = '/oak/stanford/groups/anishm/Adam/data/SingleParcellationBrains';
% would change the output folder to a new one, just remember the filepath as downstream scripts will need it
resultantFolder = [projectFolder '/RobustInitialization_2S10L_' num2str(K)];
mkdir(resultantFolder);

% if you've never ran this before, it will throw an error saying it can't find the file. It's to remove old instances and avoid confusing downstream scripts
inFile = [resultantFolder '/ParcelInit_List.txt'];
system(['rm ' inFile]);
% list all the files (pilot runs of group consensus
AllFiles = g_ls([projectFolder '/Initialization/*/*comp' num2str(K) '_S1_14118_L_5882*/*.mat']);
for i = 1:length(AllFiles)
  cmd = ['echo ' AllFiles{i} ' >> ' inFile];
  system(cmd);
end

% Parcellate into K networks (K commented out because it is set in iterative loop prior to calling this)
selRobustInit(inFile, K, resultantFolder);
