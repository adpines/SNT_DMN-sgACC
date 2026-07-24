% this just makes a little 'gNb' file, which provides a bit of spatial info to NMF about the surface it's working on (which vertices are spatial neighbors)

% add matlab path for used functions
addpath(genpath('/oak/stanford/groups/anishm/Adam/scripts/NMF/scripts/code_nmf_cifti/tool_folder'));
% surface directory
surfaceFolder='/oak/stanford/groups/anishm/Adam/TemplateBrains/humans/surf';
% load in inflated surfaces
surfL = [surfaceFolder '/lh_fs5.inflated'];
surfR = [surfaceFolder '/rh_fs5.inflated'];
ProjectFolder = '/oak/stanford/groups/anishm/Adam/data/SingleParcellationBrains';
mkdir(ProjectFolder);

% replacing these with 0-exclude masks (full include) to retain ROIs
surfML = [surfaceFolder '/lh_fs5.Medial_wall.label'];
surfMR = [surfaceFolder '/rh_fs5.Medial_wall.label'];

[surfStru, surfMask] = getFsSurf(surfL, surfR, surfML, surfMR);

gNb = createPrepData('surface', surfStru, 1, surfMask);

% save gNb into file for later use
prepDataName = [ProjectFolder '/CreatePrepData.mat'];
save(prepDataName, 'gNb');
