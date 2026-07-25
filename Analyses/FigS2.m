% This one actually does more matlab plotting, for figure S2 to show what
% the PFMs actually look like (group and individual level)

addpath(genpath('E:\codelib'))

% Load dscalar
dscalar = read_cifti('E:/Downloads/GroupSP.dscalar.nii');
full_cdata = dscalar.cdata;

% Load surfaces from .gii files
gifti_L = gifti('E:/Downloads/fs5_inflated_L.surf.gii');
gifti_R = gifti('E:/Downloads/fs5_inflated_R.surf.gii');

vertices_L = gifti_L.vertices;
faces_L = gifti_L.faces;
vertices_R = gifti_R.vertices;
faces_R = gifti_R.faces;

% Load medial wall labels
SubjectsFolder = 'E:\surf\';
label_L = read_label('', [SubjectsFolder 'lh_fs5.Medial_wall.label']);
label_R = read_label('', [SubjectsFolder 'rh_fs5.Medial_wall.label']);

% Create boolean masks (add 1 for 1-indexing)
medial_L = false(10242, 1);
medial_R = false(10242, 1);
medial_L(label_L(:,1) + 1) = true;
medial_R(label_R(:,1) + 1) = true;

% Argmax to get hard parcellation
[~, initV_max] = max(full_cdata, [], 2);

% Find all-zero rows (excluded vertices)
all_zero = all(full_cdata == 0, 2);
initV_max(all_zero) = 0;

% Then zero the medial wall
initV_max(medial_L) = 0;
frame_L = initV_max(1:10242);

frame_L = initV_max(1:10242);
frame_R = initV_max(10243:end);

% Pain-in-the-ass colormap
cmap = zeros(18, 3);
cmap(1,:)  = [0.5 0.5 0.5];
cmap(2,:)  = [0.122 0.471 0.706];
cmap(3,:)  = [0.000 0.000 0.000];
cmap(4,:)  = [0.750 0.500 0.780];
cmap(5,:)  = [0.7 0.1 0.15];
cmap(6,:)  = [0.200 0.627 0.173];
cmap(7,:)  = [0.416 0.239 0.604];
cmap(8,:)  = [1.000 0.498 0.000];
cmap(9,:)  = [1 .275 0.635];
cmap(10,:) = [0.4784 0.1765 0.2471];
cmap(11,:) = [1.000 0.980 0.820];
cmap(12,:) = [0.600 0.750 0.550];
cmap(13,:) = [0.100 0.200 0.500];
cmap(14,:) = [0.900 0.050 0.150];
cmap(15,:) = [0.8 0.75 0.300];
cmap(16,:) = [0 0 1];
cmap(17,:) = [1.000 1.000 0.000];
cmap(18,:) = [0.7765 0.5882 0.4196];


% Plot
fig = figure('Color', 'w');

views_config = {
    {vertices_L, faces_L, frame_L, [270 0], 'L Medial'},
    {vertices_L, faces_L, frame_L, [90 0], 'L Lateral'},
    {vertices_R, faces_R, frame_R, [90 0], 'R Medial'},
    {vertices_R, faces_R, frame_R, [270 0], 'R Lateral'}
};

for idx = 1:4
    subplot(2, 2, idx);
    vertices = views_config{idx}{1};
    faces = views_config{idx}{2};
    plot_data = views_config{idx}{3};
    view_angle = views_config{idx}{4};
    title_str = views_config{idx}{5};
    aplot = trisurf(faces, vertices(:,1), vertices(:,2), vertices(:,3), plot_data);
    view(view_angle);
    colormap(cmap);
    caxis([0 17]);
    daspect([1 1 1]);
    axis tight; axis vis3d off;
    material dull;
    set(aplot, 'EdgeColor', 'none');
    alpha(1);
    rotate(aplot, [0 0 1], 180);
    camlight;
    title(title_str);
end
colorbar('Position', [0.92 0.15 0.02 0.7]);


% note participants are matched throughout figures: 4 of these used

participants = {
    'E:\Downloads\hard\sub-s005_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s008_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s014_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s023_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s030_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s033_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s040_hard_parcel.dscalar.nii';
};

for p = 1:length(participants)
    hard_file = participants{p};
    [filepath, name, ~] = fileparts(hard_file);
    sub_id = extractBefore(name, '_hard');
    soft_file = ['E:/Downloads/soft_parcellations/' sub_id '/' sub_id '_SP.dscalar.nii'];
    
    if ~isfile(soft_file)
        disp(['Soft file not found: ' soft_file])
        continue
    end
    
    % Load soft parcellation
    dscalar = read_cifti(soft_file);
    full_cdata = dscalar.cdata;
    
    % Argmax across components
    [~, initV_max] = max(full_cdata, [], 2);
    
    % Find all-zero rows and zero them out
    all_zero = all(full_cdata == 0, 2);
    initV_max(all_zero) = 0;
    
    % Zero medial wall
    initV_max(medial_L) = 0;
    initV_max(10242 + find(medial_R)) = 0;
    
    frame_L = initV_max(1:10242);
    frame_R = initV_max(10243:end);
    
    % Plot
    fig = figure('Color', 'w');
    fig.Name = sub_id;
    
    views_config = {
        {vertices_L, faces_L, frame_L, [270 0], 'L Medial'},
        {vertices_L, faces_L, frame_L, [90 0], 'L Lateral'},
        {vertices_R, faces_R, frame_R, [90 0], 'R Medial'},
        {vertices_R, faces_R, frame_R, [270 0], 'R Lateral'}
    };
    
    for idx = 1:4
        subplot(2, 2, idx);
        vertices = views_config{idx}{1};
        faces = views_config{idx}{2};
        plot_data = views_config{idx}{3};
        view_angle = views_config{idx}{4};
        title_str = views_config{idx}{5};
        
        aplot = trisurf(faces, vertices(:,1), vertices(:,2), vertices(:,3), plot_data);
        view(view_angle);
        colormap(cmap);
        caxis([0 17]);
        daspect([1 1 1]);
        axis tight; axis vis3d off;
        material dull;
        set(aplot, 'EdgeColor', 'none');
        alpha(1);
        rotate(aplot, [0 0 1], 180);
        camlight;
        title(title_str);
    end
    % colorbars redundant
    %colorbar('Position', [0.92 0.15 0.02 0.7]);
    pause(1);
end