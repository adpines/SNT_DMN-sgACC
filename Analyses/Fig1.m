% This is just to plot some example brains for figure 1

% Load dscalar
dscalar = read_cifti('E:/Downloads/GroupSP.dscalar.nii');
full_cdata = dscalar.cdata;

% Load surfaces
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

medial_wall_idx_L = label_L(:,1) + 1;
medial_wall_idx_R = label_R(:,1) + 1;

% Extended colormap: gray for medial wall, then blue gradient
gray_color = [0.5 0.5 0.5];
hex_color = '00a6f9';
rgb_max = [hex2dec(hex_color(1:2)) hex2dec(hex_color(3:4)) hex2dec(hex_color(5:6))] / 255;
cols = [gray_color; 0 0 0; 0 0 0; rgb_max; rgb_max];
stops = [-0.1 0 0.1 0.9 1];
cmap = interp1(stops, cols, linspace(-0.1,1,256), 'pchip');
cmap = max(min(cmap, 1), 0);

medial_color = [0.5 0.5 0.5];

% Extract component 13 (DMN)
frame_L = full_cdata(1:10242, 13);
frame_R = full_cdata(10243:end, 13);

% Map to RGB for left hemisphere
frame_L_norm = frame_L / max(frame_L);
% save max
group_max = max(frame_L);
cmap_idx_L = round((frame_L_norm + 0.1) / 1.1 * 255) + 1;
cmap_idx_L = max(min(cmap_idx_L, 256), 1);
vertex_rgb_L = cmap(cmap_idx_L, :);
vertex_rgb_L(medial_wall_idx_L, :) = repmat(medial_color, length(medial_wall_idx_L), 1);

% Map to RGB for right hemisphere
frame_R_norm = frame_R / max(frame_R);
cmap_idx_R = round((frame_R_norm + 0.1) / 1.1 * 255) + 1;
cmap_idx_R = max(min(cmap_idx_R, 256), 1);
vertex_rgb_R = cmap(cmap_idx_R, :);
vertex_rgb_R(medial_wall_idx_R, :) = repmat(medial_color, length(medial_wall_idx_R), 1);

fig = figure('Color', 'w');

views_config = {
    {vertices_L, faces_L, vertex_rgb_L, [270 0]},
    {vertices_L, faces_L, vertex_rgb_L, [90 0]},
    {vertices_R, faces_R, vertex_rgb_R, [90 0]},
    {vertices_R, faces_R, vertex_rgb_R, [270 0]}
};

for idx = 1:4
    subplot(1, 4, idx);
    
    vertices = views_config{idx}{1};
    faces = views_config{idx}{2};
    vertex_rgb = views_config{idx}{3};
    view_angle = views_config{idx}{4};
    
    trisurf(faces, vertices(:,1), vertices(:,2), vertices(:,3), ...
            'FaceVertexCData', vertex_rgb, ...
            'FaceColor', 'interp', ...
            'EdgeColor', 'none', ...
            'FaceAlpha', 1);
    
    axis equal off vis3d;
    view(view_angle);
    camlight('headlight');
    lighting gouraud;
    material dull;
end
colormap(cmap);
caxis([-0.1 1]);
colorbar('Position', [0.92 0.15 0.02 0.7]);



% Load surfaces once
[vertices_L, faces_L] = read_surf('E:\surf\lh_fs5.inflated');
faces_L = faces_L + 1;

n_verts_L = size(vertices_L, 1);

% Load medial wall
mw_raw_L = read_label('', 'E:\surf\lh_fs5.Medial_wall');
medial_wall_idx_L = mw_raw_L(:,1) + 1;

% Colormap: black to #00a6f9
gray_color = [0.5 0.5 0.5];
hex_color = '00a6f9';
rgb_max = [hex2dec(hex_color(1:2)) hex2dec(hex_color(3:4)) hex2dec(hex_color(5:6))] / 255;
cols = [gray_color; 0 0 0; 0 0 0; rgb_max; rgb_max];
stops = [-0.1 0 0.1 0.9 1];
cmap = interp1(stops, cols, linspace(-0.1,1,256), 'pchip');
cmap = max(min(cmap, 1), 0);

% Subject list
subjects = {
    'E:\Downloads\hard\sub-s005_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s030_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s040_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s033_hard_parcel.dscalar.nii'

};

for s = 1:length(subjects)
    hard_file = subjects{s};
    % Convert to soft parcellation path
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
    frame_L = full_cdata(1:n_verts_L, 13);
    
    % Map to colormap
    frame_norm = frame_L / group_max;
    cmap_idx = round((frame_norm + 0.1) / 1.1 * 255) + 1;
    cmap_idx = max(min(cmap_idx, 256), 1);
    vertex_rgb = cmap(cmap_idx, :);
    
    % Paint medial wall on top (exactly like hard script)
    vertex_rgb(medial_wall_idx_L, :) = repmat(medial_color, length(medial_wall_idx_L), 1);
    
    fig = figure('Color', 'w');
    
    trisurf(faces_L, vertices_L(:,1), vertices_L(:,2), vertices_L(:,3), ...
            'FaceVertexCData', vertex_rgb, ...
            'FaceColor', 'interp', ...
            'EdgeColor', 'none', ...
            'FaceAlpha', 1);
    
    axis equal off vis3d;
    view([90 0]);
    camlight('headlight');
    lighting gouraud;
    material dull;
    colormap(cmap);
    caxis([-0.1 1]);
    colorbar('Position', [0.92 0.15 0.02 0.7]);
    pause(1);
end