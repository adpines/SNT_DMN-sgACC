% This gives some more in the weeds illustrations of the ROIs for figure 2
% add paths
addpath(genpath('E:\codelib'))

% load in surfaces
surfL = 'E:\surf\lh_fs5.inflated';
medial_wall_label = 'E:\surf\lh_fs5.Medial_wall';

% load in BAs
ba_label_file = 'E:\Downloads\Human.Brodmann09.32k_fs5_LR.dlabel.nii'; 
ba_index      = 83;
dmn_number = 13; 

% load in some individCons
participants = {
    'E:\Downloads\hard\sub-s005_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s008_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s014_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s023_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s030_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s033_hard_parcel.dscalar.nii';
    'E:\Downloads\hard\sub-s040_hard_parcel.dscalar.nii';
};

% Colors
bg_color     = [0.75, 0.75, 0.75];  % gray  = background
dmn_color    = [0.1608 0.2902 0.3569];  % DMN parcel
ba_color     = [0.9961 0.4980 0.1804];  % Brodmann Area
medial_color = [0,  0,  0 ];  % white = medial wall



% =========================================================================
% LOAD SURFACE
% =========================================================================

[vx_l, faces_l] = read_surf(surfL);
faces_l = faces_l + 1;  % 0-indexed -> 1-indexed

n_verts = size(vx_l, 1);  % should be 10242 for fs5

% =========================================================================
% LOAD MEDIAL WALL
% =========================================================================

mw_raw = read_label('', medial_wall_label);
medial_wall_idx = mw_raw(:,1) + 1;  % 0-indexed -> 1-indexed

% =========================================================================
% LOAD BRODMANN AREA
% =========================================================================

ba_nii  = niftiread(ba_label_file);
ba_data = double(ba_nii(:));

ba_data = ba_data(1:n_verts);

ba_idx = find(ba_data == ba_index);
fprintf('BA index %d: %d vertices found\n', ba_index, length(ba_idx));




% =========================================================================
% LOOP OVER PARTICIPANTS
% =========================================================================

for p = 1:length(participants)
    gii_file = participants{p};
    fprintf('\nParticipant %d: %s\n', p, gii_file);
    % --- Load dscalar parcellation ---
    dmn_nii  = niftiread(gii_file);
    dmn_data = double(dmn_nii(:));
    % just left hemisphere for now
    dmn_data = dmn_data(1:n_verts);
    dmn_idx = find(dmn_data == dmn_number);
    fprintf('  DMN parcel %d: %d vertices found (total verts in file: %d)\n', ...
            dmn_number, length(dmn_idx), length(dmn_data));
    % --- Build per-vertex RGB ---
    vertex_rgb = repmat(bg_color, n_verts, 1);  % n_verts x 3
    % Paint DMN (overwritten by BA where they overlap)
    vertex_rgb(dmn_idx, :) = repmat(dmn_color, length(dmn_idx), 1);
    % Paint BA on top of DMN
    vertex_rgb(ba_idx, :) = repmat(ba_color, length(ba_idx), 1);
    % Paint medial wall on top of everything
    vertex_rgb(medial_wall_idx, :) = repmat(medial_color, length(medial_wall_idx), 1);
    % --- Plot ---
    figure('Color', 'w', 'Position', [100 100 900 800]);
    trisurf(faces_l, vx_l(:,1), vx_l(:,2), vx_l(:,3), ...
            'FaceVertexCData', vertex_rgb, ...
            'FaceColor', 'interp', ...
            'EdgeColor', 'none', ...
            'FaceAlpha', 1);
    axis equal off vis3d;
    hold on
    view([90 0]);       % medial view: flip from 240 (lateral) to 60
    camlight('headlight');  % light from right illuminates the medial face
    lighting gouraud;
    material dull;
    % save and close
    out_file = sprintf('E:\\Downloads\\hard\\fig2_sub%02d_DMN%d_BA%d.png', p, dmn_number, ba_index);
    print(gcf, out_file, '-dpng', '-r300');  % 300 dpi
    close(gcf);


end