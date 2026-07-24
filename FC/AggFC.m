% Aggregate FC for all brains FC outputs
parentFP='/scratch/groups/nolanw/BrainsNMFdata/';
files=dir([parentFP '*_FC.csv']);

% Preallocate cell array for tables
allTables = cell(numel(files),1);

for i = 1:numel(files)
    % Read the current csv
    T = readtable(fullfile(parentFP, files(i).name));

    % Keep only if it has 4 variables (columns)
    if width(T) == 4
        allTables{i} = T;
    else
        fprintf('Skipping %s (has %d columns)\n', files(i).name, width(T));
    end
end

% Remove empty entries
allTables = allTables(~cellfun('isempty', allTables));

% Concatenate into one table
combinedT = vertcat(allTables{:});

% Save out to a csv
writetable(combinedT, '~/AllSubjects_FC.csv');
disp('Saved combined table as AllSubjects_FC.csv');
