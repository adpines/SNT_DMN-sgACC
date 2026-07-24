library(dplyr)
library(readr)

# This ends up being surprisingly computationally intensive, for Sherlock (Stanford), try an sdev -m 16G call to get the memory needed to run this with Rscript

# Root directory containing fmriprep output
root_dir <- "/oak/stanford/groups/nolanw/BRAINS_August2025_DataFreeze/fmriprep"

subject_dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
session_dirs <- unlist(lapply(subject_dirs, function(s) list.dirs(s, recursive = FALSE)))
func_dirs <- file.path(session_dirs, "func")

# Only include if the directory actually exists
func_dirs <- func_dirs[dir.exists(func_dirs)]

# Now find all relevant TSVs directly within those func folders
tsv_files <- unlist(lapply(func_dirs, function(f) {
  list.files(f, pattern = "_desc-confounds_timeseries.tsv$", full.names = TRUE)
}))

# Initialize result list
fd_summary_list <- list()

for (file in tsv_files) {
  # Extract components from file path
  parts <- strsplit(file, "/")[[1]]
  subj <- parts[grep("^sub-", parts)]
  ses <- parts[grep("^ses-", parts)]
  filename <- basename(file)
  scan <- sub("^.*_task-|_desc-confounds_timeseries.tsv$", "", filename)

  # Read and process file
  df <- tryCatch(read_tsv(file, show_col_types = FALSE), error = function(e) return(NULL))
  if (is.null(df) || !("framewise_displacement" %in% names(df))) next

  fd_vals <- suppressWarnings(as.numeric(df$framewise_displacement))
  fd_vals <- fd_vals[!is.na(fd_vals)]

  fd_summary_list[[length(fd_summary_list) + 1]] <- data.frame(
    Subject = subj,
    Session = ses,
    Scan = scan,
    Mean_FD = mean(fd_vals),
    Num_Under_0.3 = sum(fd_vals < 0.3)
  )
}

# Combine and save
fd_summary_df <- bind_rows(fd_summary_list)

write.csv(fd_summary_df, "fd_summary_per_scan.csv", row.names = FALSE)

