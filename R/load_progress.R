# Functions for loading and processing progress tracking data

#' Load progress data from RDS file
#' 
#' @param file Path to progress RDS file
#' @return data.table with Date, ID, Preperation, Sequencing, Success, and StatNum_* columns
load_progress_data <- function(file = "inputs/progress.RDS") {
  dt_progress <- readRDS(file)
  
  # Validate required columns
  required_cols <- c("Date", "ID", "Preperation", "Sequencing", "Success",
                     "StatNum_Prep", "StatNum_Sequ", "StatNum_Succ")
  missing_cols <- setdiff(required_cols, names(dt_progress))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Ensure Date is proper Date class
  if (!inherits(dt_progress$Date, "Date")) {
    dt_progress[, Date := as.Date(Date)]
  }
  
  # Sort by date and ID
  dt_progress <- dt_progress[order(Date, ID)]
  
  return(dt_progress)
}

#' Merge progress data into splash layout
#' 
#' @param dt_progress Progress data from load_progress_data()
#' @param dt_splash Splash layout data (from layoutSplashSheet)
#' @param date_filter Optional date to filter to single snapshot
#' @param exclude_stubs Whether to exclude moves with 'Stub' tag (default TRUE)
#' @return data.table with splash layout merged with progress data
merge_progress_into_splash <- function(dt_progress, dt_splash, date_filter = NULL, exclude_stubs = TRUE) {
  
  # Filter to specific date if requested
  if (!is.null(date_filter)) {
    dt_progress <- dt_progress[Date == date_filter]
  }
  
  # Get unique dates
  dates <- dt_progress[!is.na(Date), unique(Date)]
  
  # Build merged data for each date
  lByDate <- list()
  
  for (i in seq_along(dates)) {
    date <- dates[i]
    
    # Filter splash layout - exclude stubs if requested
    if (exclude_stubs) {
      dt_splash_filtered <- dt_splash[(Valid == TRUE | EntryType == "Title") & 
                                       (is.na(Tags) | !grepl("Stub", Tags, ignore.case = TRUE)), ]
    } else {
      dt_splash_filtered <- dt_splash[Valid == TRUE | EntryType == "Title", ]
    }
    
    # Merge progress for this date into splash layout
    dt <- merge(
      dt_splash_filtered,
      dt_progress[Date == date, ],
      by = "ID",
      all.x = TRUE
    )
    
    dt[, Date := date]
    lByDate[[i]] <- dt
  }
  
  # Combine all dates
  dt_result <- rbindlist(lByDate)
  dt_result <- dt_result[order(Date, layCol, LayoutOrder)]
  
  return(dt_result)
}

#' Get summary statistics for a progress snapshot
#' 
#' @param dt_progress_snapshot Progress data filtered to single date
#' @return List with summary statistics
get_progress_summary <- function(dt_progress_snapshot) {
  
  # Filter to actual data rows (not title rows)
  dt_data <- dt_progress_snapshot[EntryType == "Data" | is.na(EntryType)]
  
  # Count by preparation status
  prep_counts <- dt_data[, .N, by = Preperation][order(-N)]
  
  # Count by sequencing status
  seq_counts <- dt_data[, .N, by = Sequencing][order(-N)]
  
  # Count by success status
  succ_counts <- dt_data[, .N, by = Success][order(-N)]
  
  # Overall statistics
  total_moves <- dt_data[, .N]
  prepped <- dt_data[StatNum_Prep >= 4, .N]
  good_sequencing <- dt_data[StatNum_Sequ >= 4, .N]
  good_success <- dt_data[StatNum_Succ >= 3, .N]
  
  list(
    date = unique(dt_progress_snapshot$Date),
    total_moves = total_moves,
    prepped = prepped,
    good_sequencing = good_sequencing,
    good_success = good_success,
    prep_counts = prep_counts,
    seq_counts = seq_counts,
    succ_counts = succ_counts
  )
}

#' Import progress CSVs from a folder into progress.RDS
#' 
#' Reads all CSV files from a folder (typically exported from the progress editor),
#' and merges them into the existing progress.RDS file. CSVs should have columns:
#' Date, ID, Preperation, Sequencing, Success, StatNum_Prep, StatNum_Sequ, StatNum_Succ
#' 
#' @param csv_folder Path to folder containing CSV files to import
#' @param progress_file Path to progress RDS file to update
#' @param archive_imported Whether to move imported CSVs to an "imported" subfolder
#' @return Updated progress data.table (also saved to progress_file)
import_progress_csvs <- function(
  csv_folder = "inputs/progress_updates",
  progress_file = "inputs/progress.RDS",
  archive_imported = TRUE
) {
  library(data.table)
  
  # Check if folder exists
  if (!dir.exists(csv_folder)) {
    message("No CSV folder found at: ", csv_folder)
    message("Creating folder for future use...")
    dir.create(csv_folder, recursive = TRUE)
    return(invisible(NULL))
  }
  
  # Find all CSV files
  csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    message("No CSV files found in: ", csv_folder)
    return(invisible(NULL))
  }
  
  message(sprintf("Found %d CSV file(s) to import", length(csv_files)))
  
  # Load existing progress data
  if (file.exists(progress_file)) {
    dt_progress <- load_progress_data(progress_file)
    message(sprintf("Loaded existing progress with %d records across %d dates",
                    nrow(dt_progress),
                    dt_progress[, uniqueN(Date)]))
  } else {
    message("No existing progress file, creating new one")
    dt_progress <- data.table()
  }
  
  # Process each CSV
  imported_count <- 0
  for (csv_file in csv_files) {
    message(sprintf("  Processing: %s", basename(csv_file)))
    
    tryCatch({
      # Read CSV
      dt_new <- fread(csv_file)
      
      # Validate columns
      required_cols <- c("Date", "ID", "Preperation", "Sequencing", "Success",
                        "StatNum_Prep", "StatNum_Sequ", "StatNum_Succ")
      missing_cols <- setdiff(required_cols, names(dt_new))
      if (length(missing_cols) > 0) {
        warning(sprintf("Skipping %s - missing columns: %s",
                       basename(csv_file),
                       paste(missing_cols, collapse = ", ")))
        next
      }
      
      # Parse date
      dt_new[, Date := as.Date(Date)]
      
      # Ensure numeric columns
      dt_new[, StatNum_Prep := as.integer(StatNum_Prep)]
      dt_new[, StatNum_Sequ := as.integer(StatNum_Sequ)]
      dt_new[, StatNum_Succ := as.integer(StatNum_Succ)]
      
      # Get date from this file
      new_date <- dt_new[, unique(Date)]
      if (length(new_date) != 1) {
        warning(sprintf("Skipping %s - contains multiple dates", basename(csv_file)))
        next
      }
      
      message(sprintf("    Date: %s, Moves: %d", new_date, nrow(dt_new)))
      
      # Check if this date already exists
      if (nrow(dt_progress) > 0 && new_date %in% dt_progress$Date) {
        # Update existing date
        old_count <- dt_progress[Date == new_date, .N]
        dt_progress <- dt_progress[Date != new_date]  # Remove old
        dt_progress <- rbindlist(list(dt_progress, dt_new), fill = TRUE)
        message(sprintf("    Updated existing date (replaced %d records)", old_count))
      } else {
        # Add new date
        dt_progress <- rbindlist(list(dt_progress, dt_new), fill = TRUE)
        message("    Added as new date")
      }
      
      imported_count <- imported_count + 1
      
      # Archive the imported file
      if (archive_imported) {
        archive_dir <- file.path(csv_folder, "imported")
        if (!dir.exists(archive_dir)) dir.create(archive_dir)
        archive_path <- file.path(archive_dir, basename(csv_file))
        file.rename(csv_file, archive_path)
        message(sprintf("    Archived to: imported/%s", basename(csv_file)))
      }
      
    }, error = function(e) {
      warning(sprintf("Error processing %s: %s", basename(csv_file), e$message))
    })
  }
  
  # Sort and save
  if (imported_count > 0) {
    dt_progress <- dt_progress[order(Date, ID)]
    
    # Backup existing file
    if (file.exists(progress_file)) {
      backup_name <- sprintf("%s.backup_%s",
                            progress_file,
                            format(Sys.time(), "%Y%m%d_%H%M%S"))
      file.copy(progress_file, backup_name)
      message(sprintf("Backed up existing file to: %s", basename(backup_name)))
    }
    
    # Save updated progress
    saveRDS(dt_progress, progress_file)
    message(sprintf("\n✅ Imported %d file(s). Progress now has %d records across %d dates.",
                   imported_count,
                   nrow(dt_progress),
                   dt_progress[, uniqueN(Date)]))
  } else {
    message("No files were successfully imported")
  }
  
  return(invisible(dt_progress))
}
