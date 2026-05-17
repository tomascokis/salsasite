# Functions for rendering splash sheets with progress indicators

#' Get background color for progress indicator
#' 
#' @param value Numeric status value
#' @param dimension One of "prep", "sequ", or "succ"
#' @return Hex color code
get_progress_color <- function(value, dimension = c("prep", "sequ", "succ")) {
  dimension <- match.arg(dimension)
  
  # Handle NA values
  if (is.na(value)) {
    return("#ffffff")
  }
  
  # Color mappings from old gt table styling
  if (dimension == "prep") {
    colors <- c(
      "0" = "#fff5ee",  # seashell1
      "1" = "#FFDEAD",  # navajowhite
      "2" = "#FFD700",  # darkgoldenrod1 (gold)
      "3" = "#FFA07A",  # lightsalmon
      "4" = "#7FFF00"   # chartreuse2
    )
  } else if (dimension == "sequ") {
    colors <- c(
      "0" = "#ffffff",  # white
      "1" = "#EE2C2C",  # red2
      "2" = "#FFD700",  # darkgoldenrod1
      "3" = "#2F4F4F",  # darkslategrey
      "4" = "#7FFF00",  # chartreuse2
      "5" = "#66CD00"   # chartreuse3
    )
  } else if (dimension == "succ") {
    colors <- c(
      "0" = "#ffffff",  # white
      "1" = "#FF4500",  # orangered2
      "2" = "#FFD700",  # darkgoldenrod1
      "3" = "#BCEE68",  # darkolivegreen2
      "4" = "#66CD00"   # chartreuse3
    )
  }
  
  val_str <- as.character(value)
  if (val_str %in% names(colors)) {
    return(colors[[val_str]])
  }
  
  return("#ffffff")  # default to white
}

#' Render a single cell for the progress splash sheet
#' 
#' @param row Single row from progress data
#' @param show_progress Whether to show progress indicator columns
#' @return HTML string for the row
render_progress_splash_row <- function(row, show_progress = TRUE) {
  
  is_title <- !is.na(row$EntryType) && row$EntryType == "Title"
  is_data <- is.na(row$EntryType) || row$EntryType == "Data"
  
  # Determine styling classes
  cell_classes <- "splash-cell"
  if (is_title) {
    cell_classes <- paste(cell_classes, "splash-title")
  }
  
  # Level cell
  level_style <- ""
  if (is_data) {
    level_style <- "background-color: #cfe2f3;"
    if (!is.na(row$Type) && row$Type %in% c("Addition", "Variation", "Stub")) {
      level_style <- "background-color: #e7f4ff;"
    }
  }
  
  level_html <- sprintf(
    '<div class="%s" style="%s text-align: center;">%s</div>',
    cell_classes,
    level_style,
    if (!is.na(row$Level)) row$Level else ""
  )
  
  # Name cell
  name_style <- ""
  name_content <- if (!is.na(row$Name)) row$Name else ""
  name_class <- cell_classes
  
  if (is_data) {
    name_style <- "background-color: #cfe2f3;"
  }
  
  if (is_title) {
    name_class <- paste(name_class, "fw-bold")
  }
  
  if (!is.na(row$Type) && row$Type %in% c("Addition", "Variation")) {
    name_class <- paste(name_class, "fst-italic")
  }
  
  name_html <- sprintf(
    '<div class="%s" style="%s">%s</div>',
    name_class,
    name_style,
    name_content
  )
  
  # Progress indicator cells (if requested)
  progress_html <- ""
  if (show_progress && is_data) {
    prep_color <- get_progress_color(row$StatNum_Prep, "prep")
    sequ_color <- get_progress_color(row$StatNum_Sequ, "sequ")
    succ_color <- get_progress_color(row$StatNum_Succ, "succ")
    
    progress_html <- sprintf(
      '<div class="progress-col" style="background-color: %s;"></div><div class="progress-col" style="background-color: %s;"></div><div class="progress-col" style="background-color: %s;"></div>',
      prep_color, sequ_color, succ_color
    )
  } else if (show_progress) {
    # Empty progress cells for title rows
    progress_html <- '<div class="progress-col"></div><div class="progress-col"></div><div class="progress-col"></div>'
  }
  
  # Wrap row in container
  sprintf('<div class="splash-row">%s</div>', paste0(level_html, name_html, progress_html))
}

#' Render a single column of splash sheet with progress
#' 
#' @param dt_col Data for one column (filtered by layCol)
#' @param show_progress Whether to show progress indicators
#' @return HTML string for the column
render_progress_splash_column <- function(dt_col, show_progress = TRUE) {
  
  rows_html <- sapply(seq_len(nrow(dt_col)), function(i) {
    render_progress_splash_row(dt_col[i, ], show_progress)
  })
  
  col_content <- paste(rows_html, collapse = "\n")
  
  sprintf('<div class="splash-column">\n%s\n</div>', col_content)
}
