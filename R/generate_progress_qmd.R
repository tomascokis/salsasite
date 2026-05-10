# Generate progress.qmd programmatically
# This creates a static .qmd file with all date tabs that Quarto can render

#' Generate the complete progress.qmd file
#' 
#' @param progress_file Path to progress RDS file
#' @param layout_file Path to layout RDS file  
#' @param output_file Path to write the generated .qmd file
#' @return Invisible NULL, writes file as side effect
generate_progress_qmd <- function(
  progress_file = "inputs/progress.RDS",
  layout_file = "site/dt_pw_lay.RDS",
  output_file = "site/progress.qmd"
) {
  
  # Load data to get dates and column count
  dt_progress <- readRDS(progress_file)
  dt_layout <- readRDS(layout_file)
  
  # Get unique dates (most recent first)
  dates <- sort(unique(dt_progress$Date), decreasing = TRUE)
  
  # Get number of columns
  num_cols <- max(dt_layout$layCol, na.rm = TRUE)
  
  message(sprintf("Generating progress.qmd with %d date tabs, %d columns each", 
                  length(dates), num_cols))
  
  # Build the file content
  lines <- c()
  
  # ---- YAML Header ----
  lines <- c(lines, '---
title: "Progress Tracking"
format:
  html:
    toc: false
    grid:
      sidebar-width: 10px
      body-width: 2000px
    html-table-processing: none
    page-layout: full
---')
  
  # ---- CSS Styles ----
  lines <- c(lines, '
```{=html}
<style>
  a, a:hover, a:visited, a:active {
  color: inherit;
  text-decoration: none;
  }
  
  #title-block-header {
  display: none !important;
  }
  
  #quarto-document-content{
    margin-top: 0px;
    padding-top: 10px;
    min-width: 2000;
  }
  
  h1.title {
    display: none;
  }

  thead {
    display: none;
  }
  
  table.gt_table {
    margin-top: 0px;
  }
  
  .progress-summary {
    margin: 0;
    padding: 4px 10px;
    background-color: #f8f9fa;
    border-radius: 4px;
    font-size: 12px;
  }
  
  .progress-summary h4 {
    display: none;
  }
  
  .progress-summary p {
    margin: 0;
  }
  
  .panel-tabset {
    margin-top: 0;
  }
  
  .tab-content {
    padding-top: 0;
    padding-left: 10px;
    padding-right: 10px;
  }
  
  .tab-content > div[style*="display: flex"] {
    margin-top: 0;
    gap: 2px;
  }
  
  .tab-content > div[style*="display: flex"] > div {
    margin: 0;
    padding: 0;
  }
  
  .tab-pane {
    padding-top: 5px;
  }
  
  .nav-tabs .nav-link {
    padding: 0.3rem 0.8rem;
    font-size: 13px;
  }
</style>
```')
  
  # ---- Setup Chunk ----
  lines <- c(lines, '
```{r setup, include=FALSE}
library(data.table)
source("../header.R")
source.all("../R")

# Load data
dt_pw <- readRDS("dt_pw.RDS") |> as.data.table()
dt_progress_raw <- load_progress_data("../inputs/progress.RDS")
dt_pw_layout <- readRDS("dt_pw_lay.RDS") |> as.data.table()

# Merge progress into splash layout for all dates
dt_progress_full <- merge_progress_into_splash(dt_progress_raw, dt_pw_layout)

# Add links to move names
dt_progress_full[, DetailURL := sprintf("moves/%s.html", SafeID)]
dt_progress_full[, MoveN := .I]
for(i in dt_progress_full[EntryType=="Data", MoveN]) {
  dt_progress_full[i, Name := gt::html(sprintf(\'<a href="%s">%s</a>\', DetailURL, Name))]
}
```')
  
  # ---- Panel Tabset Start ----
  lines <- c(lines, '', '::: {.panel-tabset}', '')
  
  # ---- Generate Each Date Tab ----
  for (i in seq_along(dates)) {
    date <- dates[i]
    date_str <- format(as.Date(date), "%Y-%m-%d")
    tab_label <- format(as.Date(date), "%b %Y")
    
    # Tab header
    lines <- c(lines, sprintf('## %s', tab_label), '')
    
    # Summary chunk
    lines <- c(lines, sprintf('```{r, echo=FALSE, results=\'asis\'}
dt_snapshot <- dt_progress_full[Date == as.Date("%s")]
summary <- get_progress_summary(dt_snapshot)

cat(sprintf(\'
<div class="progress-summary">
  <strong>Total:</strong> %%d moves | 
  <strong>Prepped:</strong> %%d (%%.0f%%%%) | 
  <strong>Good Sequencing:</strong> %%d (%%.0f%%%%) | 
  <strong>Good Success:</strong> %%d (%%.0f%%%%)
</div>
\',
  summary$total_moves,
  summary$prepped, 100 * summary$prepped / summary$total_moves,
  summary$good_sequencing, 100 * summary$good_sequencing / summary$total_moves,
  summary$good_success, 100 * summary$good_success / summary$total_moves
))
```', date_str))
    
    # Flex container start
    lines <- c(lines, '', ':::: {style="display: flex; gap: 2px;"}', '')
    
    # Generate each column
    for (col in 1:num_cols) {
      lines <- c(lines, '::: {}')
      lines <- c(lines, sprintf('```{r,echo=F,results=\'markup\'}
renderSplash_group_progress(dt_progress_full[Date == as.Date("%s") & layCol==%d,])
```', date_str, col))
      lines <- c(lines, ':::', '')
    }
    
    # Flex container end
    lines <- c(lines, '::::', '')
  }
  
  # ---- Panel Tabset End ----
  lines <- c(lines, ':::', '')
  
  # ---- Legend ----
  lines <- c(lines, '<br>

```{=html}
<div style="margin: 20px 0; padding: 15px; background-color: #f8f9fa; border-radius: 5px;">
<h4 style="margin-top: 0;">Legend</h4>

<p><strong>Preparation:</strong>
<span style="background-color: #fff5ee; padding: 2px 8px; margin: 0 4px; border: 1px solid #ddd;">Off the cards</span>
<span style="background-color: #FFDEAD; padding: 2px 8px; margin: 0 4px;">Needs guidance</span>
<span style="background-color: #FFD700; padding: 2px 8px; margin: 0 4px;">Not prepped</span>
<span style="background-color: #FFA07A; padding: 2px 8px; margin: 0 4px;">Shaky</span>
<span style="background-color: #7FFF00; padding: 2px 8px; margin: 0 4px;">Prepped</span>
</p>

<p><strong>Sequencing:</strong>
<span style="background-color: #ffffff; border: 1px solid #ddd; padding: 2px 8px; margin: 0 4px;">Not needed</span>
<span style="background-color: #EE2C2C; padding: 2px 8px; margin: 0 4px; color: white;">Unused</span>
<span style="background-color: #FFD700; padding: 2px 8px; margin: 0 4px;">Underused</span>
<span style="background-color: #2F4F4F; padding: 2px 8px; margin: 0 4px; color: white;">Overused</span>
<span style="background-color: #7FFF00; padding: 2px 8px; margin: 0 4px;">Good</span>
<span style="background-color: #66CD00; padding: 2px 8px; margin: 0 4px;">Great</span>
</p>

<p><strong>Success:</strong>
<span style="background-color: #ffffff; border: 1px solid #ddd; padding: 2px 8px; margin: 0 4px;">N/A</span>
<span style="background-color: #FF4500; padding: 2px 8px; margin: 0 4px; color: white;">Failure</span>
<span style="background-color: #FFD700; padding: 2px 8px; margin: 0 4px;">50-50</span>
<span style="background-color: #BCEE68; padding: 2px 8px; margin: 0 4px;">Nearly there</span>
<span style="background-color: #66CD00; padding: 2px 8px; margin: 0 4px;">Great</span>
</p>
</div>
```')
  
  # Write the file
  writeLines(lines, output_file)
  message(sprintf("✅ Generated %s (%d lines)", output_file, length(lines)))
  
  invisible(NULL)
}
