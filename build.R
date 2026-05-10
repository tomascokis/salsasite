# R/build.R

library(data.table)
library(quarto)

source("header.R")
source.all("R")

# ---- Load dataset ----

dt_pw_raw <- loadMainData("https://docs.google.com/spreadsheets/d/1zEYvNilekcaQIihz37yZJadDJDdiRW7SymGijttQhco/edit?gid=1551769177#gid=1551769177",
                      "Moves")
dt_splashlayout = loadSplashLayout("https://docs.google.com/spreadsheets/d/1zEYvNilekcaQIihz37yZJadDJDdiRW7SymGijttQhco/edit?gid=1551769177#gid=1551769177",
                                   "Move Hier")
#dt_main = checkAndCorrectData(dt_main,dt_splashlayout)

# Merge in which column every entry should be in
dt_pw = merge(dt_pw_raw[!is.na(ID),],
              dt_splashlayout[,],
              by="Topic",all.x = TRUE) |>
            checkAndCorrectData()

dt_pw_layout <- layoutSplashSheet(dt_pw, "Topic","TopicCol",additionUnderStub=FALSE)
renderSplash_group(dt_pw_layout)

# Add SafeID column (sanitized for use in filenames/URLs)
dt_pw$SafeID <- sanitize_id(dt_pw$ID)
dt_pw_layout$SafeID <- sanitize_id(dt_pw_layout$ID)

saveRDS(dt_pw,"site/dt_pw.RDS")
saveRDS(dt_pw_layout,"site/dt_pw_lay.RDS")

# ---- Set site directory ----
site_dir <- "site"
moves_dir <- file.path(site_dir, "moves")
if (!dir.exists(moves_dir)) dir.create(moves_dir, recursive = TRUE)

# ---- Render index page ----
quarto_render(file.path(site_dir, "index.qmd"))

# ---- Import any new progress CSVs ----
message("Checking for progress CSV updates...")
import_progress_csvs(
  csv_folder = "inputs/progress_updates",
  progress_file = "inputs/progress.RDS",
  archive_imported = TRUE
)

# ---- Generate and render progress page ----
message("Generating progress.qmd...")
generate_progress_qmd(
  progress_file = "inputs/progress.RDS",
  layout_file = "site/dt_pw_lay.RDS",
  output_file = "site/progress.qmd"
)
message("Rendering progress page...")
quarto_render(file.path(site_dir, "progress.qmd"))

# ---- Generate progress editor ----
message("Generating progress editor...")
generate_progress_editor(
  progress_file = "inputs/progress.RDS",
  layout_file = "site/dt_pw_lay.RDS",
  output_file = "site/_site/progress_editor.html"
)

# ---- Render move template once ----
message("Rendering move template...")
quarto_render(file.path(site_dir, "move_template.qmd"))
template_path <- file.path(site_dir, "_site", "move_template.html")
template_html <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

# ---- Render move pages using template substitution ----
message("Generating move pages...")
total_moves <- nrow(dt_pw)

for (i in seq_len(total_moves)) {
  id <- dt_pw$ID[i]
  safe_id <- sanitize_id(id)
  
  # Generate content for this move
  parts <- render_move_content(id, dt_pw)
  
  # Substitute placeholders in template
  html <- template_html
  html <- gsub("MOVE_TITLE_PLACEHOLDER", parts$title, html, fixed = TRUE)
  html <- gsub("<!-- MOVE_CSS -->", parts$css, html, fixed = TRUE)
  html <- gsub("<!-- MOVE_CONTENT -->", parts$content, html, fixed = TRUE)
  html <- gsub("<!-- MOVE_JS -->", parts$js, html, fixed = TRUE)
  
  # Fix relative paths for pages in moves/ subdirectory
  html <- gsub('content="./"', 'content="../"', html, fixed = TRUE)
  html <- gsub('src="site_libs/', 'src="../site_libs/', html, fixed = TRUE)
  html <- gsub('href="site_libs/', 'href="../site_libs/', html, fixed = TRUE)
  html <- gsub('href="www/', 'href="../www/', html, fixed = TRUE)
  html <- gsub('href="index.html', 'href="../index.html', html, fixed = TRUE)
  html <- gsub('href="./index.html', 'href="../index.html', html, fixed = TRUE)
  
  # Write the final HTML
  out_path <- file.path(site_dir, "_site", "moves", sprintf("%s.html", safe_id))
  writeLines(html, out_path, useBytes = TRUE)
  
  # Progress indicator every 10 moves
  if (i %% 10 == 0 || i == total_moves) {
    message(sprintf("  %d/%d moves (%d%%)", i, total_moves, round(i / total_moves * 100)))
  }
}

message("✅ Build complete. Open site/_site/index.html in your browser.")
