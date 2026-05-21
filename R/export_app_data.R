#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

output_dir <- "migration-data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

moves <- as.data.table(readRDS("data/dt_pw.RDS"))
layout <- as.data.table(readRDS("data/dt_pw_lay.RDS"))
progress <- as.data.table(readRDS("inputs/progress.RDS"))

trim_or_null <- function(x) {
  if (is.null(x)) return(NULL)
  if (length(x) == 0) return(NULL)
  if (is.character(x)) {
    x <- trimws(x)
    x[x %in% c("", "NA", "FALSE")] <- NA_character_
  }
  x
}

compact_chr <- function(x) {
  x <- trim_or_null(x)
  x <- x[!is.na(x)]
  unique(as.character(x))
}

int_or_null <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- trimws(x)
    if (!nzchar(x) || x %in% c("NA", "NaN")) return(NULL)
  }
  if (is.na(x)) return(NULL)
  as.integer(x)
}

video_columns <- intersect(
  names(moves),
  c("Video Links", "Video Links 1", "Video Links 2", "Video Links 3", "Video Links 4")
)

local_videos_dir <- if (dir.exists("videomoves")) "videomoves" else NULL

escape_regex <- function(x) gsub("([][(){}.+*?^$|\\-\\\\])", "\\\\\\1", x)

local_video_files_for_id <- function(id) {
  if (is.null(local_videos_dir)) return(character())

  pattern <- paste0("^", escape_regex(id), ".*\\.(mp4|m4v|mov)$")
  files <- list.files(
    path = local_videos_dir,
    pattern = pattern,
    full.names = FALSE,
    ignore.case = TRUE
  )

  sort(files)
}

move_records <- lapply(seq_len(nrow(moves)), function(i) {
  row <- moves[i]
  video_links <- compact_chr(unlist(row[, ..video_columns], use.names = FALSE))
  local_video_files <- local_video_files_for_id(as.character(row$ID))

  list(
    id = as.character(row$ID),
    slug = as.character(row$SafeID),
    name = trim_or_null(as.character(row$Name)),
    topic = trim_or_null(as.character(row$Topic)),
    level = trim_or_null(as.character(row$Level)),
    type = trim_or_null(as.character(row$Type)),
    category = trim_or_null(as.character(row$Category)),
    group = trim_or_null(as.character(row$Group)),
    baseMove = trim_or_null(as.character(row$BaseMove)),
    components = trim_or_null(as.character(row$Components)),
    positions = trim_or_null(as.character(row$Positions)),
    seeAlso = trim_or_null(as.character(row[["See also"]])),
    tags = trim_or_null(as.character(row$Tags)),
    description = trim_or_null(as.character(row$Description)),
    source = trim_or_null(as.character(row$Source)),
    comments = trim_or_null(as.character(row$Comments)),
    moveOrder = int_or_null(row$MoveOrder),
    topicCol = int_or_null(row$TopicCol),
    topicOrder = int_or_null(row$TopicOrder),
    familyOrder = int_or_null(row$FamilyOrder),
    valid = isTRUE(row$Valid),
    errors = trim_or_null(as.character(row$Errors)),
    hasLocalVideo = length(local_video_files) > 0,
    videoFiles = I(file.path("videomoves", local_video_files)),
    videoLinks = I(video_links)
  )
})

moves_with_local_video <- sum(vapply(move_records, function(record) isTRUE(record$hasLocalVideo), logical(1)))

setorderv(layout, c("layCol", "LayoutOrder", "Level_order", "MoveOrder"), na.last = TRUE)

layout_columns <- lapply(sort(unique(layout$layCol)), function(col) {
  column_rows <- layout[layCol == col]

  list(
    column = as.integer(col),
    entries = lapply(seq_len(nrow(column_rows)), function(i) {
      row <- column_rows[i]
      list(
        id = trim_or_null(as.character(row$ID)),
        slug = trim_or_null(as.character(row$SafeID)),
        name = trim_or_null(as.character(row$Name)),
        entryType = trim_or_null(as.character(row$EntryType)),
        group = trim_or_null(as.character(row$group)),
        level = trim_or_null(as.character(row$Level)),
        type = trim_or_null(as.character(row$Type)),
        layoutOrder = int_or_null(row$LayoutOrder),
        levelOrder = int_or_null(row$Level_order),
        valid = isTRUE(row$Valid)
      )
    })
  )
})

layout_for_progress <- layout[
  (Valid == TRUE | EntryType == "Title") &
    (is.na(Tags) | !grepl("Stub", Tags, ignore.case = TRUE))
]

trackable_move_count <- layout_for_progress[EntryType == "Data", uniqueN(ID)]

progress_dates <- sort(unique(progress$Date), decreasing = TRUE)

progress_snapshots <- lapply(progress_dates, function(snapshot_date) {
  snapshot <- progress[Date == snapshot_date]
  setorder(snapshot, ID)

  list(
    date = format(snapshot_date, "%Y-%m-%d"),
    summary = list(
      totalMoves = as.integer(trackable_move_count),
      preppedCount = as.integer(sum(snapshot$StatNum_Prep >= 4, na.rm = TRUE)),
      goodSequencingCount = as.integer(sum(snapshot$StatNum_Sequ >= 4, na.rm = TRUE)),
      goodSuccessCount = as.integer(sum(snapshot$StatNum_Succ >= 3, na.rm = TRUE))
    ),
    entries = lapply(seq_len(nrow(snapshot)), function(i) {
      row <- snapshot[i]
      list(
        id = as.character(row$ID),
        preparation = trim_or_null(as.character(row$Preperation)),
        sequencing = trim_or_null(as.character(row$Sequencing)),
        success = trim_or_null(as.character(row$Success)),
        statNumPrep = if (is.na(row$StatNum_Prep)) NULL else as.integer(row$StatNum_Prep),
        statNumSequ = if (is.na(row$StatNum_Sequ)) NULL else as.integer(row$StatNum_Sequ),
        statNumSucc = if (is.na(row$StatNum_Succ)) NULL else as.integer(row$StatNum_Succ)
      )
    })
  )
})

valid_moves <- moves[Valid == TRUE]

search_index <- lapply(seq_len(nrow(valid_moves)), function(i) {
  row <- valid_moves[i]
  searchable_text <- paste(
    compact_chr(c(
      row$Name,
      row$Topic,
      row$Category,
      row$Group,
      row$Type,
      row$Tags,
      row$Description,
      row$Components,
      row$Positions,
      row[["See also"]]
    )),
    collapse = " | "
  )

  list(
    id = as.character(row$ID),
    slug = as.character(row$SafeID),
    title = trim_or_null(as.character(row$Name)),
    topic = trim_or_null(as.character(row$Topic)),
    text = searchable_text
  )
})

manifest <- list(
  generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  source = list(
    rawMoveReference = "data_reference.xlsx",
    moves = "data/dt_pw.RDS",
    layout = "data/dt_pw_lay.RDS",
    progress = "inputs/progress.RDS",
    localVideoDirectory = local_videos_dir,
    visualReference = "_site_reference"
  ),
  counts = list(
    moveRows = nrow(moves),
    validMoves = nrow(valid_moves),
    layoutRows = nrow(layout),
    layoutColumns = length(layout_columns),
    progressRows = nrow(progress),
    progressSnapshots = length(progress_snapshots),
    trackableMoves = as.integer(trackable_move_count),
    movesWithLocalVideo = as.integer(moves_with_local_video)
  ),
  routes = list(
    home = "/",
    moveDetail = "/moves/[slug]",
    progress = "/progress",
    progressEditor = "/progress/editor"
  )
)

write_json(manifest, file.path(output_dir, "manifest.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
write_json(move_records, file.path(output_dir, "moves.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
write_json(layout_columns, file.path(output_dir, "layout.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
write_json(progress_snapshots, file.path(output_dir, "progress.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
write_json(search_index, file.path(output_dir, "search-index.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")

cat("Wrote JSON contracts to", output_dir, "\n")
