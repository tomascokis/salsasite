# Components parsing, dependency graph, Graphviz generator

parse_components <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character())
  comps <- unlist(strsplit(x, "[;,+]"))
  comps <- trimws(comps)
  comps[nzchar(comps)]
}

# Declare data.table non-standard-evaluation globals to avoid R CMD check NOTES
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("From", "FromName", "To", "ToName"))
}

# Sanitize ID for safe filename/URL: replace only '/' with '_'
# (This duplicates the helper in header.R for local safety and clarity)
.sanitize_id <- function(x) {
  gsub("/", "_", as.character(x), fixed = TRUE)
}

build_dependency_graph <- function(dt_pw, target_id, max_depth = Inf,
                                   include_components = TRUE,
                                   include_descendants = FALSE) {
  stopifnot(is.data.frame(dt_pw))
  if (!all(c("ID", "Name", "Components") %in% names(dt_pw))) {
    stop("dt_pw must have columns: ID, Name, Components")
  }
  DT <- data.table::as.data.table(dt_pw)
  
  name_map <- setNames(as.character(DT$Name), DT$ID)
  # Pre-parse components for each row so lookups for children are fast
  comps_list <- lapply(as.character(DT$Components), function(s) {
    if (is.na(s) || !nzchar(s)) character() else parse_components(s)
  })
  names(comps_list) <- as.character(DT$ID)

  get_components <- function(id) {
    id_chr <- as.character(id)
    if (!id_chr %in% names(comps_list)) character() else comps_list[[id_chr]]
  }

  # Find rows that list `id` as one of their components -> these are children/descendants
  get_children <- function(id) {
    id_chr <- as.character(id)
    if (!nzchar(id_chr)) return(character())
    hits <- vapply(comps_list, function(x) id_chr %in% x, logical(1))
    names(comps_list)[hits]
  }
  
  edges   <- data.table::data.table(From = character(), To = character())

  # Perform parent- and child-traversals separately to avoid uncontrolled expansion
  # when both directions are requested. Using separate "visited" sets ensures we
  # don't mix paths (e.g. up->down->up) which would traverse the whole connected
  # component.
  if (isTRUE(include_components)) {
    visited_par <- new.env(parent = emptyenv())
    recurse_par <- function(id, depth = 0L) {
      if (depth >= max_depth) return(invisible())
      comps <- get_components(id)
      if (!length(comps)) return(invisible())
      edges <<- data.table::rbindlist(
        list(edges, data.table::data.table(From = comps, To = id)),
        use.names = TRUE, fill = TRUE
      )
      for (c in comps) {
        if (!exists(c, envir = visited_par, inherits = FALSE)) {
          assign(c, TRUE, envir = visited_par)
          recurse_par(c, depth + 1L)
        }
      }
    }
    assign(target_id, TRUE, envir = visited_par)
    recurse_par(target_id, 0L)
  }

  if (isTRUE(include_descendants)) {
    visited_ch <- new.env(parent = emptyenv())
    recurse_ch <- function(id, depth = 0L) {
      if (depth >= max_depth) return(invisible())
      childs <- get_children(id)
      if (!length(childs)) return(invisible())
      edges <<- data.table::rbindlist(
        list(edges, data.table::data.table(From = id, To = childs)),
        use.names = TRUE, fill = TRUE
      )
      for (ch in childs) {
        if (!exists(ch, envir = visited_ch, inherits = FALSE)) {
          assign(ch, TRUE, envir = visited_ch)
          recurse_ch(ch, depth + 1L)
        }
      }
    }
    assign(target_id, TRUE, envir = visited_ch)
    recurse_ch(target_id, 0L)
  }
  
  if (!nrow(edges)) {
    return(data.table::data.table(
      From = character(), FromName = character(),
      To   = character(), ToName   = character()
    ))
  }
  
  edges <- unique(edges)
  # Avoid data.table::`:=` to keep static analysis/linting happy
  edges$FromName <- name_map[as.character(edges$From)]
  edges$ToName   <- name_map[as.character(edges$To)]
  data.table::setcolorder(edges, c("From", "FromName", "To", "ToName"))
  edges[]
}

# Requires: DiagrammeR (only if return = "grViz")
make_component_diagram <- function(
  move_id, dt_pw, max_depth = Inf,
  orientation = c("TB", "LR"),
  link_base  = "moves",
  include_components = TRUE,
  include_descendants = TRUE,
  return     = c("dot", "grViz")
) {
  orientation <- match.arg(orientation)
  return      <- match.arg(return)
  
  edges <- build_dependency_graph(
    dt_pw, move_id, max_depth,
    include_components = include_components,
    include_descendants = include_descendants
  )
  # Note: don't return NA when there are no edges — still render a single-node graph
  
  DT <- data.table::as.data.table(dt_pw)
  id_to_name <- setNames(as.character(DT$Name), DT$ID)
  # detect which column (if any) contains video links
  vcol <- intersect(names(DT), c("Video Links", "Video", "VideoURL"))[1]
  has_video_id <- if (length(vcol) && nzchar(vcol)) {
    idx <- which(!is.na(DT[[vcol]]) & nzchar(as.character(DT[[vcol]])))
    ids <- as.character(DT$ID[idx])
    if (length(ids) == 0L) setNames(logical(0), character(0)) else setNames(rep(TRUE, length(ids)), ids)
  } else setNames(logical(0), character(0))
  
  # SAFE name lookup with fallback to id
  nm_for <- function(id) {
    val <- unname(id_to_name[id])  # [ ] returns NA when missing
    if (length(val) == 0L || is.na(val) || !nzchar(val)) id else val
  }
  
  node_ids    <- unique(c(edges$From, edges$To, move_id))
  node_labels <- vapply(node_ids, nm_for, character(1))
  # append a small play icon for nodes that have videos
  # use safe subsetting (single-bracket) and check for NA to avoid
  # "subscript out of bounds" from using [[ on atomic vectors
  if (length(has_video_id)) {
    node_labels <- vapply(node_ids, function(id) {
      lab <- nm_for(id)
      hv <- has_video_id[as.character(id)]
      if (length(hv) && !is.na(hv) && isTRUE(hv)) {
        paste0(lab, " ▶")
      } else lab
    }, character(1))
  }
  
  # optional: warn if some IDs aren’t in dt_pw$ID
  missing_ids <- setdiff(node_ids, names(id_to_name))
  if (length(missing_ids)) {
    warning(sprintf(
      "ID(s) not found in dt_pw$ID: %s",
      paste(missing_ids, collapse = ", ")
    ))
  }
  
  # Sanitize DOT IDs (labels/URLs keep originals)
  # Note: .sanitize_id() replaces '/' with '_' for safe filenames/URLs
  safe_ids <- sapply(node_ids, .sanitize_id, USE.NAMES = TRUE)
  id_map   <- setNames(safe_ids, node_ids)
  
  esc_dot <- function(x) gsub('"', '\\"', x, fixed = TRUE)
  
  # Nodes
  node_lines <- vapply(seq_along(node_ids), function(i) {
    id      <- node_ids[i]
    safe_id <- id_map[[id]]
  label   <- esc_dot(node_labels[i])
  # Use sanitized id for URL/file names so characters like '/' don't
  # produce invalid links or directory paths. Keep the original id
  # only for display purposes; id_map maps original -> safe id.
  url_id  <- id_map[[id]]
  url     <- sprintf("%s.html", url_id)
    attrs <- c(
      sprintf('label="%s"', label),
      sprintf('URL="%s"', esc_dot(url)),
      sprintf('tooltip="%s"', esc_dot(paste0("Open ", label))),
      'target="_blank"'
    )
    if (identical(id, move_id)) {
      attrs <- c(attrs, 'style="filled,rounded"', 'fillcolor="lightgoldenrod1"')
    } else {
      attrs <- c(attrs, 'style="rounded"')
    }
    sprintf('"%s" [%s];', safe_id, paste(attrs, collapse = ", "))
  }, character(1))
  
  # Edges
  edge_lines <- sprintf('"%s" -> "%s";', id_map[edges$From], id_map[edges$To])
  
  rankdir <- if (orientation == "LR") "LR" else "TB"
  dot <- paste0(
    'digraph {\n',
    sprintf('  graph [rankdir=%s, nodesep=0.4, ranksep=0.6];\n', rankdir),
    '  node  [shape=box, fontname="Helvetica", color="#bbbbbb"];\n',
    '  edge  [arrowhead=normal, color="#999999"];\n\n',
    '  // Nodes\n  ', paste(node_lines, collapse = "\n  "), '\n\n',
    '  // Edges\n  ', paste(edge_lines, collapse = "\n  "), '\n',
    '}\n'
  )
  
  if (return == "dot") dot else DiagrammeR::grViz(dot)
}