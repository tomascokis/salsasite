#' Render move page content
#'
#' Generates all HTML components for a move page: title, CSS, content, and JS.
#' This function is designed to be used with the template-based rendering approach
#' where Quarto is called once to generate a template, then this function generates
#' the dynamic content for each move.
#'
#' @param id Character. The move ID (e.g., "ILT00001")
#' @param dt_pw data.table. The moves data table with columns ID, Name, etc.
#' @param videos_src_dir Character. Path to the directory containing video files.
#' @return A list with components: title, css, content, js
#' @export
#' @importFrom htmltools tags HTML
#' @importFrom data.table data.table

# Silence R CMD check notes for data.table and htmltools
utils::globalVariables(c("ID", "tags"))

render_move_content <- function(id, dt_pw,
                                 videos_src_dir = "/Users/tomascokis/Dropbox/Salsa Library/Partnerwork Encyclopedia") {
  
  # Ensure htmltools tags are available
  tags <- htmltools::tags
  HTML <- htmltools::HTML

  # ---- get the row for this move ----
  row <- dt_pw[ID == id][1]
  if (nrow(row) != 1L) {
    stop(sprintf("Move ID '%s' not found in dt_pw", id))
  }

 # ---- Set page title to the move name ----
  move_name <- if (!is.null(row$Name) && !is.na(row$Name) && nzchar(trimws(row$Name))) {
    row$Name
  } else {
    id
  }

 # ---- build component diagram ----
  dot_code <- make_component_diagram(id, dt_pw, orientation = "TB", return = "dot")

  # ---- locate local videos ----
  escape_regex <- function(x) gsub("([][(){}.+*?^$|\\-\\\\])", "\\\\\\1", x)
  pattern <- paste0("^", escape_regex(id), ".*\\.(mp4|m4v|mov)$")

  vid_paths <- list.files(
    path = videos_src_dir,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  mime_for <- function(p) {
    ext <- tolower(tools::file_ext(p))
    if (ext %in% c("mp4","m4v")) "video/mp4"
    else if (ext == "mov") "video/quicktime"
    else "video/mp4"
  }

  # ---- helpers for tab labels/ids ----
  label_from_path <- function(p) {
    bn <- basename(p)
    m  <- regexpr("\\[(.*?)\\]", bn, perl = TRUE)
    if (m[1] > 0) {
      inside <- regmatches(bn, m)
      inside <- sub("^\\[(.*)\\]$", "\\1", inside)
    } else {
      inside <- tools::file_path_sans_ext(bn)
    }
    parts <- trimws(strsplit(inside, ",")[[1]])
    tools::toTitleCase(paste(parts, collapse = ", "))
  }

  # ---- Build video block ----
  video_block <- if (length(vid_paths)) {
    labels <- vapply(vid_paths, label_from_path, character(1L))
    ord    <- ave(seq_along(labels), labels, FUN = seq_along)
    dup    <- ord > 1
    labels[dup] <- paste0(labels[dup], " #", ord[dup])
    ids <- paste0("mv-vid-", tolower(gsub("[^a-z0-9_-]+", "-", labels)))

    nav_items <- lapply(seq_along(vid_paths), function(i) {
      active <- i == 1
      tags$li(class = "nav-item", role = "presentation",
        tags$button(
          class = paste("nav-link", if (active) "active"),
          `data-bs-toggle` = "tab",
          `data-bs-target` = paste0("#", ids[i]),
          type = "button",
          role = "tab",
          `aria-controls` = ids[i],
          `aria-selected` = if (active) "true" else "false",
          labels[i]
        )
      )
    })

    panes <- lapply(seq_along(vid_paths), function(i) {
      active <- i == 1
      tags$div(
        id = ids[i],
        class = paste("tab-pane fade", if (active) "show active"),
        role = "tabpanel",
        tabindex = "0",
        tags$video(controls = NA, preload = "auto", muted = NA, playsinline = NA, class = "mv-video",
          tags$source(src = sprintf("file://%s", vid_paths[i]), type = mime_for(vid_paths[i]))
        )
      )
    })

    tags$div(
      class = "mv-tabs",
      tags$ul(class = "nav nav-tabs", role = "tablist", nav_items),
      tags$div(class = "tab-content", panes)
    )
  } else {
    tags$p(class = "mv-empty", sprintf("No videos found for %s.", id))
  }

  # ---- Description block ----
  present <- function(x) length(x) && !is.na(x) && nzchar(trimws(x)) && trimws(x) != "\u2014"
  desc_block <- if (present(row[["Description"]])) {
    tags$div(class = "mv-description",
      tags$h5("Description"),
      tags$p(row[["Description"]])
    )
  } else NULL

  # ---- Meta box content ----
  f <- function(x) if (present(x)) x else "\u2014"

  # ---- Compute layout based on diagram size ----
  edges_local <- build_dependency_graph(dt_pw, id, max_depth = Inf,
                                       include_components = TRUE,
                                       include_descendants = TRUE)
  node_ids <- unique(c(edges_local$From, edges_local$To, id))
  node_count <- length(node_ids)

  has_diagram <- node_count > 1
  use_wide_layout <- node_count > 6

  grid_cols <- if (use_wide_layout) {
    "minmax(700px, 0.8fr) minmax(650px, 2.8fr)"
  } else {
    "minmax(700px, 1.4fr) minmax(260px, 0.8fr)"
  }

  card_max_width <- if (use_wide_layout) "none" else "400px"
  diag_container_max_width <- if (use_wide_layout) "650px" else "520px"

  # ---- Build meta list ----
  meta_children <- list(
    tags$dt("ID"),         tags$dd(row$ID),
    tags$dt("Name"),       tags$dd(f(row$Name)),
    tags$dt("Level"),      tags$dd(f(row$Level)),
    tags$dt("Topic"),      tags$dd(f(row$Topic))
  )
  if (present(row[["Tags"]]))      meta_children <- c(meta_children, list(tags$dt("Tags"),      tags$dd(row[["Tags"]])))
  if (present(row[["Positions"]])) meta_children <- c(meta_children, list(tags$dt("Positions"), tags$dd(row[["Positions"]])))
  if (present(row[["See also"]]))  meta_children <- c(meta_children, list(tags$dt("See also"),  tags$dd(row[["See also"]])))
  vcol <- intersect(names(row), c("Video Links","Video","VideoURL"))[1]
  if (length(vcol) && present(row[[vcol]])) {
    meta_children <- c(meta_children, list(
      tags$dt("Dropbox video link"),
      tags$dd(tags$a(href = row[[vcol]], target = "_blank", "Open in new tab"))
    ))
  }
  meta_dl <- tags$dl(class = "mv-meta", meta_children)

  # ---- Build CSS ----
  css_prefix <- "  main.content, .quarto-container, .page-columns { max-width: 1200px; }\n  .mv-header {\n    display: grid;\n    grid-template-columns: "

  css_body <- paste0(
    ";\n    gap: 16px;\n    align-items: start;\n    margin: 0.75rem 0 1.25rem;\n  }\n  .mv-video {\n    width: 100%;\n    height: auto;\n    max-height: 700px;\n    object-fit: contain;\n    display: block;\n    margin: 0 0 12px 0;\n    border-radius: 12px;\n    box-shadow: 0 1px 6px rgba(0,0,0,.06);\n    background: #000;\n  }\n  .mv-empty { color: #777; }\n  .mv-card {\n    border: 1px solid #e6e6e6;\n    border-radius: 12px;\n    padding: 12px 14px;\n    background: #fafafa;\n    max-width:", card_max_width, ";\n  }\n  .mv-meta {\n    margin: 0;\n    font-size: 0.95rem;\n    line-height: 1.2;\n  }\n  .mv-meta dt {\n    font-weight: 600;\n    color: #444;\n    margin-top: 6px;\n    margin-bottom: 2px;\n  }\n  .mv-meta dd {\n    margin: 0 0 4px 0;\n    color: #333;\n    word-break: break-word;\n  }\n  .mv-diagram-wrap {\n    position: relative;\n    max-width: 100%;\n  }\n  .mv-diagram-container {\n    max-width:", diag_container_max_width, ";\n    max-height: 600px;\n    overflow: auto;\n    border: 1px solid #ddd;\n    border-radius: 8px;\n    padding: 8px;\n    background: #fff;\n  }\n  .mv-diagram-wrap svg {\n    width: 100%;\n    height: auto;\n    display: block;\n    margin: 0 auto;\n  }\n  .mv-diagram-wrap .html-widget {\n    max-width: 100% !important;\n    height: auto !important;\n  }\n  @media (max-width: 860px) {\n    .mv-header { grid-template-columns: 1fr; }\n    .mv-diagram-container { max-width: 100%; max-height: 500px; }\n  }\n  .mv-description {\n    margin-top: 0.5em;\n    font-size: 0.9rem;\n    line-height: 1.35;\n  }\n  .mv-description h5 {\n    font-size: 1rem;\n    margin-bottom: 0.3em;\n    color: #555;\n  }\n  .mv-diagram-section {\n    margin-top: 0.5em;\n  }\n  .mv-diagram-section h5 {\n    font-size: 1rem;\n    font-weight: 600;\n    margin-top: 6px;\n    margin-bottom: 2px;\n    color: #444;\n  }\n  .mv-page-title {\n    font-size: 1.75rem;\n    font-weight: 600;\n    margin: 0 0 0.5rem 0;\n    color: #333;\n  }\n  /* Fullscreen button for large diagrams */\n  .mv-diagram-fullscreen-btn {\n    position: absolute;\n    top: 8px;\n    right: 8px;\n    z-index: 10;\n    background: rgba(255,255,255,0.9);\n    border: 1px solid #ccc;\n    border-radius: 6px;\n    padding: 4px 8px;\n    cursor: pointer;\n    font-size: 1rem;\n    line-height: 1;\n    box-shadow: 0 1px 4px rgba(0,0,0,0.1);\n    transition: background 0.15s, box-shadow 0.15s;\n  }\n  .mv-diagram-fullscreen-btn:hover {\n    background: #fff;\n    box-shadow: 0 2px 8px rgba(0,0,0,0.15);\n  }\n  /* Fullscreen overlay */\n  .mv-fullscreen-overlay {\n    display: none;\n    position: fixed;\n    top: 0; left: 0; right: 0; bottom: 0;\n    z-index: 9999;\n    background: rgba(0,0,0,0.85);\n    justify-content: center;\n    align-items: center;\n    padding: 20px;\n  }\n  .mv-fullscreen-overlay.active {\n    display: flex;\n  }\n  .mv-fullscreen-content {\n    position: relative;\n    background: #fff;\n    border-radius: 12px;\n    padding: 16px;\n    max-width: 95vw;\n    max-height: 95vh;\n    overflow: auto;\n    box-shadow: 0 4px 30px rgba(0,0,0,0.3);\n  }\n  .mv-fullscreen-content svg {\n    display: block;\n    max-width: none;\n    height: auto;\n  }\n  .mv-fullscreen-close {\n    position: absolute;\n    top: 8px;\n    right: 12px;\n    background: #f5f5f5;\n    border: 1px solid #ccc;\n    border-radius: 6px;\n    padding: 4px 10px;\n    cursor: pointer;\n    font-size: 1.2rem;\n    line-height: 1;\n    z-index: 10;\n  }\n  .mv-fullscreen-close:hover {\n    background: #eee;\n  }")

  css_tag <- tags$style(HTML(paste0(css_prefix, grid_cols, css_body)))

  # ---- Build JS ----
  js_tag <- tags$script(HTML("
document.addEventListener('DOMContentLoaded', function () {
  function primeVideo(v) {
    try {
      v.muted = true;
      v.preload = 'auto';
      const p = v.play();
      if (p && typeof p.then === 'function') {
        p.then(function(){ v.pause(); v.currentTime = 0; }).catch(function(){});
      } else {
        v.pause(); v.currentTime = 0;
      }
    } catch (e) {}
  }

  document.querySelectorAll('.mv-video').forEach(function(v){
    if (v.readyState < 2) {
      v.addEventListener('loadeddata', function once(){ v.removeEventListener('loadeddata', once); primeVideo(v); });
      try { v.load(); } catch(e) {}
    } else {
      primeVideo(v);
    }
  });

  document.querySelectorAll('.nav-tabs [data-bs-toggle=\"tab\"]').forEach(function(btn){
    btn.addEventListener('shown.bs.tab', function(ev){
      var sel = ev.target.getAttribute('data-bs-target');
      var pane = document.querySelector(sel);
      if (!pane) return;
      var v = pane.querySelector('.mv-video');
      if (!v) return;
      if (v.readyState < 2) {
        v.addEventListener('loadeddata', function once(){ v.removeEventListener('loadeddata', once); primeVideo(v); });
        try { v.load(); } catch(e) {}
      } else {
        primeVideo(v);
      }
    });
  });
    
  // --- Diagram: render DOT with Viz.js (separate from video tab handling)
  var dotEl = document.getElementById('mv-dot');
  var outEl = document.getElementById('mv-diagram');
  if (dotEl && outEl) {
    var dot = dotEl.textContent || '';
    var s1 = document.createElement('script');
    s1.src = 'https://unpkg.com/viz.js@2.1.2/viz.js';
    s1.onload = function(){
      var s2 = document.createElement('script');
      s2.src = 'https://unpkg.com/viz.js@2.1.2/full.render.js';
      s2.onload = function(){
        try {
            var viz = new Viz();
            window._mvViz = viz;
              window._mvDot = dot;
              
              viz.renderSVGElement(dot).then(function(svg){
                outEl.innerHTML = '';
                outEl.appendChild(svg);

                try {
                  var container = outEl.parentElement;
                  var isWide = container && container.dataset && container.dataset.wide === '1';
                  if (isWide) {
                    var svgWidth = 0;
                    if (svg.viewBox && svg.viewBox.baseVal && svg.viewBox.baseVal.width) {
                      svgWidth = svg.viewBox.baseVal.width;
                    } else if (svg.getBBox) {
                      svgWidth = svg.getBBox().width;
                    } else {
                      svgWidth = svg.getBoundingClientRect().width;
                    }
                    var contW = container.clientWidth || 650;
                    var scale = Math.min(1, contW / svgWidth);
                    var minScale = 0.7;
                    if (scale < minScale) scale = minScale;
                    var targetWidth = Math.round(svgWidth * scale);
                    svg.style.width = targetWidth + 'px';
                    svg.style.height = 'auto';
                  }
                } catch (e) {}
                
                setupFullscreenHandlers();
                
              }).catch(function(err){
              outEl.innerHTML = '<pre style=\"white-space:pre-wrap;color:#b00\"></pre>';
              outEl.firstChild.textContent = 'Graphviz render error: ' + (err && err.message ? err.message : err);
            });
          } catch(e) {
            outEl.textContent = 'Could not initialize Viz: ' + e;
          }
        };
        document.head.appendChild(s2);
      };
      document.head.appendChild(s1);
  }
  
  function setupFullscreenHandlers() {
    var fsBtn = document.getElementById('mv-fullscreen-btn');
    var fsOverlay = document.getElementById('mv-fullscreen-overlay');
    var fsClose = document.getElementById('mv-fullscreen-close');
    var fsDiagram = document.getElementById('mv-fullscreen-diagram');
    
    if (!fsBtn || !fsOverlay) return;
    
    fsBtn.addEventListener('click', function() {
      if (window._mvViz && window._mvDot && fsDiagram) {
        fsDiagram.innerHTML = '<p style=\"color:#888;\">Loading diagram...</p>';
        window._mvViz.renderSVGElement(window._mvDot).then(function(svg) {
          fsDiagram.innerHTML = '';
          fsDiagram.appendChild(svg);
          svg.style.transform = 'scale(0.8)';
          svg.style.transformOrigin = 'top left';
          svg.style.width = 'auto';
          svg.style.maxWidth = 'none';
          svg.style.height = 'auto';
        }).catch(function(err) {
          fsDiagram.innerHTML = '<p style=\"color:#b00;\">Error rendering diagram</p>';
        });
      }
      fsOverlay.classList.add('active');
      document.body.style.overflow = 'hidden';
    });
    
    function closeFullscreen() {
      fsOverlay.classList.remove('active');
      document.body.style.overflow = '';
    }
    
    if (fsClose) {
      fsClose.addEventListener('click', closeFullscreen);
    }
    
    fsOverlay.addEventListener('click', function(e) {
      if (e.target === fsOverlay) {
        closeFullscreen();
      }
    });
    
    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape' && fsOverlay.classList.contains('active')) {
        closeFullscreen();
      }
    });
  }
});
"))

  # ---- Build main content ----
  content_tag <- tags$div(
    tags$div(class = "mv-header",
      tags$div(class = "mv-left",
        video_block,
        desc_block
      ),
      tags$div(class = "mv-right mv-card",
        meta_dl,
        tags$div(class = "mv-diagram-section",
          tags$h5("Relationship to other moves"),
          tags$div(class = "mv-diagram-wrap",
            if (!has_diagram) {
              tags$p(style = "margin:0", "This move has no components")
            } else {
              tags$div(
                if (use_wide_layout) {
                  tags$button(class = "mv-diagram-fullscreen-btn", id = "mv-fullscreen-btn",
                              title = "View fullscreen", "\u26F6")
                } else NULL,
                tags$div(class = "mv-diagram-container", `data-wide` = if (use_wide_layout) "1" else "0",
                  tags$div(id = "mv-diagram", style = "width:100%; height:auto;")
                ),
                tags$script(id = "mv-dot", type = "text/plain", htmltools::HTML(dot_code))
              )
            }
          )
        )
      )
    ),
    # Fullscreen overlay
    tags$div(class = "mv-fullscreen-overlay", id = "mv-fullscreen-overlay",
      tags$div(class = "mv-fullscreen-content",
        tags$button(class = "mv-fullscreen-close", id = "mv-fullscreen-close", "\u00D7"),
        tags$div(id = "mv-fullscreen-diagram")
      )
    )
  )

  # Return all components
  list(
    title = move_name,
    css = as.character(css_tag),
    content = as.character(content_tag),
    js = as.character(js_tag)
  )
}
