# GT table rendering for splash/index views

renderSplash_group <- function(dt) 
{
  # Ensure a data.table for ..cols and fast subsetting
  DT <- data.table::as.data.table(dt)
  has_vid <- "VidLinksHTML" %in% names(DT)
  
  # columns to show
  cols <- c("Level", "Name", if (has_vid) "VidLinksHTML")
  sub  <- DT[, ..cols]
  
  # style masks (handle missing Tags column)
  tag_txt <- if ("Tags" %in% names(DT)) DT[["Tags"]] else rep("", nrow(DT))
  is_var  <- grepl("(?i)\\bvariation\\b",              tag_txt)
  is_add  <- grepl("(?i)\\b(addition|combo|combination)\\b", tag_txt)
  is_base <- !(is_var | is_add)
  
  gt_tab <- gt::gt(sub) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "all"),
      locations = list(gt::cells_body())
    ) |>
    gt::tab_options(
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      column_labels.border.bottom.style = "hidden",
      data_row.padding = 2
    ) |>
    gt::opt_table_font(font = "Helvetica") |>
    gt::opt_horizontal_padding(0.5) |>
    # colouring
    gt::tab_style(
      style = gt::cell_fill("#e7f4ff"),
      locations = gt::cells_body(columns = "Name", rows = which(is_var))
    ) |>
    gt::tab_style(
      style = gt::cell_fill("wheat1"),
      locations = gt::cells_body(columns = "Name", rows = which(is_add))
    ) |>
    gt::tab_style(
      style = gt::cell_fill("#cfe2f3"),
      locations = gt::cells_body(columns = "Name", rows = which(is_base))
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = "12px"),
      locations = gt::cells_body()
    ) |>
    gt::cols_align(align = "center", columns = "Level")
  
  # Conditional steps (do NOT put `{}` on RHS of the base pipe)
  if (has_vid) 
  {
    gt_tab <- gt::fmt_markdown(gt_tab, columns = "VidLinksHTML")
  }
  
  if (has_vid) 
  {
    gt_tab <- gt::cols_label(gt_tab, Level = "", Name = "", VidLinksHTML = "")
  } else 
  {
    gt_tab <- gt::cols_label(gt_tab, Level = "", Name = "")
  }
  
  gt_tab <- gt::cols_width(gt_tab, Level ~ gt::px(20), Name ~ gt::px(260))
  if (has_vid) 
  {
    gt_tab <- gt::cols_width(gt_tab, VidLinksHTML ~ gt::px(80))
  }
  
  gt_tab <- gt::tab_header(gt_tab, title = gt::md(" "))
  gt_tab
}


renderSplash_group <- function(dt)
{
  gt_tab = gt(dt[,.(Level,Name,Type,EntryType,Blank="")]) |> 
    # Table styling
    tab_style(style= cell_borders(sides="all",color=NULL,style=NULL,weight=NULL),
              locations= list(cells_body())) |>
    tab_options(table.border.top.style = "hidden",
                table.border.bottom.style = "hidden",
                column_labels.border.bottom.style ="hidden",
                data_row.padding=2) |>
    opt_table_font(font="Helvetica") |>
    opt_horizontal_padding(0.5) |>
    fmt_markdown(columns="Name") |>
    # Data formatting
    tab_style(style=cell_fill("#cfe2f3"),
              locations=cells_body(columns=c("Level","Name"),
                                   rows= EntryType=="Data")) |>
    tab_style(style=cell_fill("wheat1"),
              locations=cells_body(columns=c("Name"),
                                   rows= Type=="Addition" & EntryType == "Data")) |>
    tab_style(style=cell_fill("#e7f4ff"),
              locations=cells_body(columns=c("Name"),
                                   rows= Type=="Variation" & EntryType == "Data"))|>
    tab_style(style=cell_fill("#cfe2f3"),
              locations=cells_body(columns=c("Name"),
                                   rows= Type=="Stub" & EntryType == "Data")) |>
    tab_style(style=cell_text(weight="bold"),
              locations= cells_body(columns="Name",
                                    rows = EntryType=="Title")) |>
    tab_style(style=list("min-width: 145px; font-size: 11px;",
                         cell_text(size="11px")),
              locations= cells_body(columns="Name")) |>
    tab_style(style=cell_text(size="11px"),
              locations= cells_body(columns="Level")) |>
    # Column specific stuff
    cols_align(align="center","Level") |>
    cols_hide(columns=c(Type,EntryType)) |>
    cols_label(Level="",Name="",Blank="") |>
    cols_width(Level ~ "13px", Name ~ "169px") |>
    rm_header() |>
    tab_options(table.border.top.style = "hidden")
  
  return(gt_tab)
}

#' Render splash group with progress indicators
#' 
#' @param dt data.table with splash layout and progress columns
#' @return gt table object with progress indicator columns
renderSplash_group_progress <- function(dt)
{
  gt_tab = gt(dt[,.(Level,Name,Type,EntryType,StatNum_Prep_cell="",StatNum_Sequ_cell="",StatNum_Succ_cell="",StatNum_Prep,StatNum_Sequ,StatNum_Succ,Blank="")]) |> 
    # Table styling
    tab_style(style= cell_borders(sides="all",color=NULL,style=NULL,weight=NULL),
              locations= list(cells_body())) |>
    tab_options(table.border.top.style = "hidden",
                table.border.bottom.style = "hidden",
                column_labels.border.bottom.style ="hidden",
                data_row.padding=2) |>
    opt_table_font(font="Helvetica") |>
    opt_horizontal_padding(0.5) |>
    fmt_markdown(columns="Name") |>
    # Data formatting
    tab_style(style=cell_fill("#cfe2f3"),
              locations=cells_body(columns=c("Level","Name"),
                                   rows= EntryType=="Data")) |>
    tab_style(style=cell_fill("wheat1"),
              locations=cells_body(columns=c("Name"),
                                   rows= Type=="Addition" & EntryType == "Data")) |>
    tab_style(style=cell_fill("#e7f4ff"),
              locations=cells_body(columns=c("Name"),
                                   rows= Type=="Variation" & EntryType == "Data"))|>
    tab_style(style=cell_fill("#cfe2f3"),
              locations=cells_body(columns=c("Name"),
                                   rows= Type=="Stub" & EntryType == "Data")) |>
    tab_style(style=cell_text(weight="bold"),
              locations= cells_body(columns="Name",
                                    rows = EntryType=="Title")) |>
    tab_style(style=list("min-width: 145px; font-size: 11px;",
                         cell_text(size="11px")),
              locations= cells_body(columns="Name")) |>
    tab_style(style=cell_text(size="11px"),
              locations= cells_body(columns="Level")) |>
    # Progress - Prep
    tab_style(style=cell_fill("#7FFF00"),  # chartreuse2
              locations=cells_body(columns=c("StatNum_Prep_cell"),
                                   rows= StatNum_Prep==4)) |>
    tab_style(style=cell_fill("#FFA07A"),  # lightsalmon
              locations=cells_body(columns=c("StatNum_Prep_cell"),
                                   rows= StatNum_Prep==3)) |>
    tab_style(style=cell_fill("#FFD700"),  # gold
              locations=cells_body(columns=c("StatNum_Prep_cell"),
                                   rows= StatNum_Prep==2)) |>
    tab_style(style=cell_fill("#FFDEAD"),  # navajowhite
              locations=cells_body(columns=c("StatNum_Prep_cell"),
                                   rows= StatNum_Prep==1)) |>
    tab_style(style=cell_fill("#fff5ee"),  # seashell1
              locations=cells_body(columns=c("StatNum_Prep_cell"),
                                   rows= StatNum_Prep==0))  |>
    # Progress - Sequencing
    tab_style(style=cell_fill("#66CD00"),  # chartreuse3
              locations=cells_body(columns=c("StatNum_Sequ_cell"),
                                   rows= StatNum_Sequ==5)) |>
    tab_style(style=cell_fill("#7FFF00"),  # chartreuse2
              locations=cells_body(columns=c("StatNum_Sequ_cell"),
                                   rows= StatNum_Sequ==4)) |>
    tab_style(style=cell_fill("#2F4F4F"),  # darkslategrey
              locations=cells_body(columns=c("StatNum_Sequ_cell"),
                                   rows= StatNum_Sequ==3)) |>
    tab_style(style=cell_fill("#FFD700"),  # gold
              locations=cells_body(columns=c("StatNum_Sequ_cell"),
                                   rows= StatNum_Sequ==2)) |>
    tab_style(style=cell_fill("#EE2C2C"),  # red2
              locations=cells_body(columns=c("StatNum_Sequ_cell"),
                                   rows= StatNum_Sequ==1)) |>
    tab_style(style=cell_fill("white"),
              locations=cells_body(columns=c("StatNum_Sequ_cell"),
                                   rows= StatNum_Sequ==0))  |>
    # Progress - Success
    tab_style(style=cell_fill("#66CD00"),  # chartreuse3
              locations=cells_body(columns=c("StatNum_Succ_cell"),
                                   rows= StatNum_Succ==4)) |>
    tab_style(style=cell_fill("#BCEE68"),  # darkolivegreen2
              locations=cells_body(columns=c("StatNum_Succ_cell"),
                                   rows= StatNum_Succ==3)) |>
    tab_style(style=cell_fill("#FFD700"),  # gold
              locations=cells_body(columns=c("StatNum_Succ_cell"),
                                   rows= StatNum_Succ==2)) |>
    tab_style(style=cell_fill("#FF4500"),  # orangered2
              locations=cells_body(columns=c("StatNum_Succ_cell"),
                                   rows= StatNum_Succ==1)) |>
    tab_style(style=cell_fill("white"),
              locations=cells_body(columns=c("StatNum_Succ_cell"),
                                   rows= StatNum_Succ==0))  |>
    # Column specific stuff
    cols_align(align="center","Level") |>
    cols_hide(columns=c(Type,EntryType,StatNum_Prep,StatNum_Sequ,StatNum_Succ)) |>
    cols_label(Level="",Name="",Blank="",
               StatNum_Prep_cell="",StatNum_Sequ_cell="",StatNum_Succ_cell="") |>
    cols_width(Level ~ "13px", Name ~ "169px",
               StatNum_Succ_cell~"6px",StatNum_Sequ_cell~"6px",StatNum_Prep_cell~"6px") |>
    rm_header() |>
    tab_options(table.border.top.style = "hidden")
  
  return(gt_tab)
}
