loadMainData <- function(doc_link,move_sheet)
{
  data = data.table(read_sheet(doc_link,sheet=move_sheet,skip=1))
  
  data = data[,.(Level,ID,Name,Topic,Components,Positions,`See also`,Tags,Description,Source,Comments,
          `Video Links 1`, `Video Links 2`,`Video Links 3`,`Video Links 4`)]
  data[,MoveOrder := 1:nrow(data)]
  
  data[,`Video Links` := `Video Links 1`]
  data[is.na(`Video Links`),`Video Links` := `Video Links 2`]
  data[is.na(`Video Links`),`Video Links` := `Video Links 3`]
  data[is.na(`Video Links`),`Video Links` := `Video Links 4`]
  
  for(iRow in 1:nrow(data))
  {
    if(data[iRow,!is.na(Components)])
      data[iRow, BaseMove := stringr::str_split_1(Components, pattern=" \\+ ")[[1]]] 
  }

  data[grepl("Addition",Tags),Type := "Addition"]
  data[grepl("Variation",Tags),Type := "Variation"]
  data[grepl("Stub",Tags),Type := "Stub"]
  
  return(data[])
}

loadSplashLayout <- function(doc_link, layout_sheet)
{
  dt_splashlayout = data.table(read_sheet(doc_link, layout_sheet,skip=2))
  
  dt_splashlayout[,TopicOrder:=1:nrow(dt_splashlayout)]
  dt_splashlayout[,FamilyOrder:=1:nrow(dt_splashlayout)]
  
  return(dt_splashlayout)
}

# Sanitize ID for safe filename/URL: replace only '/' with '_' to avoid collisions
sanitize_id <- function(x) 
  {
  gsub("/", "_", as.character(x), fixed = TRUE)
}


