
# Sanitize ID for safe filename/URL: replace only '/' with '_' to avoid collisions
sanitize_id <- function(x) {
  gsub("/", "_", as.character(x), fixed = TRUE)
}

source.all <- function(folder) 
{
  files = list.files(folder,pattern="*.R$")
  
  for(iFile in files)
  {
    tryCatch({
      source(paste0(folder,"/",iFile))
    },
    error=function(e){stop(paste0("**Error sourcing ",iFile,": '",e,"'"))},
    warning=function(w){warning(paste0("**Warning sourcing ",iFile,": '",w,"'"))}
    )
  }
  
  return(paste0("Sourced ",length(files)," files in ", folder,"/"))
}


