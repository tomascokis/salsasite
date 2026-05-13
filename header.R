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

install_missing_packages <- function(packages) 
{
  # Find packages that are not currently installed
  missing_packages <- packages[!packages %in% rownames(installed.packages())]
  
  # Install any missing packages
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  invisible(missing_packages)
}