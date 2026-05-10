
checkAndCorrectData <- function(dt_main)
{
  # Blank IDs are allowed, this is for unfinished entries
  dt_main = copy(dt_main)
  dt_main[,Valid:=TRUE]
  dt_main[,Errors:=FALSE]
  
  
  nNA = dt_main[is.na(ID),length(ID)]
  if(nNA>0)
  {
    dt_main[is.na(ID),Type:="Unfinished"]
    dt_main[is.na(ID),Valid:=FALSE]
    dt_main[is.na(ID),ID:=paste0("XUF",sprintf("%05d",1:nNA))]
  }
  
  dt_main[Valid==FALSE,]
  
  # Check for duplicates
  if(dt_main[,any(duplicated(ID))])
  {
    nDup = dt_main[duplicated(ID),length(ID)]
    dt_main[duplicated(ID),Valid := FALSE]
    dt_main[duplicated(ID),ID:=paste0("DUP",sprintf("%05d",1:nDup))]
  }
  
  # Check for invalid references
  basemoveInvalid = c() # TODO...
  
  if(length(basemoveInvalid)>0)
  {
    warning(paste0("Some moves refered somethingthat did not exist:\n",paste0(dt_main[ID %in% basemoveInvalid, Name],collapse = ", ")))
  }
  
  naName = dt_main[Valid==TRUE & is.na(Name),ID]
  if(length(naName)>0)
  {
    warning(paste0("Some moves did not have a name specified:\n",paste0(dt_main[ID %in% naName, ID],collapse = ", ")))
    dt_main[ID %in% naName, Valid := FALSE]
    dt_main[ID %in% naName, Errors := TRUE]
  }
  
  naLevel = dt_main[Valid==TRUE & is.na(Level),ID]
  
  if(length(naLevel)>0)
  {
    warning(paste0("Some moves did not have a level specified:\n",paste0(dt_main[ID %in% naLevel, Name],collapse = ", ")))
    dt_main[ID %in% naLevel, Errors := TRUE]
    dt_main[ID %in% naLevel, Level := 4]
  }
  
  ### Check for unspecified topics and families
  noTopic = dt_main[Valid==TRUE & is.na(Topic),ID]
  
  if(length(noTopic)>0)
  {
    warning(paste0("Some moves did not have a Topic specified:\n",paste0(dt_main[ID %in% noTopic, Name],collapse = ", ")))
  }
  
  vMissingTopics = c()
  if(length(vMissingTopics)>0)
  {
    warning(paste0("Some Topics referenced in the move database are not layed out into columns on the splashsheet\n",
                   paste0(vMissingTopics,collapse = ", "),
                   "\nLook at these moves: (",dt_main[Topic %in% vMissingTopics,paste0(ID,collapse = ",")],")"))
  }

  return(dt_main)  
}