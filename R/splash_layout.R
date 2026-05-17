layoutSplashSheet <- function(dt_splashsheet,groupNameCol,groupLayColCol,additionUnderStub)
{
  dt_splashsheet = copy(dt_splashsheet)
  setnames(dt_splashsheet,c(groupNameCol,groupLayColCol),c("group","layCol"))
  
  dt_splashsheet = dt_splashsheet[Valid==TRUE,]
  dt_splashsheet[,Level_order:=Level]
  
  ## Do we put addition under the group of their base move?
  ## Note this means the group set under an addition only actually matters if the base or addition move is not defined.
  if(additionUnderStub==FALSE)
  {
    dt_splashsheet[Type=="Addition" & !is.na(BaseMove), group:= dt_splashsheet$group[match(BaseMove,dt_splashsheet$ID)]]
    dt_splashsheet[Type=="Addition" & !is.na(BaseMove), MoveOrder:=  0.1+dt_splashsheet$MoveOrder[match(BaseMove,dt_splashsheet$ID)]]
    dt_splashsheet[Type=="Addition" & !is.na(BaseMove), layCol:=  dt_splashsheet$layCol[match(BaseMove,dt_splashsheet$ID)]]
    dt_splashsheet[Type=="Addition" & !is.na(BaseMove), Level_order:=  dt_splashsheet$Level_order[match(BaseMove,dt_splashsheet$ID)]]
    
  } # note, 0.1 is added so that additions always sit under their parent
  
  else
  {
    dt_splashsheet[Type=="Addition" & !is.na(AddedMove), group:= dt_splashsheet$group[match(AddedMove,dt_splashsheet$ID)]]
    dt_splashsheet[Type=="Addition" & !is.na(AddedMove), MoveOrder:= 0.1+dt_splashsheet$MoveOrder[match(AddedMove,dt_splashsheet$ID)]]
    dt_splashsheet[Type=="Addition" & !is.na(AddedMove), layCol:= dt_splashsheet$layCol[match(AddedMove,dt_splashsheet$ID)]]
    dt_splashsheet[Type=="Addition" & !is.na(AddedMove), Level_order:=  dt_splashsheet$Level_order[match(AddedMove,dt_splashsheet$ID)]]
  }  
  
  dt_splashsheet = dt_splashsheet[!is.na(layCol),]
  dt_splashsheet = dt_splashsheet[order(group %in% dt_splashsheet[,unique(group)],Level_order,MoveOrder),]
  
  lDT = list()
  for(iTopic in dt_splashsheet[!is.na(group),unique(group)])
  {
    dt_topicbody = dt_splashsheet[group==iTopic,]
    dt_topicbody[,EntryType:="Data"]
    dt_title = dt_topicbody[1,.(layCol=layCol,Level="",ID=NA,Name=iTopic,group=group,EntryType="Title")]
    dt_blank = dt_topicbody[1,.(layCol=layCol,Level="",ID=NA,Name="",group=group,EntryType="Blank")]
    
    firstTopicInCol = dt_splashsheet[group == dt_topicbody$group[1]][,match(iTopic,group)]==1
    
    if(firstTopicInCol)
    {
      dt_all = rbind(dt_title,
                     dt_topicbody,
                     fill=TRUE)
    } else {
      dt_all = rbind(dt_blank,
                     dt_title,
                     dt_topicbody,
                     fill=TRUE)
    }
    
    lDT[[length(lDT)+1]] = dt_all
  }
  
  layedOutTable = rbindlist(lDT)
  layedOutTable[order(layCol),LayoutOrder := 1:.N]
  
  # makeHTMLbadge <- Vectorize(function(link,name,colour="white")
  # {
  #   if(is.na(link) | link=="")
  #     return("")
  #   return(paste0('<a class="btn btn-default" target="_blank" style="line-height:1.6;font-size:8.5px; border:0px; background-color:',colour,';" href="',link,'">',name,'</a>'))
  # },
  # "link")
  # 
  # layedOutTable[,VidLinksHTML:=(paste0(
  #   '<div class="btn-group btn-group-xs" role="group">\n',
  #   makeHTMLbadge(VidLink.CountOn2,"&#8278;","#84FFFF"),
  #   makeHTMLbadge(VidLink.MusicOn2,"&#127925;","#8C9EFF"),
  #   makeHTMLbadge(VidLink.CountOn1,"&#8278;","#FFD740"),
  #   makeHTMLbadge(VidLink.MusicOn1,"&#127925;","#FF6E40"),
  #   '\n</div>'
  # ))]
  # 
  
  return(layedOutTable)
}
