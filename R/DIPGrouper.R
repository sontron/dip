#' DIPGrouper
#' 
#' DIP Grouping function
#' @export

DIPGrouper<-function(data,lstrules=lstRules){
  require('magrittr')
  require('stringi')
  adrgVarsLst=ADRGVarsLst
  as.data.frame(data)->data
  if(!is.element('myRowID',names(data))){
    data$myRowID<-1:nrow(data)
  }
  
  names(data)->namesDt
  
  # as.numeric(data$bornWt)->data$bornWt
  # as.numeric(data$age)->data$age
  # as.numeric(data$LOS)->data$LOS
  # ifelse(data$age==0,1e-10,data$age)->data$age
  # ifelse(data$LOS==0,1e-10,data$LOS)->data$LOS
  data[data=='NA']<-NA
  as.data.frame(data)->data
  ifelse(data$PDX%in%adrgVarsLst$PDX,data$PDX,'OutOfRange')->data$PDX
  data$PDX[is.na(data$PDX)]<-'OutOfRange'
  for(i in 1:15){
    ifelse(data[,paste0('ADX',i)]%in%adrgVarsLst$PDX,data[,paste0('ADX',i)],'OutOfRange')->data[,paste0('ADX',i)]
    ifelse(is.na(data[,paste0('ADX',i)]),'OutOfRange',data[,paste0('ADX',i)])->data[,paste0('ADX',i)]
  }
  
  for(i in 1:8){
    ifelse(data[,paste0('PROC',i)]%in%adrgVarsLst$PPROC,data[,paste0('PROC',i)],'OutOfRange')->data[,paste0('PROC',i)]
    ifelse(is.na(data[,paste0('PROC',i)]),'OutOfRange',data[,paste0('PROC',i)])->data[,paste0('PROC',i)]
  }
  
  CCExcl(data=data,ccEcl=CCEclCHS) %>% CCStat2(data=.)->dt
  as.data.frame(dt)->dt
  
  # names(lstrules)->icds
  # icds[which(stri_detect_fixed(icds,'x'))]->icdX
  # paste('(',paste(substr(icdX,1,3),collapse='|'),')',sep='')->icdXReg
  # 
  # dt$subPDX<-substr(dt$PDX,1,5)
  # 
  # ifelse(stri_detect_regex(dt$subPDX,icdXReg),paste0(substr(dt$subPDX,1,4),'x'),dt$subPDX)->dt$subPDX
  
  dt$PROCs<-apply(dt[,paste0('PROC',1:8)],1,function(i)paste(i,collapse=';'))
  
  t(sapply(1:nrow(dt),function(i){
    # if(is.element(dt$subPDX[i],names(lstrules))){  ## 注销
      if(any(stri_startswith_fixed(dt$PDX[i],names(lstrules)))){  ## 更换成开头字符判断
      
      # lstrules[dt$subPDX[i]]->lstrulesi  ## 注销
        lstrules[which(stri_startswith_fixed(dt$PDX[i],names(lstrules)))]->lstrulesi
      sapply(1:length(lstrulesi[[1]]),function(j){
        all(stri_detect_regex(dt$PROCs[i],lstrulesi[[1]][[j]][['PROC']]),na.rm=T)
      })->indj
      
      # names(lstrulesi[[1]])[which(indj)[1]]->dipcode
      names(lstrulesi[[1]])[which(indj)[which.max(sapply(lstrulesi[indj],length))]]->dipcode
      
      # setdiff(unlist(lstrulesi[[1]][indj][[1]]$PROC),'.')->procsj
      setdiff(unlist(lstrulesi[[1]][which(indj)[which.max(sapply(lstrulesi[indj],length))]][[1]]$PROC),'.')->procsj
      
      if(length(procsj)>0){
        paste(procsj,collapse='+')->procsjj
      } else {
        procsjj<-'.'
      }
      return(c(dipcode,procs=procsjj))
      
    } else {
      return(c(dipcode=NA,procs=NA))
    }
    
  }))->dipCodes
  dt$DIPCode<-dipCodes[,1]
  dt$procs<-dipCodes[,2]
  dt[dt=='OutOfRange']<-NA
  ifelse(dt$DIPCode%in%dipDel$delete,paste0(substr(dt$DIPCode,1,5),'B'),dt$DIPCode)->dt$DIPCode
  return(dt[,c(namesDt,'CCStatus','DIPCode','procs')])
}


#' DIPGrouper2
#' 
#' @export

DIPGrouper2<-function(data){
  DIPGrouper(data)->res
  
  subset(res,is.na(DIPCode))->res2
  subset(res,!is.na(DIPCode))->res1
  
  if(nrow(res2)>0){
    res2[,-which(names(res2)%in%c('DIPCode','procs','CCStatus','myRowID'))]->dt2
    
    DIPGrouper(dt2,lstrules = lstRules2)->res2New
    if(nrow(res2)==nrow(res)){
      return(res2New)
    } else {
      return(rbind(res1,res2New))
    }
  } else {
    return(res)
  }
}
