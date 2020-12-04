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
  
  
  dt$subPDX<-substr(dt$PDX,1,5)
  
  dt$PROCs<-apply(dt[,paste0('PROC',1:8)],1,function(i)paste(i,collapse=';'))
  
  t(sapply(1:nrow(dt),function(i){
    if(is.element(dt$subPDX[i],names(lstrules))){
      lstrules[dt$subPDX[i]]->lstrulesi
      sapply(1:length(lstrulesi[[1]]),function(j){
        all(stri_detect_regex(dt$PROCs[i],lstrulesi[[1]][[j]][['PROC']]),na.rm=T)
      })->indj
      names(lstrulesi[[1]])[which(indj)[1]]->dipcode
      setdiff(unlist(lstrulesi[[1]][indj][[1]]$PROC),'.')->procsj
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
  return(dt[,c(namesDt,'CCStatus','DIPCode','procs')])
}



