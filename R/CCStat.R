#' CCStat2
#' 
#' CCStat2 is a function that return the CCstatus of data
#' 
#' @export

CCStat2<-function(data,ccmat=CCMat){
  require(data.table)
  as.data.frame(data)->data
  
  CCMat[data$ADX1,,drop=F]->CCStatADX1
  
  for(i in 2:15){
    CCStatADX1|CCMat[data[,paste0('ADX',i)],,drop=F]->CCStatADX1
  }  
  
  
  apply(CCStatADX1[,,drop=F],1,function(i){
    ifelse(i[3],'2',ifelse(i[2],'1','0'))
  })->CCss
  
  data$CCStatus<-CCss
  
  
  return(data)
  
}