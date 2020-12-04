#' CCExcl
#' 
#' @export
CCExcl<-function(data,ccEcl=CCEclCHS){
  
  require('data.table')
  #require('stronmisc')
  
  if(is.character(ccEcl)){
    eval(as.name(ccEcl))->CCEcl
  } else {ccEcl->CCEcl}
  
  
  CCEcl->ICDCCMap
  
  merge(data,ICDCCMap,by.x='PDX',by.y='ICDCode',all.x=T)->data
  
  paste("ADX",1:15,sep='')->ADXData
  
  data[,c(ADXData,"CCEcl"),drop=F]->datCC
  
  t(apply(datCC,1,function(x){
    
    
    unlist(stri_split_regex(x['CCEcl'],'[\\(\\)\\|]'))->dxDelete
    x[ADXData][which(x[ADXData]%in%dxDelete)]<-'NtAvb'
    return(x)
  }))->datCC
  as.data.table(datCC)->datCC
  
  data[,c(ADXData,"CCEcl")]<-datCC
  
  return(data)
}



