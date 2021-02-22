.onLoad <- function(lib, pkg){
  #automatically loads the dataset when package is loaded
  #do not use this in combination with lazydata=true
  
  utils::data(lstRules, package = pkg, envir = parent.env(environment()))
  utils::data(lstRules2, package = pkg, envir = parent.env(environment()))
  utils::data(ADRGVarsLst, package = pkg, envir = parent.env(environment()))
  
  utils::data(CCEclCHS, package = pkg, envir = parent.env(environment()))
  
  utils::data(CCStatusCHS, package = pkg, envir = parent.env(environment()))

  utils::data(CCMat, package = pkg, envir = parent.env(environment()))
  utils::data(dipDel, package = pkg, envir = parent.env(environment()))
  
  
}
