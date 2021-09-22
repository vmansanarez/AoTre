

######################################################################
##### Wrap for the GeneralMannKendall function

#'@param X data (vector). IMPORTANT: assumes X is regularly-spaced.
#'@param level real, level of the test (between 0 and 1, default 0.1)
#'@param dep.option dependency option, option for handling temporal dependence.
#'@param DoDetrending logical, only used for dep.option==LTP.
#' @return a dataframe, with the different statistiques and values of interest of the test.
#' @examples
#' GeneralMannKendall.wrap(X=data.Var$value)
#' @export
GeneralMannKendall.wrap=function(X,level=0.1,dep.option='INDE',DoDetrending=TRUE){
  res.test=GeneralMannKendall(X = X,level=level,dep.option=dep.option,DoDetrending=DoDetrending)
  data.tmp=data.frame(H=res.test$H,p=res.test$P,stat=res.test$STAT,trend=res.test$TREND,dep=res.test$DEP)
  list(data.tmp) ### Only trend interest us?
}

# OUT=list(pval=NA,stat=NA,xtra=NA)


######################################################################
##### Applies the GeneralMannKendall function to an extracted variable

#'@param data.extract data (vector). IMPORTANT: assumes X is regularly-spaced.
#'@param level real, level of the test (between 0 and 1, default 0.1)
#'@param dep.option dependency option, option for handling temporal dependence.
#'@param DoDetrending logical, only used for dep.option==LTP.
#' @return a dataframe, with the different statistiques and values of interest of the test.
#' @examples
#' GeneralMannKendall.wrap(X=data.Var$value)
#' @export
Estimate.stats=function(data.extract,funct.stat=GeneralMannKendall.wrap,list.stats=NULL,...){
  data.Y.extract=aggregate.data.frame(x=data.extract$value,
                                      by = list(group=data.extract$group),
                                      FUN = funct.stat,...)
  data.final=cbind.data.frame(group=data.Y.extract$group,
                              as.data.frame(t(matrix(unlist(data.Y.extract$x,use.names = TRUE),nrow=5))))

  colnames(data.final)=c("group","H","p","stat","trend","dep")

  data.final=data.final[,-c(2,6)]

  if(!is.null(list.stats)){
    data.final$station=list.stats
  }

  return(data.final)
}


