

######################################################################
#' @title Load and process data
#' @description Read data from specified files and orgnaize it in the proper format for variable extraction.
#' @param datapath vector of character, path of the files to load.
#' @param ind.catch vector of integer, index of station to keep (default: NULL, all files are considered).
#' @param NA.code vector of strings, codes of the NA values in the files to be read (default: "-99.000").
#' @param col.date vector of integer, column number for year, month, day, if length == 1,
#' it is already a date format.
#' @return a list of two objects (data, a unique data.tibble containing all
#' the data grouped by file; info, a data.tibble with the file and matching groups)
#' @examples
#' read.timeSeries(datapath=datapath)
#' read.timeSeries(datapath=datapath,ind.catch=1:51)
#' read.timeSeries(datapath=datapath,ind.catch=c(1,9,14,5,80))
#' read.timeSeries(datapath=datapath,,NA.code="-9999")
#' read.timeSeries(datapath=datapath,,NA.code=c("-9999","-1"))
#' @export
readFiles.TS=function(datapath,ind.catch=NULL,NA.code="-99.000",col.date=c(1,2,3)){

  require(dplyr)

  ### Checking that datapath is a vector of character
  if(!is.vector(datapath)){
    stop("read.timeSeries: argument datapath is not a vector")
  }else{
    ### It is a vector, checking the class of the first iteration is only needed.
    if(!is.character(datapath[1])){
      stop("read.timeSeries: wong class for argument datapath")
    }
    ### Check if missing filenames (NA)?
    #[EVOL]
  }

  ### Selection of files?
  # [EVOL]: check ind.catch
  if(is.null(ind.catch)){
    indices.catchment=1:length(datapath)
  }else{
    indices.catchment=ind.catch
  }

  # represent the station number: link with data.info
  k_group=1

  # Blank object
  data.val=data.frame(datetime="",value=NA,group=NA)
  data.val=data.val[FALSE,]

  ##### Read first file: it initialise the table
  i_file=indices.catchment[1]
  ### Path of the current catchment/station
  path.data.current=datapath[i_file]

  ### Check if hearders
  header.var=FALSE
  data.headers=read.table(file = path.data.current,nrows=1)
  if(is.character(data.headers[1,1])){
    header.var=TRUE
  }

  if(is.null(NA.code)){
    data.raw=read.table(file = path.data.current,skip = 1*header.var)
  }else{
    ### Read current data
    data.raw=read.table(file = path.data.current,skip = 1*header.var,na.strings = NA.code)
  }
  ### Time processing
  ## Assumption that same time over different stations?
  tmp.time=paste(data.raw$V1,data.raw$V2,data.raw$V3,sep="-")
  day.time=as.Date(tmp.time,format = "%Y-%m-%d", origin = "1970-01-01")
  # Date format was chosen.

  ### Store current data in dataframe
  data.val.tmp=data.frame(datetime=day.time,
                          group=rep(k_group,nrow(data.raw)),
                          value=data.raw$V4)
  data.val=as_tibble(data.val.tmp)

  k_group=2
  if(length(indices.catchment)==1){
    ### All data were read
  }else{
    ### Loop of the remaining catchement/Stations
    for(i_file in indices.catchment[-1]){
      ### Path of the current catchment/station
      path.data.current=datapath[i_file]

      ### Check if hearders
      header.var=FALSE
      data.headers=read.table(file = path.data.current,nrows=1)
      if(is.character(data.headers[1,1])){
        header.var=TRUE
      }

      if(is.null(NA.code)){
        data.raw=read.table(file = path.data.current,skip = 1*header.var)
      }else{
        ### Read current data
        data.raw=read.table(file = path.data.current,skip = 1*header.var,na.strings = NA.code)
      }
      ### Time processing
      ## Assumption that same time over different stations?
      tmp.time=paste(data.raw$V1,data.raw$V2,data.raw$V3,sep="-")
      day.time=as.Date(tmp.time,format = "%Y-%m-%d", origin = "1970-01-01")
      # Date format was chosen.

      ### Store current data in dataframe
      data.val.tmp=data.frame(datetime=day.time,
                              group=rep(k_group,nrow(data.raw)),
                              value=data.raw$V4)

      #### rbind data
      data.val=bind_rows(data.val,as_tibble(data.val.tmp))

      # represent the station number
      k_group=k_group+1
    }

  }

  ### transform the column group into a factor
  data.val=mutate_at(.tbl = data.val,.vars = vars(group), .funs = factor)

  ### Informations: group associate with the path of the file
  data.info=as_tibble(data.frame(group=factor(1:(k_group-1)),filename=datapath))

  ### Function read.timeSeries returns a list:
  # - Element 1) data of the stations
  # - Element 2) information of the station
  return(list(data=data.val,info=data.info))

}











