
######################################################################
#' @title  Cumulative frequency curve at a specific percentile
#' @description Function to compute the Cumulative frequency Curve of a time serie at
#'              a specific occurrence probability. In Hydrology, the cumulative frequency curve
#'              of a Discharge time serie is called "Flow Duration curve".
#' @param x a vector of real, value of the time series.
#' @param probs.FDC a real, between 0 and 1. Specific probability of occurence of the Cumulative
#'        frequency curve (default: "0.1).
#' @return a real, value corresponding to the Cumulative frequency curve at the probalbility probs.FDC
#' @examples
#' f_FDC_x(x=vect.Q)
#' f_FDC_x(x=vect.Q,probs.FDC=0.5)
#' read.timeSeries(datapath=datapath,ind.catch=c(1,9,14,5,80))
#' @export
f_FDC_x=function(x,probs.FDC=0.1){

  if(probs.FDC==0){
    percen.res = min(x,na.rm=FALSE)
  }else if(probs.FDC==1){
    percen.res = max(x,na.rm=FALSE)
  }else if((probs.FDC<0)||(probs.FDC>1)){
    stop("f_FDC_x: Wrong number provided for argument 'probs.FDC'!")
  }else{
    # Order data, NA values are set at the end
    x.ord.ind=order(x,decreasing = TRUE,na.last = TRUE)
    # Compute length of non Na values and
    n.end=length(x)-length(which(is.na(x[x.ord.ind])))
    # select only non Na value and accoiate empiricall probability of occurences.
    data.fdc=data.frame(x=x[x.ord.ind][1:n.end],probs=(1:n.end)/n.end)
    # Remove all duplicates to set the same proba of occurences to each duplicated value
    dt.plot.fdc=data.fdc[!duplicated(data.fdc[,"x"]),]


    ind.below=which(dt.plot.fdc$probs<probs.FDC)

    if(length(ind.below)==0){
      percen.res=dt.plot.fdc$x[1]
    }else if(length(ind.below)==nrow(dt.plot.fdc)){
      percen.res=dt.plot.fdc$x[nrow(dt.plot.fdc)]
    }else{
      percen.res=dt.plot.fdc$x[ind.below[length(ind.below)]+1]
    }
  }

  return(percen.res)
}


######################################################################
#' @title Volume deficit
#' @description Function to compute the variable "Volume deficit": sum of differences
## between the values of the time series and the cumulative frequency curve
## (probability of occurence against value) at the percentile 0.15.
#' @param x a vector of real, value of the time series.
#' @return a real, number corresponding to the value of the FDC at the probalbility probs.FDC
#' @examples
#' FDC_lowvol(x=vect.Q)
#' @export
FDC_lowvol=function(x){
  ## Volume deficit: difference between the actual flow and the FDC threshold
  # for value below
  FDC.threshold=f_FDC_x(x,probs=0.15)

  ind.below=which(x<FDC.threshold)
  diff.Flow_FDCthres=FDC.threshold-x[ind.below]
  res=sum(diff.Flow_FDCthres)
  return(res)
}


######################################################################
#' @title Extration of variable from a time serie
#' @description A specific variable is extracted from the data accordingly
#'              to a provided classification of groups.
#' @param data.station data object from the StatTrendAnalysis package, data from which to extract the variable of interest.
#' @param funct function, function to apply for extracting the variable from the value of the time serie.
#' @param timestep character, option on the type of aggregation on time to perform. Available:
#' \enumerate{
#'            \item 'year', variable is extracted for each year (default option)
#'            \item 'month', variable is extracted for each months
#'            \item 'None', no aggregation on time is considered
#'            }
#'        Any other value for this argument will return an error message.
#' @param period vector of character, date of start and of end of the period to considered in the data.
#'        Imposed date format is "YYYY-mm-dd". Default option (period = NULL) is to considered all the
#'        periods avalailable in the data.
#' @param per.start character, allow to index years/months accotdingly to per.start (default: "01-01")
#' @param ... arguments needed for the function provided through the argument "funct".
#' @return a list of two objects (data, a unique data.tibble containing all
#' the data grouped by file;; info, a data.tibble with the file and matching groups)
#' @examples
#' extract.Var(data.station=data)
#' extract.Var(data.station=data,funct=min)
#' extract.Var(data.station=data,funct=FDC_lowvol,period=c("1965-01-01","2020-01-01"))
#' extract.Var(data.station=data,funct=f_FDC_x,probs.FDC=0.15)
#' @export
extract.Var=function(data.station
                     ,funct=max
                     ,timestep="year"
                     ,period=NULL
                     ,per.start="01-01"
                     ,...){

  require(dplyr)
  require(lubridate)

  ### Check argument data.station

  # [EVOL]
  data.all=data.station$data

  ############################################
  ### Select period
  if(is.null(period)){
    ### Nothing to do, whole period is considerated
  }else{
    ### Check validity of period argument: vector of 2 character, format = "YYYY-mm-dd"
    # To be done

    ### Grouped the data by date and station
    data.all.tmp.grouped = data.all %>% group_by(group)

    ### Filter data: only the wanted period is kept
    # data.all.sel.group = filter(.data = data.all.tmp.grouped, datetime > period[1] & datetime < period[2])

    data.all.sel.group = filter(.data = data.all.tmp.grouped, datetime  <= period[2])

    data.all.time.filtered = filter(.data = data.all.sel.group, datetime  >= period[1])
    data.all=data.all.time.filtered

    ### Remove temporary object to free memory
    rm(data.all.time.filtered,data.all.sel.group,data.all.tmp.grouped)
  }

  ############################################
  ### Mutate function used to convert a column without adding a new one (avoid using temporary memory)

  ### Allow to do yearly, monthly grouping. Also allow to not compute a function per group of time
  if(timestep=="year"){

    if(per.start=="01-01"){
      ### Default year definition: from 1st of January to the 31st of December.
      data.all.grTime=mutate(data.all,datetime=factor(as.numeric(format(datetime, format="%Y"))))
    }else{
      ### Local function to index years accotdingly to per.start
      f.data.start.tmp=function(x.date,per.start){
        n.year=as.numeric(format(x.date, format="%Y"))
        start.date=as.Date(paste(n.year-1,per.start,sep="-"),format="%Y-%m-%d")
        end.date=as.Date(paste(n.year,per.start,sep="-"),format="%Y-%m-%d")-1
        if(between(x.date,start.date,end.date)){
          res.ind=n.year
        }else{
          res.ind=n.year+1
        }
        return(res.ind)
      }
      # datetime.mod=sapply(data.all$datetime,f.data.start.tmp,per.start=per.start)
      # datetime.tmp=factor(datetime.mod)
      # data.all.grTime.new=data.all
      # data.all.grTime.new$datetime=datetime.tmp
      # #Similar

      data.all.grTime=mutate(data.all,datetime=factor(sapply(datetime,f.data.start.tmp,per.start=per.start)))
    }

    # data.all.grTime=data.all %>% mutate(datetime=factor(as.numeric(format(datetime, format="%Y"))))
  }else if(timestep=="month"){


    if(per.start=="01"){
      ### Default year definition: from 1st of January to the 31st of December.
      data.all.grTime=mutate(data.all,datetime=factor(as.numeric(format(datetime, format="%m"))))
    }else{
      ### Local function to index years accotdingly to per.start
      f.data.start.tmp2=function(x.date,per.start){
        n.year=as.numeric(format(x.date, format="%Y"))
        n.month=as.numeric(format(x.date, format="%m"))
        start.date=as.Date(paste(n.year,sprintf("%02d", n.month),per.start,sep="-"),format="%Y-%m-%d") %m+% months(-1)
        end.date=as.Date(paste(n.year,sprintf("%02d", n.month),per.start,sep="-"),format="%Y-%m-%d")-1
        if(between(x.date,start.date,end.date)){
          res.ind=format(x.date, format="%Y-%m")
        }else{
          res.ind=format(as.Date(x.date) %m+% months(1),format="%Y-%m")
        }
        return(res.ind)
      }
      # datetime.mod=sapply(data.all$datetime,f.data.start.tmp,per.start=per.start)
      # datetime.tmp=factor(datetime.mod)
      # data.all.grTime.new=data.all
      # data.all.grTime.new$datetime=datetime.tmp
      # #Similar

      data.all.grTime=mutate(data.all,datetime=factor(sapply(datetime,f.data.start.tmp2,per.start=per.start)))
    }

  }else if(timestep=="Station"){
    ### Only grouping by stations
    data.all$datetime=factor(rep(1,nrow(data.all)))
    data.all.grTime=select(data.all,datetime,value,group)
  }else if(timestep=="None"){
    ### No grouping by date
    data.all$datetime=factor(rep(1:nrow(data.all)))
    data.all.grTime=select(data.all,datetime,value,group)
  }else{
    stop("extract.Var: wrong timestep argument value")
  }

  ############################################
  ### Handling missing data. If too much NA values per yera, year is set to NA.
  # View(data.all.grTime %>%
  #   filter(is.na(value))  %>% group_by(datetime,group) %>% count(group))
  #
  # data.count.step1=filter(.data = data.all.grTime,.preserve = !is.na(value))
  # data.count.step2=data.count.step1 %>% group_by(datetime,group)
  # data.count.step3=count(x = data.count.step2,wt=NULL)
  #
  # days.need=as.integer((1-percent.missing)*365.25)
  #
  # data.count.step4=filter(.data = data.count.step3,n>=days.need)
  #

  ### Group data
  data.extract.step1= group_by(.data=data.all.grTime,datetime,group)

  ### Apply function of interest
  data.extract=summarise_all(.tbl = data.extract.step1,.funs = funct,...)

  return(data.extract)
}
