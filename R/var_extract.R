
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
#' vect.Q= exp(seq(from = 0.01,to = 2,by = 0.01))
#' f_FDC_x(x=vect.Q)
#' f_FDC_x(x=vect.Q,probs.FDC=0.5)
#' f_FDC_x(x=vect.Q,probs.FDC=1)
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
#' vect.Q= exp(seq(from = 0.01,to = 2,by = 0.01))
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
#' @param data.station data object from the StatAnalysisTrend package, data from which to extract the variable of interest.
#'        If provided, take over argument 'data.group' and 'data.values'.
#' @param data.group data.frame or tbl, group(s) to consider in the extraction.
#' @param data.values vector or dataframe/tbl of 1 column, values associated with the provided groups in argument'data.group".
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
#' @param per.start character, allow to index years/months accotdingly to per.start (default: "01-01").
#' @param pos.datetime integer, column number of the Date object in the provided groups. NA by default: No group of date provided.
#' @param ... arguments needed for the function provided through the argument "funct".
#' @return a list of two objects (data, a unique data.tibble containing all
#' the data grouped by file;; info, a data.tibble with the file and matching groups)
#' @details If there is a group of Date, argument 'pos.datetime" needs to be provided.
#'        If not, it is assume that there are no Date object in the provided groups.
#' @examples
#' extract.Var(data.station=data)
#' extract.Var(data.station=data,funct=min)
#' extract.Var(data.station=data,funct=FDC_lowvol,period=c("1965-01-01","2020-01-01"))
#' extract.Var(data.station=data,funct=f_FDC_x,probs.FDC=0.15)
#' @export
extract.Var=function(data.station = NULL ## data already prepared. Assumed: last column is the value and previous ones are the groups
                     ,data.group = NULL ## Data with the different groups
                     ,data.values = NULL ## Data with the corresponding values for each groups (hyp. Same index)
                     ,funct=max,timestep="year"
                     ,period=NULL
                     ,per.start="01-01"
                     ### settings if working with datetime
                     ,pos.datetime=NA
                     ,...){

  require(dplyr)
  require(lubridate)

  ############################################
  ############################################
  ############################################
  ### Check argument data.station
  # WORK IN PROGRESS
  ############################################
  # Either data already joined or provid, group and value separately

  if(is.null(data.group) && is.null(data.values)){
    if(is.null(data.station)){
      stop("ARGUMENT ERROR:'extract.Var': No data provided")
    }else{
      ### Data are already prepared
      data.all=data.station$data
      n.group=ncol(data.all)-1
      if(is.na(pos.datetime)){
        ### No Date object in the group
        colnames(data.all)=c(paste0("group",1:n.group),"values")
      }else{
        ### One of the group is Dates, will be called date
        if(n.group==1){
          ### Special case: only one group and it is date
          colnames(data.all)=c("datetime","values")
        }else{
          ### More than one group
          vec.name=rep("",n.group)
          vec.name[pos.datetime]="datetime"
          vec.name[-pos.datetime]=paste0("group",1:(n.group-1))
          colnames(data.all)=c(vec.name,"values")
        }
      }
    }
  }else if (is.null(data.group) && !is.null(data.values)){
    stop("ARGUMENT ERROR:'extract.Var': Missing groups!")
  }else if (!is.null(data.group) && is.null(data.values)){
    stop("ARGUMENT ERROR:'extract.Var': Missing values!")
  }else{
    ### group and values were provided
    if(nrow(data.group)!=nrow(data.values)){
      stop("ARGUMENT ERROR:'extract.Var': different number of row in inputs!")
    }
    if(!is.tbl(data.group)){
      ## group.d is data.frame, combining with val will give a data.frame
      tmp.dataframe=bind_cols(data.group,data.values)
      data.all=tibble(tmp.dataframe)
      rm(tmp.dataframe) ### remove the tmp object from memory
    }else{
      data.all=bind_cols(data.group,data.values)
    }
    if(!is.na(pos.datetime)){
      ### No date object in the group
      colnames(data.all)=c(paste0("group",1:ncol(data.group)),"values")
    }else{
      # a datetime object was provided in the group at position 'pos.datetime",
      # change column name to "datetime"
      n.group=ncol(data.group)
      vec.name=rep("",n.group)
      vec.name[pos.datetime]="datetime"
      vec.name[-pos.datetime]=paste0("group",1:(n.group-1))
      colnames(data.all)=c(vec.name,"values")
    }

  }

  ############################################
  ### Select period
  if(is.null(period)){
    ### Nothing to do, whole period is considered
  }else{
    ### Check validity of period argument: vector of 2 character, format = "YYYY-mm-dd"
    # To be done

    ### Grouped the data by all columns that are factor, id est the "group" columns
    data.all.tmp.grouped = data.all %>% group_by(across(where(is.factor)))

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
    ### Only grouping by stations: n.group==1
    n.group=ncol(data.all)-1
    if(n.group==1){
      data.all.grTime=data.all
    }else{
      stop("extract.Var: error with option 'Station' of argument 'timestep': only one group needed in data.")
    }
  }else if(timestep=="None"){
    ### No grouping
    data.all.grTime=select(data.all,values)
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
  ## [To be done]

  ### Group data by all columns except column of values (setdiff function remove the column 'values' from
  # the list of names of the columns to group)
  data.extract.step1= group_by_at(.tbl=data.all.grTime,.vars = setdiff(names(data.all.grTime), "values"))

  ### Apply function of interest
  data.extract=summarise_all(.tbl = data.extract.step1,.funs = funct,...)

  ### Replace -Inf and Inf values by NA. Some primitive function (like max()) return -Inf (or Inf for min function)
  # when all data are NA.
  data.extract = mutate(.data = data.extract, values = replace(values, is.infinite(values), NA))

  return(data.extract)
}
