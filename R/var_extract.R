

######################################################################
#' @title Extraction of variable from a time series
#' @description A specific variable is extracted from the data accordingly
#'              to a provided classification of groups.
#' @param data.station data object from the StatAnalysisTrend package, data from
#' which to extract the variable of interest. If provided, take over argument
#' 'data.group' and 'data.values'.
#' @param data.group data.frame or tbl, group(s) to consider in the extraction.
#' @param data.values vector or dataframe/tbl of 1 column, values associated
#' with the provided groups in argument \code{data.group}.
#' @param funct function, function to apply for extracting the variable from the
#' value of the time series.
#' @param timestep character, option on the type of aggregation on time to
#' perform. Available:
#' \enumerate{
#'            \item 'year', variable is extracted for each year (default option)
#'            \item 'year-month', variable is extracted for each months and years
#'            \item 'month', variable is extracted for each months between the years
#'            \item 'station', variable is extracted for each group
#'            \item 'None', no aggregation on time is considered
#'            }
#'        Any other value for this argument will return an error message.
#' @param period vector of character, date of start and of end of the period to
#' considered in the data. Imposed date format is "YYYY-mm-dd". Default option
#' (period = NULL) is to considered all the periods available in the data.
#' @param per.start character, allow to index years/months accordingly to
#' per.start (default: "01-01").
#' @param pos.datetime integer, column number of the Date object in the provided
#' groups. NA by default: No group of date provided.
#' @param formatDate character string, format of the date column in the data
#' @param ... arguments needed for the function provided through the argument
#' \code{funct}.
#' @return a list of two objects (data, a unique data.tibble containing all the
#' data grouped by file;; info, a data.tibble with the file and matching groups)
#' @details If there is a group of Date, argument 'pos.datetime" needs to be
#' provided. If not, it is assume that there are no Date object in the provided
#' groups.
#' @examples
#' library(StatsAnalysisTrend)
#' data(StationQ_3catch)
#' extract.Var(data.station=StationQ_3catch,pos.datetime=1)
#' extract.Var(data.station=StationQ_3catch,funct=min,pos.datetime=1)
#' @importFrom lubridate %m+% %m-%
#' @export extract.Var
extract.Var=function(data.station = NULL # data already prepared.
                     ,data.group = NULL # Data with the different groups.
                     ,data.values = NULL # Data with the values for each
                     # respective groups (hyp. Same index).
                     ,funct=max
                     ,timestep="year"
                     ,period=NULL
                     ,per.start="01-01"
                     ### settings if working with datetime
                     ,pos.datetime=NA
                     ,formatDate="%Y-%m-%d"
                     ,...){
  ############################################
  ############################################
  ############################################

  #* ------------------------------------------------------------------------ *
  #* ---- ---- ---- ---- ---- STEP 0: Argument check ---- ---- ---- ---- ---- *
  #* ------------------------------------------------------------------------ *
  ### Check argument of the function
  # WORK IN PROGRESS
  ############################################
  # Either data already joined or provided, group and value separately

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
    if(!dplyr::is.tbl(data.group)){
      ## group.d is data.frame, combining with val will give a data.frame
      tmp.dataframe=dplyr::bind_cols(data.group,data.values)
      data.all=dplyr::tibble(tmp.dataframe)
      rm(tmp.dataframe) ### remove the tmp object from memory
    }else{
      data.all=dplyr::bind_cols(data.group,data.values)
    }
    if(is.na(pos.datetime)){
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

  ### check if datetime is in Date format or Character
  # Needed because of the permutation on the start of the year/time later on.
  # Maybe do it if ther is a permutation?
  if(!is.na(pos.datetime)){
    ## Forcing the name of the column of time to "datetime"
    colnames(data.all)[pos.datetime]="datetime"
    ## Which class?
    date.class=class(data.all$datetime)
    ## Conversion to Date format if it is not.
    if(date.class == "character"){
      if(formatDate == "%Y-%m-%d"){
        data.all=dplyr::mutate(.data=data.all,
                               datetime=as.Date(x = datetime,
                                                format=formatDate))
      }else if(formatDate == "%Y-%m"){
        data.all=dplyr::mutate(.data=data.all,
                               datetime=as.Date(x = paste(datetime,"01",sep="-"),
                                                format="%Y-%m-%d"))
      }else if(formatDate == "%Y"){
        data.all=dplyr::mutate(.data=data.all,
                               datetime=as.Date(x = paste(datetime,"01","01",
                                                          sep="-"),
                                                format="%Y-%m-%d"))
      }else{
        stop("ARGUMENT ERROR:'extract.Var': wrong 'formatDate' argument!")
      }
    }else if(date.class == "Date"){
      ## Correct format, nothing to do.
    }else{
      ## Wrong format, neither Date or character
      stop("ARGUMENT ERROR:'extract.Var': wrong format for the column with dates!")
    }
  }


  #* ------------------------------------------------------------------------ *
  #* ---- ---- ---- ---- --- STEP 1: Preprocess Dates ---- ---- ---- ---- --- *
  #* ------------------------------------------------------------------------ *

  ############################################
  ### STEP 1.1 Select period, first time: rough selection to cute the data
  # Less computing time.
  ############################################
  if(is.null(period)){
    ### Nothing to do, whole period is considered
    ### Processing: select the oldest time??
  }else{
    ### Check validity of period argument: vector of 2 character, format =
    # "YYYY-mm-dd"
    # To be done?

    period.date=as.Date(period,format = formatDate)

    ### Grouped the data by all columns that are factor, = the "group" columns
    data.all.tmp.grouped = dplyr::group_by(.data = data.all,
                                           dplyr::across(where(is.factor)))

    #####################
    ### Data are cut accordingly to the provided period.
    # first trimmed is rough
    lim.period.lowRough=period.date[1] %m-% lubridate::years(1)
    lim.period.upRough=period.date[2] %m+% lubridate::years(1)

    ### Filter data: only the wanted period is kept
    data.all.sel.group = dplyr::filter(.data = data.all.tmp.grouped,
                                       datetime  <= lim.period.upRough)

    data.all.time.filtered = dplyr::filter(.data = data.all.sel.group,
                                           datetime  >= lim.period.lowRough)
    data.all=data.all.time.filtered

    ### Remove temporary object to free memory [necessary?]
    rm(data.all.time.filtered,data.all.sel.group,data.all.tmp.grouped)
  }

  ############################################
  ### Mutate function used to convert a column without adding a new one (avoid
  # using temporary memory)


  ############################################
  ### STEP 1.2 Set the start of the period
  ############################################


  ### timestep option allowed
  timeStep.allowed=c("year","year-month","month","station","none")
  ## year --> time is grouped by year, so as many groups as there are /= years
  ## month --> time is grouped by months, so 12 groups
  ## year-month --> time is grouped by year*month, so 12*years groups
  ## station --> no work on time: data are group by station (for example, smoothing of values)
  ## none --> data are not grouped.


  change.startFunct=function(opt_time
                             ,val_start
                             ,opt_TimeNeeded){
    res.change=list(change=FALSE,StartParam=NULL)
    if(opt_time %in% opt_TimeNeeded){
      ### options that changing start period makes sense
      if(opt_time==opt_TimeNeeded[1]){
        ## Data are aggregated by Years
        if(val_start != "01-01"){
          ### Catch month and day of new start of the year
          val_start.month=as.numeric(substr(val_start,1,2))
          val_start.day=as.numeric(substr(val_start,4,5))
          ### Save it in returned result
          res.change$change=TRUE
          res.change$StartParam=c(val_start.month,val_start.day)
        }else{
          res.change$StartParam=c(01,01)
        }
      }else if(opt_time==opt_TimeNeeded[2]){
        ## Data are aggregated by Year*month
        if(val_start != "01"){
          ### Catch day of new start of the year
          val_start.day=as.numeric(val_start)
          ### Save it in returned result
          res.change$change=TRUE
          res.change$StartParam=val_start.day
        }else{
          res.change$StartParam=1
        }
      }else{
        ## Data are aggregated by month
        if(val_start != "01"){
          ### Catch day of new start of the year
          val_start.day=as.numeric(val_start)
          ### Save it in returned result
          res.change$change=TRUE
          res.change$StartParam=val_start.day
        }else{
          res.change$StartParam=1
        }
      }
    }
    return(res.change)
  }



  Settings_changeStart=change.startFunct(opt_time=timestep
                                         ,val_start=per.start
                                         ,opt_TimeNeeded=timeStep.allowed[1:3])

  if(Settings_changeStart$change){
    ### Start of the "period" need to be changed

    ### I used a if stucture. Following if structure could be changed to a more
    # generic equation but need to be tested in terms of computing time/memory
    if(timestep == "year"){
      m.change=Settings_changeStart$StartParam[1]
      d.change=Settings_changeStart$StartParam[2]

      ## Translate start of the year
      data.extract.step1=dplyr::mutate(.data = data.all,
                                       datetime=datetime %m-% months(m.change-1) - (d.change-1))
      ## Change datetime into year
      data.extract.step2=dplyr::mutate(.data = data.extract.step1,
                                       datetime=lubridate::year(datetime))

      ## group variable by all except column "values"
      data.all.grTime=dplyr::group_by_at(.tbl=data.extract.step2
                                         ,.vars = dplyr::setdiff(names(data.extract.step2),
                                                                 "values"))
      if(!is.null(period)){
        ### Refined period selection
        lim.period.low=lubridate::year(period.date[1])
        lim.period.up=lubridate::year(period.date[2])
        data.all.grTime = dplyr::filter(.data = data.all.grTime,
                                        datetime  <= lim.period.up)

        data.all.grTime = dplyr::filter(.data = data.all.grTime
                                        ,datetime  >= lim.period.low)

      }

    }else if(timestep == "month"){
      d.change=Settings_changeStart$StartParam[1]

      ## Translate start of the year
      data.extract.step1=dplyr::mutate(.data = data.all,
                                       datetime=datetime - (d.change-1))


      ### Refined period selection
      if(!is.null(period)){
        data.extract.step1 = dplyr::filter(.data = data.extract.step1,
                                           datetime  <= period.date[2])
        data.extract.step1 = dplyr::filter(.data = data.extract.step1,
                                           datetime  >= period.date[1])
      }
      ## Change datetime into year
      data.extract.step2=dplyr::mutate(.data = data.extract.step1,
                                       datetime=lubridate::month(datetime))

      ## group variable by all except column "values"
      data.all.grTime=dplyr::group_by_at(.tbl=data.extract.step2
                                         ,.vars = dplyr::setdiff(names(data.all.grTime),
                                                                 "values"))

    }else if(timestep == "year-month"){
      d.change=Settings_changeStart$StartParam[1]

      ## Translate start of the year
      data.extract.step1=dplyr::mutate(.data = data.all,
                                       datetime=datetime - (d.change-1))


      ### Refined period selection
      if(!is.null(period)){
        data.extract.step1 = dplyr::filter(.data = data.extract.step1,
                                           datetime  <= period.date[2])
        data.extract.step1 = dplyr::filter(.data = data.extract.step1,
                                           datetime  >= period.date[1])
      }
      ## Change datetime into year
      # faster than using format(). By 30%.
      # faster than using substr(). by 95%
      # Maybe there is a lubridate function for getting %Y-%m?
      # data.extract.step2=dplyr::mutate(.data = data.extract.step1,
      #                                  datetime=paste(lubridate::year(datetime)
      #                                                 ,lubridate::month(datetime)
      #                                                 ,sep="-"))
      ### Test 1200% faster
      data.extract.step2=dplyr::mutate(.data = data.extract.step1
                                       ,datetime.year=lubridate::year(datetime)
                                       ,datetime.month=lubridate::month(datetime))
      data.extract.step2=dplyr::select(.data=data.extract.step2
                                       ,dplyr::setdiff(names(data.extract.step2),
                                                       "datetime"))

      ## group variable by all except column "values"
      data.all.grTime=dplyr::group_by_at(.tbl=data.extract.step2
                                         ,.vars = dplyr::setdiff(names(data.extract.step2),
                                                                 "values"))

    }else{

    }
  }

  ####### UNFINISHED


  ### No date columns
  if(timestep=="station"){
    ### Only grouping by stations: n.group==1
    n.group=ncol(data.all)-1
    if(n.group==1){
      data.all.grTime=data.all
    }else{
      stop("extract.Var: ERROR, 'timestep = Station': only one group needed.")
    }
  }else if(timestep=="None"){
    ### No grouping
    data.all.grTime=dplyr::select(data.all,values)
  }else if(timestep %in% timeStep.allowed[1:3]){

    if(!Settings_changeStart$change){
      # dates were not aggregated before
      if(timestep == "year"){
        ## Change datetime into year
        data.extract.step2=dplyr::mutate(.data = data.all,
                                         datetime=lubridate::year(datetime))

        ## group variable by all except column "values"
        data.all.grTime=dplyr::group_by_at(.tbl=data.extract.step2
                                           ,.vars = dplyr::setdiff(names(data.extract.step2),
                                                                   "values"))

      }else if(timestep == "month"){

        ### Refined period selection
        if(!is.null(period)){
          data.all = dplyr::filter(.data = data.all,
                                   datetime  <= period.date[2])
          data.all = dplyr::filter(.data = data.all,
                                   datetime  >= period.date[1])
        }
        ## Change datetime into year
        data.extract.step2=dplyr::mutate(.data = data.all,
                                         datetime=lubridate::month(datetime))

        ## group variable by all except column "values"
        data.all.grTime=dplyr::group_by_at(.tbl=data.extract.step2
                                           ,.vars = dplyr::setdiff(names(data.all.grTime),
                                                                   "values"))

      }else if(timestep == "year-month"){

        ### Refined period selection
        if(!is.null(period)){
          data.all = dplyr::filter(.data = data.all,
                                   datetime  <= period.date[2])
          data.all = dplyr::filter(.data = data.all,
                                   datetime  >= period.date[1])
        }
        ### Test 1200% faster
        data.extract.step2=dplyr::mutate(.data = data.all
                                         ,datetime.year=lubridate::year(datetime)
                                         ,datetime.month=lubridate::month(datetime)
                                         ,datetime = paste(datetime.year,datetime.month,sep="-"))

        data.extract.step2=dplyr::select(.data=data.extract.step2
                                         ,names(data.all))

        ## group variable by all except column "values"
        data.all.grTime=dplyr::group_by_at(.tbl=data.extract.step2
                                           ,.vars = dplyr::setdiff(names(data.extract.step2),
                                                                   "values"))

      }else{

      }
    }

    ## Already dealt with above
    if(!exists("data.all.grTime")){
      data.all.grTime=data.all
    }

  }else{
    stop("extract.Var: wrong timestep argument value")
  }

  #* ------------------------------------------------------------------------ *
  #* ---- ---- ---- ---- ---- - STEP 2: GROUP DATA ---- ---- ---- ---- ---- - *
  #* ------------------------------------------------------------------------ *

  ### Group data by all columns except column of values (setdiff function remove
  # the column 'values' from the list of names of the columns to group)
  data.extract.step1= dplyr::group_by_at(.tbl=data.all.grTime,
                                         .vars = dplyr::setdiff(names(data.all.grTime),
                                                                "values"))

  #* ------------------------------------------------------------------------ *
  #* ---- ---- ---- ---- STEP 3: COMPUTE NA % per groups. ---- ---- ---- ---- *
  #* ------------------------------------------------------------------------ *

  count_NA=function(x){
    res=sum(is.na(x))/length(x)
    return(res)
  }

  #* ------------------------------------------------------------------------ *
  #* ---- ---- ---- ---- --- STEP 4: EXTRACT VARIABLE ---- ---- ---- ---- --- *
  #* ------------------------------------------------------------------------ *

  #### wrap of provided function
  funct_wrap=function(x_vect){
    funct(x_vect,...)
  }
  ### Apply function of interest + compute percent NA
  data.extract.var=dplyr::summarise_all(.tbl = data.extract.step1
                                        ,.funs = funct_wrap)

  data.extract.NA=dplyr::summarise_all(.tbl = data.extract.step1
                                       ,.funs = count_NA)

  data.extract.var$Na.percent=data.extract.NA$values

  data.extract.var= dplyr::group_by_at(.tbl=data.all.grTime,
                                       .vars = dplyr::setdiff(names(data.all.grTime),
                                                              c("values","Na.percent")))
  ### Replace -Inf and Inf values by NA. Some primitive function (like max())
  # return -Inf (or Inf for min function) when all data are NA.
  data.extractFinal = dplyr::mutate(.data = data.extract.var,
                                    values = replace(values, is.infinite(values), NA))



  return(data.extractFinal)

}


