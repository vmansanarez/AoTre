

######################################################################
#' @title Load and process data
#' @description Read data from specified files and orgnaize it in the proper format for variable extraction.
#' @param datapath vector of character, path of the files to load.
#' @param ind.catch vector of integer, index of files to read. By default (`ind.catch = NULL`), all files are read.
#' @param NA.code vector of strings, codes of the NA values in the files to be read. Default value is `NA.code = "-99.000"`.
#' @param sep.file character, the field separator character. For "white space" use `sep.file = ""`.
#' See argument [sep] of [utils::read.table()] for more information.
#' @param col.date vector of integer, the column number for year, month, day. If `length(col.date) == 1`,
#' the column at the index `col.date` contains the year, the month and the day.
#' @param col.val integer, the column number of the values to read in the files. By default (`col.val = 4`),
#' the 4th column is considered as the column of values.
#' @param col.extra vector of integer, the column number of the extra column to read. By default, no extra colmuns
#' are read (`col.extra = NA`).
#' @param header.files logical, indicate wheter the files contains or not the name of the variable as its first line.
#' See argument [header] of [utils::read.table()] for more information.
#' @param skip.lines integer, number of line to skip before reading the data in the file. See argument [skip] of
#' [utils::read.table()] for more information.
#' @param formatDate character string, format of the Date in the files. Only used when [col.date] has been specified and
#' `length(col.date) == 1`. Specify the format of the date in the data. More details are available in description of
#' argument [format] of [base::as.Date()].
#' @param origin character string, wrap of argument [origin] of [base::as.Date()].
#' @param coord.get logical, indicate either or not some information contained in the files needs to be specifically
#' read and saved.
#' @param coord.skip.lines integer. Only used if [coord.get] is `TRUE`. It indicates the number of lines to skip at the
#' start of the file before reading the specific information.
#' @param coord.col vector of integer of length 4. Only used if [coord.get] is `TRUE`. The function will read
#' line `coord.skip.lines+1` (character separator is white space). Only columns with numeric values will be kept and
#' among it only the 4 columns specified in [coord.col].
#' @return a list of two objects (data, a unique data.tibble containing all
#' the data grouped by file; info, a data.tibble with the file and matching groups)
#' @examples
#' read.timeSeries(datapath=datapath)
#' read.timeSeries(datapath=datapath,ind.catch=1:51)
#' read.timeSeries(datapath=datapath,ind.catch=c(1,9,14,5,80))
#' read.timeSeries(datapath=datapath,,NA.code="-9999")
#' read.timeSeries(datapath=datapath,,NA.code=c("-9999","-1"))
#' @export
readFiles.TS=function (datapath,
                       ind.catch = NULL,
                       NA.code = "-99.000",
                       sep.file = "",
                       col.date = c(1, 2, 3),
                       col.val = 4,
                       col.extra = NA,
                       header.files = FALSE,
                       skip.lines = 1,
                       formatDate = "%Y-%m-%d",
                       origin="1970-01-01",
                       coord.get=FALSE,
                       coord.skip.lines=17,
                       coord.col=1:4){

  ### CHECK of arguments
  # argument datapath: character or vector of character.
  if (!is.vector(datapath)){
    stop("readFiles.TS: argument datapath is not a vector")
  }else{
    if (!is.character(datapath[1])) {
      stop("readFiles.TS: wong class for argument datapath")
    }
  }
  # argument col.date: NA, integer (1 column of date) or vector of 3 integers (Year, month, day in that order)
  if(length(col.date)==1){
    # NA or integer (1 column of date)
    if(is.na(col.date)){
      # No date provided
      date.col=FALSE
    }else if(col.date%%1==0){ ## x%%1 give the fractional part of x, if ==0, it is an integer (in mathematical sense, not R sense)
      # One integer => col.date is the index of the Date column in data
      date.col=TRUE
    }else{
      stop("readFiles.TS: wrong argument col.date")
    }
  }else{
    if(is.vector(col.date)){
      ### Check if all provided numbers are integers (in mathematical sense, not R sense)
      ### Check if only NA values
      if(all(is.na(col.date))){
        ## vector provided for col.date but only NAs. If no date columns, col.date=NA
        stop("readFiles.TS: wrong argument col.date")
      }else{
        if(any(is.character(col.date))){
          ### ERROR: vector of character provided
          stop("readFiles.TS: wrong argument col.date")
        }
        is.MathInteger=col.date%%1==0 ## (x%%1 is x modulo 1)
        if(any(!is.MathInteger,na.rm = TRUE)){
          ### ERROR: provided vector has a real number
          stop("readFiles.TS: wrong argument col.date")
        }
        date.col=TRUE
      }
    }else{
      stop("readFiles.TS: wrong argument col.date")
    }
  }
  # argument col.extra: NA / numeric or vector of integer/numeric
  # [CHECK to be done?]

  ### Read of files or only few ones?
  if (is.null(ind.catch)) {
    # All files
    indices.catchment = 1:length(datapath)
  }else{
    # read only the ones asked (argument ind.catch)
    indices.catchment = ind.catch
  }

  ### Read first file
  k_group = 1
  ## Set structure to store data
  # It is assumed that data have a date
  if(!is.na(col.extra)){
    if(date.col){
      data.val = data.frame(datetime = "",
                            value = NA,
                            group = NA,
                            t(rep(x = NA,length=length(col.extra))))
    }else{
      data.val = data.frame(value = NA,
                            group = NA,
                            t(rep(x = NA,length=length(col.extra))))
    }
  }else{
    if(date.col){
      data.val = data.frame(datetime = "",
                            value = NA,
                            group = NA)
    }else{
      data.val = data.frame(value = NA,
                            group = NA)
    }
  }
  ## Empty structure
  data.val = data.val[FALSE,]

  ### Read firs file
  i_file = indices.catchment[1]
  path.data.current = datapath[i_file]
  if(is.null(NA.code)){
    data.raw = read.table(file = path.data.current,
                          header = header.files,
                          sep = sep.file,
                          skip = skip.lines)
  }else{
    data.raw = read.table(file = path.data.current,
                          header = header.files,
                          na.strings = NA.code,
                          sep = sep.file,
                          skip = skip.lines)
  }

  if(date.col){
    if(length(col.date)==1){
      ### Date is already collapsed in 1 column in the files.
      tmp.time = data.raw[, col.date]
      day.time = as.Date(as.character(tmp.time),
                         format = formatDate,
                         origin = origin,
                         tz=tz)
    }else{
      ### In the files, date is dispatched among several columns.
      # col.date is the index of the column representing Years, Months and days in that order.
      # We assumed that Year, Months and Days are written in the files
      tmp.time = apply(X = data.raw[,col.date],
                       MARGIN = 1,
                       FUN = paste,
                       collapse = "-")
      day.time = as.Date(as.character(tmp.time),
                         format = "%Y-%m-%d",
                         origin = origin,
                         tz=tz)
    }
    if(any(!is.na(col.extra))){
      ## Add extra column(s)
      data.val.tmp = data.frame(datetime = day.time,
                                group = rep(k_group,nrow(data.raw)),
                                value = data.raw[, col.val],
                                data.raw[, col.extra])
    }else{
      ## No extra columns
      data.val.tmp = data.frame(datetime = day.time,
                                group = rep(k_group,nrow(data.raw)),
                                value = data.raw[, col.val])
    }
  }else{
    data.val.tmp = data.frame(group = rep(k_group, nrow(data.raw)),
                              value = data.raw[, col.val])
  }
  data.val = dplyr::as_tibble(data.val.tmp)
  k_group = 2
  if (length(indices.catchment) == 1){
    ### Only 1 level in group
    warning("Only 1 file in the provided paths")
  }else{
    for(i_file in indices.catchment[-1]){
      path.data.current = datapath[i_file]
      if (is.null(NA.code)){
        ### No NA
        data.raw = read.table(file = path.data.current,
                              header = header.files,
                              sep = sep.file,
                              skip = skip.lines)
      }else{
        data.raw = read.table(file = path.data.current,
                              header = header.files,
                              na.strings = NA.code,
                              sep = sep.file,
                              skip = skip.lines)
      }
      if(any(!is.na(col.date))){
        if(length(col.date)==1){
          ### Date is collapsed in 1 column
          tmp.time = data.raw[, col.date]
        }else{
          tmp.time = apply(X = data.raw[, col.date],
                           MARGIN = 1,
                           FUN = paste,
                           collapse="-")
        }
        ### Convert date data to Date format
        day.time = as.Date(tmp.time,
                           format = formatDate,
                           origin = origin)
        if(any(!is.na(col.extra))){
          data.val.tmp = data.frame(datetime = day.time,
                                    group = rep(k_group, nrow(data.raw)),
                                    value = data.raw[,col.val],
                                    data.raw[, col.extra])
        }else{
          data.val.tmp = data.frame(datetime = day.time,
                                    group = rep(k_group, nrow(data.raw)),
                                    value = data.raw[,col.val])
        }
      }else{
        data.val.tmp = data.frame(group = rep(k_group,nrow(data.raw)),
                                  value = data.raw[, col.val])
      }
      data.val = dplyr::bind_rows(data.val, dplyr::as_tibble(data.val.tmp))
      k_group = k_group + 1
    }
  }
  data.val = dplyr::mutate_at(.tbl = data.val,
                              .vars = dplyr::vars(group),
                              .funs = factor)
  data.info = dplyr::as_tibble(data.frame(group = factor(1:(k_group-1)),
                                          filename = datapath[indices.catchment]))


  if(coord.get){
    ## create temporay data.frame to store numeric values when coord.get = TRUE
    data.coords=data.frame(group=numeric(),
                           x.long=numeric(),
                           x.lat=numeric(),
                           y.long=numeric(),
                           y.lat=numeric())
    col.name.save=colnames(data.coords)

    for(i_file in 1:length(indices.catchment)){
      ## path of the file
      path.data.current = datapath[indices.catchment[i_file]]
      ## read data
      data.coords.tmp=read.table(file = path.data.current,
                                 skip = coord.skip.lines,
                                 nrows = 1,
                                 comment.char = "",sep="",
                                 header = FALSE)

      ### Keep only the numeric values
      data.coords.tmp=data.coords.tmp[,which(sapply(data.coords.tmp,class) == "numeric")]

      ### Combine it to the
      data.coords=rbind.data.frame(data.coords,c(i_file,as.numeric(data.coords.tmp[1,coord.col])))
    }


    colnames(data.coords)=col.name.save
    data.coords$group=factor(data.coords$group)

    ### Add data.coords to the data.info: So 4 new columns, x.long, x.lat, y.long, and y.lat
    data.info=dplyr::left_join(data.info,dplyr::as_tibble(data.coords),by="group")

  }


  return(list(data = data.val, info = data.info))
}


