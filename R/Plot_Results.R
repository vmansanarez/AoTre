
################################################################################
#' @title MAP results
#' @description Function returning a map of the data in a specific region
#' accordingly to the result of a statistic. Each group is represented as a
#' point filled by the result of the statistic of the test and sized accordingly
#' to the significativity of the test.
#' @param data.plot dtbl, data tibble of the resultats to be plotted.
#' @param colnames.settings vector of characters, names of the columns
#' containing, in that order, coordinates X and Y of the station, the group
#' representing the station indexation, the p value and the statistic at the
#' station (colnames.settings = c("Long","Lat","group","p","stat") by default).
#' @param size.point vector of integer, size of the point representing the
#' result at the station, first number for non significant results, seconf
#' number for significant result (size.point = c(0.8,1) by default).
#' @param alpha.test real, significant level used in the Mann-Kendall test
#' realized and matching with the results from the data (alpha.test = 0.05 by
#' default);
#' @param region.box character, Region to zoom in the final plot (default:
#' "France").
#' @param color.fill.low character, color of the lower bound of the statistic
#' (default: "#053061", for red color).
#' @param color.fill.middle character, color of the middle of the statistic
#' (default: "#f7f7f7", for white color).
#' @param color.fill.high character, color of the higher bound of the statistic
#' (default: "#67001f", for blue color).
#' @param color.fill.midpoint real, value corresponding to the middle of the
#' statistic (default: "#67001f", for blue color).
#' @param color.fill.lim vector of real, values of the lower and the higher
#' bounds of the statistic (default: c(-4,4)). In case a value of the statistic
#' is outside the bounds, it will take the color of the closest bound.
#' @param color.fill.na_Value character, color of the NA values (default:
#' "green").
#' @param title.size character, title of the legend associated with the size of
#' the points.
#' @param title.fill character, title of the legend associated with the fill of
#' the points.
#' @param axis.size.text integer, size of the text in the axis of the plot
#' (default: 25).
#' @param theme.base_size integer, base size of the theme.
#' @return a ggplot object
#' @examples
#' @export
plot_map=function(data.plot,
                  ### colnames of X coords, Y coords, group (station), pvalue
                  # and stat
                  colnames.settings=c("Long","Lat","group1","p","stat"),
                  ### size points
                  size.point=c(0.8,1),
                  ### significance level alpha
                  alpha.test=0.05,
                  ### settings of the box/of the coords
                  region.box = "France",
                  color.colBorder="black",
                  color.fillBorder=NA,
                  #### Fill settings
                  color.fill.low="#053061",
                  color.fill.middle="#f7f7f7",
                  color.fill.high="#67001f",
                  color.fill.midpoint=0,
                  color.fill.lim=c(-4,4),
                  color.fill.na_Value="green",
                  #### Legend
                  title.size = "Significant?",
                  title.fill = "Statistic",
                  #### Setting plot
                  axis.size.text=25,
                  theme.base_size = 20){

  world <- ggplot2::map_data(map = "world")


  xy_stations=data.plot[,colnames.settings]
  colnames(xy_stations)=c("long","lat","group","p","stat")

  xy_stations$ID=data.plot$station

  ## Assign different sizes to non significant and significant results
  f_sign.size=function(x,alpha=0){
    return(ifelse(x<=alpha,size.point[2],size.point[1]))
  }
  ### alpha As an argument
  xy_stations$size.signif=sapply(xy_stations$p,f_sign.size,alpha=alpha.test)

  ###
  xy_stations$fill.stat=xy_stations$stat
  ### Only color the significant result [DROPPED]
  # which stat result is over the upper bound for color scale
  ind.tmp.pos.up = which(xy_stations$stat>color.fill.lim[2])
  xy_stations$fill.stat[ind.tmp.pos.up] = color.fill.lim[2]
  # which stat result is under the lower bound for color scale
  ind.tmp.pos.low = which(xy_stations$stat<color.fill.lim[1])
  xy_stations$fill.stat[ind.tmp.pos.low] = color.fill.lim[1]
  #

  ### Factorize the size
  xy_stations$size.signif=factor(x = xy_stations$size.signif,
                                 levels = sort(unique(xy_stations$size.signif)),
                                 labels = c("No","Yes"))

  plot.save=ggplot2::ggplot(data = subset(world,region %in% region.box))+
    ### Polygon of country, catchment
    ggplot2::geom_polygon(mapping = ggplot2::aes(long,lat,group=group),
                 fill = color.fillBorder,
                 color = color.colBorder)+
    ### Station of interest
    ggplot2::geom_point(data = xy_stations,
               mapping = ggplot2::aes(x = long,
                             y = lat,
                             fill = fill.stat,
                             size = size.signif),
               color = "black",
               stroke = 1,
               shape = 21)+
    ### Scale
    ggplot2::scale_fill_gradient2(midpoint = color.fill.midpoint,
                         mid = color.fill.middle,
                         high = color.fill.high,
                         low = color.fill.low,
                         limits = color.fill.lim,
                         na.value = color.fill.na_Value)+
    ggplot2::labs(size=title.size,fill=title.fill)+
    ggplot2::xlab("Longitude")+
    ggplot2::ylab("Latitude")+
    ### Annotations
    ggplot2::theme_bw(base_size = theme.base_size)+
    ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size.text))

  return(plot.save)
}
