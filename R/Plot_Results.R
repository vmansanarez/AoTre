
######################################################################
#' @title MAP results
#' @description Function returning a map of the data in a specific region accordingly to the result of
#'              a statistic. Each group is represented as a point fill by the result of the statistic of the test
#'              and sized accordingly to the significativity of the test.
#' @param data.plot dtbl, data tibble of the resultats to be plotted.
#' @param colnames.settings vector of characters, names of the columns containing, in that order,
#'        coordinates X and Y of the station, the group representing the station indexation, the pvalue
#'         and the statistic at the station (colnames.settings = c("Long","Lat","group","p","stat")
#'         by default).
#' @param size.point vector of integer, size of the point representing the result at the station,
#'        first number for non significant results, seconf number for significant result (size.point = c(0.8,1)
#'        by default).
#' @param alpha.test real, significant level used in the Mann-Kendall test realized and matching with the results
#'        from the data (alpha.test = 0.05 by default);
#' @param region.box character, Region to zoom in the final plot (default: "France").
#' @param crs.coords.sf character, projection system of the coordinates (default: "EPSG:27572").
#' @param agr.coords.sf character, attribute-geometry-relationship (default: "constant"). See documentation of
#'        the function sf::st_sf() for more information on it.
#' @param color.fill.low character, color of the lower bound of the statistic (default: "#053061", for red color).
#' @param color.fill.middle character, color of the middle of the statistic (default: "#f7f7f7", for white color).
#' @param color.fill.high character, color of the higher bound of the statistic (default: "#67001f", for blue color).
#' @param color.fill.midpoint real, value corresponding to the middle of the statistic (default: "#67001f", for blue color).
#' @param color.fill.lim vector of real, values of the lower and the higher bounds of the statistic (default: c(-4,4)).
#'        In case a value of the statistic is outside the bounds, it will take the color of the closest bound.
#' @param color.fill.na_Value character, color of the NA values (default: "green").
#' @param title.size character, title of the legend associated with the size of the points
#' @param title.fill character, title of the legend associated with the fill of the points.
#' @param axis.size.text integer, size of the text in the axis of the plot (default: 25).
#' @param theme.base_size integer, base size of the theme.
#' @return a ggplot object
#' @examples
#' @export
plot_map=function(data.plot,
                  ### colnames of X coords, Y coords, group (station), pvalue and stat
                  data.colnames.settings=c("X_L2E","Y_L2E","group","p","stat"),
                  ### size points
                  size.point=c(0.8,1),
                  ### significance level alpha
                  alpha.test=0.05,
                  ### settings of the box/of the coords
                  region.box = "France",
                  crs.coords.sf = "EPSG:27572",
                  agr.coords.sf = "constant",
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
                  axis.size.text=25
                  ){

  # require(sf)
  # require(rgeos)
  # require(rnaturalearth)
  # require(ggspatial)

  world <- ne_countries(scale = "medium", returnclass = "sf")

  xy_stations=data.plot[,data.colnames.settings]
  colnames(xy_stations)=c("long","lat","group","p")
  (xy_stations <- st_as_sf(xy_stations, coords = c("long", "lat"),
                           crs = CRS(crs.coords.sf), agr = agr.coords.sf))

  xy_stations$ID=data.plot$station
  xy_stations$group=data.plot$group
  xy_stations$stat=data.plot$stat

  box.lim=getBBox(bbox = region.box)

  ###
  f_sign.size=function(x,alpha=0){
    return(ifelse(x<alpha,size.point[2],size.point[1]))
  }

  xy_stations$size.signif=sapply(xy_stations$p,f_sign.size,alpha=alpha.test)### alpha As an argument

  xy_stations$fill.stat=xy_stations$stat
  xy_stations$fill.stat[which(xy_stations$p>alpha.test)] = 0 ### p>0.05, no tendance
  xy_stations$fill.stat[which(xy_stations$stat>color.fill.lim[2])] = color.fill.lim[2]
  xy_stations$fill.stat[which(xy_stations$stat<color.fill.lim[1])] = color.fill.lim[1]

  xy_stations$size.signif=factor(x = xy_stations$size.signif,levels = sort(unique(xy_stations$size.signif)),labels = c("No","Yes"))

  plot.save=ggplot(data = world) +
    geom_sf()+
    geom_sf(data = xy_stations,aes(fill = fill.stat,size=size.signif),color="black",stroke=1,shape=21)+
    coord_sf(xlim = box.lim[1:2], ylim = box.lim[3:4], expand = FALSE)+
    ### Scale
    scale_fill_gradient2(midpoint = color.fill.midpoint,mid=color.fill.middle,high=color.fill.high,
                         low=color.fill.low,limits=color.fill.lim,na.value = color.fill.na_Value)+
    labs(size=title.size,fill=title.fill)+
    ### Annotations
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme_bw()+
    theme(axis.text = element_text(size = axis.size.text))

  return(plot.save)
}











