

######################################################################
##### Function returning a map of the data in France

#'@param data.plot data.frame, resultas/data to be plotted.
#'@param size.point vector of integer, size of the point representing the result at the station,
#' first number for non significant results, seconf number for significant result
#'@param alpha.test significant level used in the Mann-Kendall test realized and matching with the results from the data
#' @return a ggplot object
#' @examples
#' plot_map(data.plot=data2bePlotted)
#' plot_map(data.plot=data2bePlotted,size.point=c(1,2))
#' plot_map(data.plot=datapath,size.point=c(1,2),alpha.test=0.32)
#' @export
plot_map=function(data.plot,
                  ### colnames of X coords, Y coords, and station group
                  data.colnames.settings=c("X_L2E","Y_L2E","group","p"),
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
                  title.fill = "Value",
                  #### Setting plot
                  axis.size.text=25
                  ){
  require(dplyr)

  require(sf)
  require(rgeos)
  require(rnaturalearth)
  require(ggspatial)
  require(ggplot2)

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











