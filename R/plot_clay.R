#' Plot clay percentage by depth for group of pedons
#'
#' @name plot_clay
#'
#' @param pedons Soil Profile Collection (pedons) for analysis
#' @param claymin clay axis minimum
#' @param claymax clay axis maximum
#' @return list containing one plot for each peiid
#' @export

plot_clay <- function(pedons, claymin = 0, claymax = 100) {

  require(aqp)
  require(soilDB)
  require(tidyverse)

sink(file = "a")

xh <- horizons(pedons)
xs <-site(pedons)
xs2 <- xs %>% select(siteiid, peiid, site_id)
xjoin <- left_join(xh, xs2, by="peiid")
spl <- split(xjoin, xjoin$peiid)


spl_name<- as.vector(NULL)

for(i in 1:length(spl)) {
  spl_name <- rbind(spl_name, dput(as.character(unique(spl[[i]]$peiid))))
}

for(i in 1:length(spl)) {
  assign(spl_name[i], spl[[i]])
}

df_list <- dput(as.numeric(unique(xjoin$peiid)))

sink(NULL)

clayplots <- vector('list', length(df_list))

for(i in seq_along(df_list)) {
  message(i)
  clayplots[[i]] <-
  p1 <- get(as.character(df_list)[i]) %>% ggplot(aes(x = hzdept, y = clay)) +
    scale_y_continuous(limits = c(claymin, claymax)) + scale_x_continuous(limits = c(200,0), trans = "reverse") +
    geom_point(aes(col = site_id)) + geom_line(aes(col = site_id)) + coord_flip() + theme_minimal()
  print(p1)

}

return(clayplots)

}
