#' Plot clay percentage by depth for group of pedons
#'
#' @name plot_clay
#'
#' @param pedons Soil Profile Collection (pedons) for analysis
#' @return list containing one plot for each peiid
#' @export

plot_clay <- function(pedons) {

  require(aqp)
  require(soilDB)
  require(tidyverse)

sink(file = "a")

xh <- horizons(pedons)
xs <-site(pedons)
xs2 <- xs %>% select(siteiid, peiid, site_id)
xjoin <- left_join(xh, xs2, by="peiid")
spl <- split(xjoin, xjoin$peiid)
spl_name <- dput(as.character(unique(xjoin$peiid)))

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
    geom_point(aes(col = site_id)) + geom_line(aes(col = site_id)) + coord_flip()
  print(p1)

}

return(clayplots)

}
