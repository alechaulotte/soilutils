#' Subset pedons by county symbol
#'
#' @name county_subset
#'
#' @param pedons Soil Profile Collection (pedons) for analysis
#' @return Soil Profile Collection objects (one for each county)
#' @export

county_subset <- function(pedons) {

  require(aqp)
  require(soilDB)
  require(tidyverse)

  xh <- horizons(pedons)
  xs <- site(pedons)
  xe <- get_extended_data_from_NASIS_db()

  xsao <- as.data.frame(xe$siteaoverlap) %>%
    filter(areatypename == "County or Parish")

  sink(file = "a")
  spl <- split(xsao, xsao$areasymbol)
  spl_name <- dput(as.character(unique(xsao$areasymbol)))
  df_list <- dput(as.character(unique(xsao$areasymbol)))
  sink(NULL)


  for(i in 1:length(spl)) {
    pedons <- pedons
    assign(spl_name[i], subset(pedons, pedons$siteiid %in% spl[[i]]$siteiid))
  }


}


