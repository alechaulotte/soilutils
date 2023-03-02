#' Check for Contrasting Surface/Subsurface Textures
#'
#' @name contrast_tex
#'
#' @param pedons Soil Profile Collection (pedons) for analysis
#' @param tex1 surface texture classes
#' @param tex2 subsurface texture classes
#' @param depth minimum depth to assess subsurface (cm)
#' @examples
#' pedons <- fetchNASIS(from = "pedons")
#' tex1 <- c("lvfs", "lfs", "ls", "lcos", "vfs", "fs", "s", "cos")
#' tex2 <- c("fsl", "vfsl", "l", "sil", "si", "scl", "cl", "sicl", "sc", "sic", "c")
#' depth <- 25
#' contrast_tex(pedons, tex1, tex2, depth)
#' @return df of outputs
#' @export

contrast_tex <- function(pedons, tex1, tex2, depth) {

  require(aqp)
  require(soilDB)
  require(tidyverse)

  sink(file = "a")

  xh <- horizons(pedons)
  xs <- site(pedons)
  spl <- split(xh, xh$peiid)
  spl_name <- dput(as.character(unique(xh$peiid)))

  for(i in 1:length(spl)) {
    assign(spl_name[i], spl[[i]])
  }

  df_list <- dput(as.numeric(unique(xh$peiid)))

  sink(NULL)

  out_list <- data.frame(status=character())

  for(i in df_list) {
    if(!(get(as.character(i))[1,]$texcl %in% tex1) & !(get(as.character(i))[1,]$texcl %in% tex2)) {
      out_list <- rbind(out_list, ("surface texture not in tex1 or tex2")) }
    else if (get(as.character(i))[1,]$texcl %in% tex2) {
      out_list <- rbind(out_list, ("tex2 at surface")) }
    else if (get(as.character(i))[1,]$texcl %in% tex1) {
      if(nrow(get(as.character(i))) >= 2) {
        if(get(as.character(i))[2,]$texcl %in% tex1) {
          if(nrow(get(as.character(i))) >= 3) {
            if(get(as.character(i))[3,]$texcl %in% tex1) {
              if(nrow(get(as.character(i))) >= 4) {
                if(get(as.character(i))[4,]$texcl %in% tex1) {
                  if(nrow(get(as.character(i))) >= 5) {
                    if(get(as.character(i))[5,]$texcl %in% tex1) {
                      if(nrow(get(as.character(i))) >= 6){
                        if(get(as.character(i))[6,]$texcl %in% tex1) {
                          if(nrow(get(as.character(i))) >= 7) {
                            if(get(as.character(i))[7,]$texcl %in% tex1) {
                              if(nrow(get(as.character(i))) >= 8) {
                                if(get(as.character(i))[8,]$texcl %in% tex1) {
                                  if(nrow(get(as.character(i))) >= 9) {
                                    if(get(as.character(i))[9,]$texcl %in% tex1) {
                                      if(nrow(get(as.character(i))) >= 10) {
                                        if(get(as.character(i))[10,]$texcl %in% tex1) {
                                          out_list <- rbind(out_list, ("tex1 through h10"))
                                        }
                                        else if (get(as.character(i))[10,]$texcl %in% tex2) {
                                          out_list<- rbind(out_list, ("tex1 -> tex2 at h10"))
                                        }
                                        else {
                                          out_list <- rbind(out_list, ("other tex at h10"))
                                        }}
                                      else {
                                        out_list <- rbind(out_list, ("tex1 throughout"))
                                      }
                                    }
                                    else if (get(as.character(i))[9,]$texcl %in% tex2) {
                                      out_list <- rbind(out_list, ("tex1 -> tex2 at h9"))
                                    }
                                    else {
                                      out_list <- rbind(out_list, ("other tex at h9"))
                                    }
                                  }
                                  else {
                                    out_list <- rbind(out_list, ("tex1 throughout"))
                                  }
                                }
                                else if (get(as.character(i))[8,]$texcl %in% tex2) {
                                  out_list <- rbind(out_list, ("tex1 -> tex2 at h8"))
                                }
                                else {
                                  out_list <- rbind(out_list, ("other tex at h8"))
                                }
                              }
                              else {
                                out_list <- rbind(out_list, ("tex1 throughout"))
                              }
                            }
                            else if (get(as.character(i))[7,]$texcl %in% tex2) {
                              out_list <- rbind(out_list, ("tex1 -> tex2 at h7"))
                            }
                            else {
                              out_list <- rbind(out_list, ("other tex at h7"))
                            }
                          }
                          else {
                            out_list <- rbind(out_list, ("tex1 throughout"))
                          }
                        }
                        else if (get(as.character(i))[6,]$texcl %in% tex2) {
                          out_list <- rbind(out_list, ("tex1 -> tex2 at h6"))
                        }
                        else {
                          out_list <- rbind(out_list, ("other tex at h6"))
                        }
                      }
                      else {
                        out_list <- rbind(out_list, ("tex1 throuhgout"))
                      }
                    }
                    else if (get(as.character(i))[5,]$texcl %in% tex2) {
                      out_list <- rbind(out_list, ("tex1 -> tex2 at h5"))
                    }
                    else {
                      out_list <- rbind(out_list, ("other tex at h5"))
                    }
                  }
                  else {
                    out_list <- rbind(out_list, ("tex1 throughout"))
                  }
                }
                else if (get(as.character(i))[4,]$texcl %in% tex2) {
                  out_list <- rbind(out_list, ("tex1 -> tex2 at h4"))
                }
                else {
                  out_list <- rbind(out_list, ("other tex at h4"))
                }
              }
              else {
                out_list <- rbind(out_list, ("tex1 throughout"))
              }
            }
            else if (get(as.character(i))[3,]$texcl %in% tex2) {
              out_list <- rbind(out_list, ("tex1 -> tex2 at h3"))
            }
            else {
              out_list <- rbind(out_list, ("other tex at h3"))
            }
          }
          else {
            out_list <- rbind(out_list, ("tex1 throughout"))
          }
        }
        else if (get(as.character(i))[2,]$texcl %in% tex2) {
          out_list <- rbind(out_list, ("tex1 -> tex2 at h2"))
        }
        else {
          out_list <- rbind(out_list, ("other tex at h2"))
        }
      }
      else {
        out_list <- rbind(out_list, ("tex1 throughout"))
      }
    }
  }

  return(out_list)

}
