#' Check for Contrasting Surface/Subsurface Textures
#'
#' @name contrast_tex
#'
#' @param pedons Soil Profile Collection (pedons) of interest
#' @param tex1 surface texture classes
#' @param tex2 subsurface texture classes
#' @param tex1name name to assign surface textural classes
#' @param tex2name name to assign subsurface textural classes
#' @examples
#' pedons <- fetchNASIS(from = "pedons")
#' tex1 <- c("lvfs", "lfs", "ls", "lcos", "vfs", "fs", "s", "cos")
#' tex2 <- c("fsl", "vfsl", "l", "sil", "si", "scl", "cl", "sicl", "sc", "sic", "c")
#' tex1name <- "coarse"
#' tex2name <- "loamy"
#' contrast_tex(pedons, tex1, tex2, depth)
#' @return df of outputs
#' @export

contrast_tex <- function(pedons, tex1, tex2, tex1name = "tex1", tex2name = "tex2") {

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
      out_list <- rbind(out_list, paste("surface texture not in", tex1name, "or", tex2name, sep = " ")) }
    else if (get(as.character(i))[1,]$texcl %in% tex2) {
      out_list <- rbind(out_list, paste(tex2name, "at surface", sep = " ")) }
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
                                          out_list <- rbind(out_list, paste(tex1name, "through h10", sep = " "))
                                        }
                                        else if (get(as.character(i))[10,]$texcl %in% tex2) {
                                          out_list<- rbind(out_list, paste(tex1name, "->", tex2name, "at h10", sep = " "))
                                        }
                                        else {
                                          out_list <- rbind(out_list, ("other texture at h10"))
                                        }}
                                      else {
                                        out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
                                      }
                                    }
                                    else if (get(as.character(i))[9,]$texcl %in% tex2) {
                                      out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h9", sep = " "))
                                    }
                                    else {
                                      out_list <- rbind(out_list, ("other texture at h9"))
                                    }
                                  }
                                  else {
                                    out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
                                  }
                                }
                                else if (get(as.character(i))[8,]$texcl %in% tex2) {
                                  out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h8", sep = " "))
                                }
                                else {
                                  out_list <- rbind(out_list, ("other texture at h8"))
                                }
                              }
                              else {
                                out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
                              }
                            }
                            else if (get(as.character(i))[7,]$texcl %in% tex2) {
                              out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h7", sep = " "))
                            }
                            else {
                              out_list <- rbind(out_list, ("other texture at h7"))
                            }
                          }
                          else {
                            out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
                          }
                        }
                        else if (get(as.character(i))[6,]$texcl %in% tex2) {
                          out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h6", sep = " "))
                        }
                        else {
                          out_list <- rbind(out_list, ("other texture at h6"))
                        }
                      }
                      else {
                        out_list <- rbind(out_list, paste(tex1name, "throuhgout", sep = " "))
                      }
                    }
                    else if (get(as.character(i))[5,]$texcl %in% tex2) {
                      out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h5", sep = " "))
                    }
                    else {
                      out_list <- rbind(out_list, ("other texture at h5"))
                    }
                  }
                  else {
                    out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
                  }
                }
                else if (get(as.character(i))[4,]$texcl %in% tex2) {
                  out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h4", sep = " "))
                }
                else {
                  out_list <- rbind(out_list, ("other texture at h4"))
                }
              }
              else {
                out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
              }
            }
            else if (get(as.character(i))[3,]$texcl %in% tex2) {
              out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h3", sep = " "))
            }
            else {
              out_list <- rbind(out_list, ("other texture at h3"))
            }
          }
          else {
            out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
          }
        }
        else if (get(as.character(i))[2,]$texcl %in% tex2) {
          out_list <- rbind(out_list, paste(tex1name, "->", tex2name, "at h2", sep = " "))
        }
        else {
          out_list <- rbind(out_list, ("other texture at h2"))
        }
      }
      else {
        out_list <- rbind(out_list, paste(tex1name, "throughout", sep = " "))
      }
    }
  }

  return(out_list)

}
