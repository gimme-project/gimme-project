library(gimme)
library(gtools)

  fit <- gimmeSEM(
  data        = "/Users/jenomillechek/Documents/UNC/Spring 2023/PSYC 840/GIMME_data/",
  out         = "/Users/jenomillechek/Desktop/Gimme/",
  sep         = ",",
  header      = TRUE,
  ar          = TRUE,
  plot        = TRUE,
  subgroup    = TRUE
)

#### Function ####
gimmePlot <- function(object, data, out, group = "black", subgroup = "green3", individual = "gray50", positive = "#FF0000FF", negative = "#0000FFFF", sub_filename = "Summary Plot.pdf", ind_filename = "Individual Plot") {
  # create a folder titled gimmePlots for the altered plots - folder is created in the directory specified by the "out" argument
  if (!file.exists(file.path(out, "gimmePlots"))) {
    dir.create(file.path(out, "gimmePlots"))
  }

  # create PDF file for the summary plot - saved in the created gimmePlots folder
  pdf(file.path(out, "gimmePlots", sub_filename)) # opens a PDF graphical device of the original plot so that it does not appear in the Plots pane
  obj <- plot(object)
  obj$graphAttributes$Edges$color <-
    plyr::revalue(obj$graphAttributes$Edges$color,
                  c("black" = group, "green3" = subgroup, "gray50" = individual), warn_missing = F)
  
  pdf(file.path(out, "gimmePlots", sub_filename)) # Overwrites PDF of original plot with altered plot.
  plot(obj)
  dev.off()
  
  # Create a list of individual data files - will be used to count number of participants in sample and to feed file names to plot function
  file_list <- list.files(data)
  sorted_index <- mixedorder(file_list)
  file_list <- file_list[sorted_index]
  
  # Count number of individuals in sample - used to set the number of iterations in the for loop to create individual plots
  N <- length(file_list)

  # For loop to create altered plots for each of the individual plots and saves them in gimmePlots folder
  for (i in 1:N) {
    filename <- file_list[i]
    pdf(file.path(out, "gimmePlots", paste0(ind_filename, "_", i, ".pdf"))) # opens a PDF graphical device of the original plot so that it does not 
    # appear in the Plots pane
    ind <- plot(object, file = filename)
    dev.off() # closes PDF graphical device of original plot
    ind$graphAttributes$Edges$color <-
      plyr::revalue(ind$graphAttributes$Edges$color,
                    c("#FF0000FF" = positive, "#0000FFFF" = negative), warn_missing = F)
    
    pdf(file.path(out, "gimmePlots", paste0(ind_filename, "_", i, ".pdf"))) # Overwrites PDF of original plot with altered plot.
    plot(ind)
    dev.off()
    plot(ind)
  }

  # return altered summary plot
  return(plot(obj))

}
  
#### Running the gimmePlot function ####
gimmePlot(object = fit, data = "/Users/jenomillechek/Documents/UNC/Spring 2023/PSYC 840/GIMME_data/", out = "/Users/jenomillechek/Desktop/Gimme/", 
          group = "#f9c3e6", subgroup = "#99badd", individual = "#FFA500", positive = "black", negative = "red")
