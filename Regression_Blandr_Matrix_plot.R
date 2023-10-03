## ===================================================
# Generate plots
## ===================================================

## -----------------
# Helper Functions
## -----------------

my_fn1 <- function(data, mapping, method="lm", ...){
  p <- ggplot2::ggplot(data = data, mapping = mapping) + 
    ggplot2::geom_point(colour = "lightsteelblue") + 
    ggplot2::geom_smooth(method=method,colour="red", fill="lightgrey", ...)+
    ggpubr::stat_cor(ggplot2::aes(label=paste("Pearson",..rr.label.., ..p.label.., sep = "~`,`~")), 
                     method="pearson", label.y.npc = 0.99) +
    ggpubr::stat_regline_equation(label.y.npc = 0.85)
  p
}

my_blandr1 = function(data, mapping, ...) {
  library(tibble)
  library(ggplot2)
  
  xChar = as.character(rlang::quo_get_expr(mapping$x))
  yChar = as.character(rlang::quo_get_expr(mapping$y))
  
  x = data[, c(xChar)]
  y = data[, c(yChar)]
  
  p = blandr::blandr.draw(x, y, ciShading = TRUE, ciDisplay = TRUE, plotter = "ggplot")
  p$layers[[1]]$aes_params$colour = 'lightsteelblue' # points
  p$layers[[3]]$aes_params$colour = 'blue' # Bias line
  p$layers[[4]]$aes_params$colour = 'green' # Upper SD line
  p$layers[[5]]$aes_params$colour = 'red' # lower SD line
  
  pl = p + geom_text(data = data.frame(y = p$layers[[3]]$data$yintercept), #Bias
                     aes(x = max(x)*0.95, y = y,
                         label = paste("mean.diffs", round(y, 2))),
                     hjust = 0.8, vjust = -0.4, size = 3) +
    geom_text(data = data.frame(y = p$layers[[4]]$data$yintercept), #Upper SD
              aes(x = max(x)*0.95, y = y,
                  label = paste("upper.limit", round(y, 2))),
              hjust = 0.8, vjust = -0.4, size = 3) +
    geom_text(data = data.frame(y = p$layers[[5]]$data$yintercept), #Lower SD
              aes(x = max(x)*0.95, y = y,
                  label = paste("lower.limit", round(y, 2))),
              hjust = 0.8, vjust = -0.4, size = 3) +
    geom_text(data = data.frame(y = p$layers[[2]]$data$yintercept), # 0 line
              aes(x = min(x), y = y,
                  label = paste(round(y, 2))),
              hjust = 0.8, vjust = -0.4, size = 3) +
    theme_bw() +
    theme(axis.title = element_blank(), plot.title = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank())
  
  #  p_build <- ggplot_build(pl)
  #  p_build$data[[12]][["fill"]] <- "steelblue"
  #  p_build$data[[13]][["fill"]] <- "thistle"
  #  p_build$data[[14]][["fill"]] <- "thistle"
  
  #  plt = ggpubr::as_ggplot(ggplot_gtable(p_build))
  #  plt
}



## -----------------
# Main Function
## -----------------
Matrix_plot = function(data, title) {
  GGally::ggpairs(data = data,
                  upper = list(continuous = my_blandr1),
                  lower = list(continuous = my_fn1),
                  title = paste0(title)) +
    ggplot2::theme_bw()
}
