PlotVariableCardinality <- function(dataset,target) {
   
   cat.prop <- dataset %>%
      dplyr::select(-c(target), -which(map_lgl(., is.numeric))) %>%
      map(function(x)
         as.integer(table(x))) %>%
      reduce(c)
   g <- data_frame(x = cat.prop) %>%
      ggplot(aes(x = x, y = ..count.., fill = ..count..)) +
      geom_histogram(bins = 50) +
      scale_fill_distiller(name = "Count",
                           palette = "Spectral",
                           limits = c(0, 50)) +
      guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
      scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
      labs(x = "Category frequency", y = "Frequency")
   
   return(g)
}
