PlotVariableSparsity <- function(dataset) {

   zero.prop <-
      apply(dataset, 2, function(x) {
         sum(x == 0) / nrow(dataset)
      })

g <- data_frame(x = zero.prop) %>%
   ggplot(aes(x = x, y = ..count.., fill = ..count..)) +
   geom_histogram(bins = 50) +
   scale_fill_distiller(name = "Count",
                        palette = "Spectral",
                        limits = c(0, 50)) +
   guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
   scale_y_continuous(breaks = seq(0, 50, by = 10), limits = c(0, 50)) +
   labs(x = "Variable sparsity", y = "Frequency")

return(g)
}
