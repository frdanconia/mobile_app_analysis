PlotPcaFeatures <- function(dataset, onehotmatrix, target) {
   col.scale <- colMaxs(abs(onehotmatrix))
   x.sc <- sweep(onehotmatrix, 2, col.scale, "/")
   pca.model <-
      prcomp(x.sc,
             retx = FALSE,
             center = TRUE,
             scale. = FALSE)
   x.pca <- predict(pca.model, x.sc)[, 1:3]
   g <- data_frame(pc1 = x.pca[, 1],
                   pc2 = x.pca[, 2],
                   y = dataset[[target]]) %>%
      ggplot(aes(x = pc1, y = pc2, color = y)) +
      geom_point(size = 1, alpha = 0.8) +
      scale_color_brewer(name = "Income", palette = "Set1") +
      labs(x = "PC1", y = "PC2")
   
   return(g)
}
