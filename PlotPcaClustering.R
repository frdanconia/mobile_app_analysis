PlotPcaClustering <- function(onehotmatrix) {
   col.scale <- colMaxs(abs(onehotmatrix))
   x.sc <- sweep(onehotmatrix, 2, col.scale, "/")
   pca.model <-
      prcomp(x.sc,
             retx = FALSE,
             center = TRUE,
             scale. = FALSE)
   x.pca <- predict(pca.model, x.sc)[, 1:3]
   colnames(x.pca) <- paste0("pca_", 1:ncol(x.pca))
   exp.var <- cumsum(pca.model$sdev) / sum(pca.model$sdev)
   g <- data_frame(var = exp.var,
                   npcs = 1:length(exp.var)) %>%
      ggplot(aes(x = npcs, y = var, fill = var)) +
      geom_col(
         width = 0.8,
         size = 0.1,
         alpha = 0.8,
         color = "white"
      ) +
      scale_fill_distiller(guide = "none", palette = "Spectral") +
      geom_hline(
         yintercept = 0.8,
         size = 0.5,
         color = "gray40",
         linetype = "dashed"
      ) +
      geom_vline(
         aes(xintercept = npcs[which(var >= 0.8)[1]]),
         size = 0.5,
         color = "gray40",
         linetype = "dashed"
      ) +
      scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
      labs(x = "Number of PCs", y = "Explained Variance")
   
   save(x.pca, file = "./data/pca_features.rda", compress = "bzip2")
   
   return(g)
   
}


