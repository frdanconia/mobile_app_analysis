PlotDbscanClustering <- function(onehotmatrix,col) {
   col.scale <- colMaxs(abs(onehotmatrix))
   x.sc <- sweep(onehotmatrix, 2, col.scale, "/")
   clustering_df <- data.frame(onehotmatrix)
   
   cl.model <- dbscan(x.sc, eps = 0.5, minPts = 128)
   cl.features <-
      data_frame(cluster = factor(cl.model$cluster, levels = sort(unique(
         cl.model$cluster
      ))),
      class = !clustering_df[[col]]) %>%
      mutate(
         pos_class_prop = sum(class == 1) / n(),
         neg_class_prop = sum(class == 0) / n()
      ) %>%
      group_by(cluster) %>%
      mutate(
         pos_cluster_prop = sum(class == 1) / n() / pos_class_prop,
         neg_cluster_prop = sum(class == 0) / n() / neg_class_prop
      ) %>%
      ungroup() %>%
      mutate(
         pos_cluster_prop = pos_cluster_prop / (pos_cluster_prop + neg_cluster_prop),
         neg_cluster_prop = neg_cluster_prop / (pos_cluster_prop + neg_cluster_prop)
      )
   
   
   
   g <- ggplot(cl.features, aes(x = pos_cluster_prop, fill = class)) +
      geom_density(
         size = 0.5,
         adjust = 5,
         color = "black",
         alpha = 0.8
      ) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
      labs(x = "Positive class proportion inside each cluster", y = "KDE Estimates")

   g
   return(g)
}
