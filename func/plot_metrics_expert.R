plot_metrics_expert <- function(data_test, list_expert, orig_rmse) {
  eval <- lapply(list_expert, function(x) {
    evalExp(data_test$Load, x)
  })
  metrics <- data.frame(do.call(rbind, eval)) %>%
    select(- cor)
  metrics$names <- rownames(metrics)
  metrics <- metrics %>%
    select(-Ridge)
  metrics_to_plot <- metrics %>%
    as.data.frame() %>%
    tidyr::gather('id', 'value', 1:5)
  colnames(metrics_to_plot)[2] <- 'metric'
  metrics_to_plot$value <- unlist(metrics_to_plot$value)
  ggplot(metrics_to_plot, aes(names, value, fill = metric)) +
    geom_col(position = 'dodge') +
    geom_hline(yintercept = orig_rmse, linetype = 'dashed') +
    annotate("text", x = metrics_to_plot$names[1], y = orig_rmse + 10,
             label = c("orig expert")) +
    coord_cartesian(ylim = c(min(metrics_to_plot$value, orig_rmse) - 100,
                         max(metrics_to_plot$value, orig_rmse) + 50)) +
    scale_fill_brewer(palette = "Spectral") +
    labs(x = 'strategy', y ='RMSE en MW',
         title = 'individual evaluation of strategies online condition')
}
