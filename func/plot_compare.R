plot_compare <- function(data_test, list_expert, orig_rmse) {
  eval <- lapply(list_expert, function(x) {
    evalExp(data_test$Load, x)
  })
  eval <- data.frame(do.call(rbind, eval))
  eval <- eval %>%
    select(- cor)
  eval$names <- rownames(eval)
  eval <- eval %>%
    select(- Ridge)
  metrics_to_plot <- eval %>%
    as.data.frame() %>%
    tidyr::gather('id', 'value', 1:5)
  metrics_to_plot$value <- unlist(metrics_to_plot$value)
  metrics_to_plot <- metrics_to_plot %>%
    group_by(id) %>%
    arrange(by = value, .by_group = TRUE)
  metrics_to_plot$x <- rep(1:7, 5)
  this_min <- min(metrics_to_plot$value) - 20
  this_max <- max(metrics_to_plot$value) + 20

  ggplot(metrics_to_plot, aes(x, value, colour = id)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = orig_rmse, linetype = 'dashed') +
    annotate("text", x = 1.5, y = orig_rmse + 10,
             label = c("orig expert")) +
    geom_text(aes(label = names),
              hjust = 0, nudge_x = 0.05) +
    labs(y ='RMSE in MW',
         title = 'Compare all strategies') +
    ylim(this_min, this_max)
}
