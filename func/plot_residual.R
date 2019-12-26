plot_residual <- function(data_test, orig = NULL, experts) {
  if (!is.null(orig)) {
    names_exp <- colnames(experts)
    pred <- as.matrix(cbind(orig, experts))
    colnames(pred) <- c("gam", names_exp)
  } else {
    pred <- experts
  }
  pred <- pred - data_test$Load
  to_plot <- pred %>%
    as_tibble(rownames = NULL) %>%
    gather('id', 'value', 1:ncol(pred)) %>%
    group_by(id)
  to_plot$date <- as.Date(paste(data_test$Year,
                                data_test$Month,
                                data_test$Day,
                                sep = '-'))
  ggplot(to_plot, aes(date, value, colour = id)) +
    geom_line() +
    labs(y ='Bias in MW',
         title = 'Bias of all experts')
}
