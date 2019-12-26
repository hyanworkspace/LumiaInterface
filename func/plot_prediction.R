plot_prediction <- function(data_test, orig = NULL, experts) {
  if (!is.null(orig)) {
    names_exp <- colnames(experts)
    experts <- as.matrix(cbind(data_test$Load, orig, experts))
    colnames(experts) <- c("real", "gam", names_exp)
  } else {
    names_exp <- colnames(experts)
    experts <- as.matrix(cbind(data_test$Load, experts))
    colnames(experts) <- c("real", names_exp)
  }
  to_plot <- experts %>%
    as_tibble(rownames = NULL) %>%
    gather('id', 'value', 1:ncol(experts)) %>%
    group_by(id)
  to_plot$date <- as.Date(paste(data_test$Year,
                                data_test$Month,
                                data_test$Day,
                                sep = '-'))

  # Dynamically generate default color values, but have Paid="black".
  adj_names <- sort(setdiff(unique(to_plot$id), "real"))
  values <- gg_color_hue(length(adj_names))
  names(values) <- adj_names
  values <- c(values, c(real = "black"))

  ggplot(to_plot, aes(date, value, colour = id)) +
    geom_line() +
    labs(y ='RMSE in MW',
         title = 'Predictions of all experts') +
    scale_colour_manual(values=values)
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
