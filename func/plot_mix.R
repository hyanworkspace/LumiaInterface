plot_mix <- function(Y, orig, experts) {
  names_exp <- colnames(experts)
  experts <- as.matrix(cbind(orig, experts))
  colnames(experts) <- c("gam", names_exp)
  agg.online <- mixture(Y = Y , experts = experts,
                       model = 'MLpol', loss.type = "square",
                       loss.gradient = T)
  col <- piratepal("basel", length.out = ncol(experts))
  plot(agg.online, pause = F, col = col)
}

plot_mix_no_gam <- function(Y, experts) {
  agg.online <- mixture(Y = Y , experts = experts,
                        model = 'MLpol', loss.type = "square",
                        loss.gradient = T)
  col <- piratepal("basel", length.out = ncol(experts))
  plot(agg.online, pause = F, col = col)
}
