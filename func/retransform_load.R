retransform_load <- function(x, pre_proc_values) {
  (x * pre_proc_values$std[['Load']]) + pre_proc_values$mean[['Load']]
}
