create_status_box <- function (input_data, input_text) {
  if (!is.null(input_data)) {
    status <- 'Uploaded'
    status_icon <- 'check'
    status_col <- 'light-blue'
  } else {
    status <- 'To be uploaded'
    status_icon <- 'exclamation'
    status_col <- 'red'
  }
  valueBox(
    h5(status), input_text, icon = icon(status_icon),
    color = status_col
  )
}
