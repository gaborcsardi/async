
get_colors <- function(num) {

  cols <- c("green", "blue", "magenta", "cyan", "yellow", "red", "silver")
  c(
    cols,
    sample(colors(), max(num - length(cols), 0))
  )[seq_len(num)]
}
