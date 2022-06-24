lm_summary <- function(.data) {
  .lm <- lm(.data[,1]~., data = .data)
}
