g3a_spawn_recruitment_bevertonholt_ss3 <- function (
    h = g3_parameterized('srr_h', by_stock = by_stock, lower = 0.1, upper = 1, value = 0.5),
    R0 = g3_parameterized('R0', by_stock = by_stock),
    B0 = g3_parameterized('B0', by_stock = by_stock),
    by_stock = TRUE) {
  list(
    s = quote( sum(stock_ss(stock__wgt) *
                     stock_ss(stock__spawningnum)) ),
    r = ~4*h*R0*s/(B0 * (1L - h) + s * (5L * h - 1) - s )
  )
}