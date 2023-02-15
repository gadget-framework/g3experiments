
g3a_predate_catchability_hockeyfleet <- function (btrigger, hr, proportion_f = 1,  E, sum_stocks = list(), recalc_f = NULL) {
  #stopifnot(is.data.frame(quota_table) && identical(names(quota_table), c('biomass', 'quota')))
  #stopifnot(nrow(quota_table) > 0 && is.infinite(quota_table[nrow(quota_table), 'biomass']))
  
  #stopifnot(is.list(sum_stocks) && all(sapply(sum_stocks, g3_is_stock)))
  #stopifnot(is.null(recalc_f) || rlang::is_formula(recalc_f))
  
  # Generate code to produce biomass sum
  if (length(sum_stocks) == 0) {
    biomass_c <- quote(sum(stock__num * stock__wgt))
  } else {
    biomass_c <- 0
    
    for (stock in sum_stocks) {
      biomass_c <- gadget3:::f_substitute(quote(
        stock_with(s, sum(s__num * s__wgt * suit_f)) + biomass_c
      ), list(
        s = as.symbol(stock$name),
        s__num = as.symbol(paste0(stock$name, "__num")),
        s__wgt = as.symbol(paste0(stock$name, "__wgt")),
        suit_f = gadget3:::list_to_stock_switch(proportion_f, stock_var
                                                = stock$name),
        biomass_c = biomass_c))
    }
    
    # 
    # for (stock in sum_stocks) {
    #   ss <- stock_instance(stock,desc='Proportion SSB')
    #   stock_ss(ss) <- proportion_f
    #   biomass_c <- substitute(stock_with(s, sum(s__num * s__wgt * ss__suit )) + biomass_c, list(
    #     s = as.symbol(stock$name),
    #     s__num = as.symbol(paste0(stock$name, "__num")),
    #     s__wgt = as.symbol(paste0(stock$name, "__wgt")),
    #     ss__suit = stock_ss(ss),
    #     biomass_c = biomass_c))
    # }
  }
  
  # Make tertiary condition encoding quota
  
  quota_f <- gadget3:::f_substitute(~hr * logspace_add(-1000 * s / btrigger, -1000) / -1000, 
                                    list(hr = hr,
                                         btrigger = btrigger,
                                         s = biomass_c))
  
  # 
  #   for (i in rev(seq_len(nrow(quota_table)))) {
  #   if (i == nrow(quota_table)) {
  #     # NB: [[1]] picks out value from a list data.frame col (e.g. for formula)
  #     quota_f <- quota_table[i, 'quota'][[1]]
  #   } else {
  #     quota_f <- gadget3:::f_substitute(~if (biomass_c < cond) val else quota_f, list(
  #       biomass_c = biomass_c,
  #       cond = quota_table[i, 'biomass'][[1]],
  #       val = quota_table[i, 'quota'][[1]],
  #       quota_f = quota_f))
  #   }
  # }
  
  if (!is.null(recalc_f)) {
    quota_var_name <- paste0("stock__hockeyfleet_", unique_action_name())
    # NB: This should remain global to model
    assign(quota_var_name, structure(
      0.0,
      desc = "Current quota of stock"))
    quota_f <- gadget3:::f_substitute(~(quota_var <- if (recalc_f) quota_f else quota_var), list(
      recalc_f = recalc_f,
      quota_f = quota_f,
      quota_var = as.symbol(quota_var_name)))
  }
  
  out <- gadget3:::f_substitute(
    ~quota_f * E * cur_step_size * stock_ss(stock__predby_fleet_stock), list(
      quota_f = quota_f,
      E = E))
  
  for (stock in sum_stocks) {
    assign(stock$name, stock, envir = environment(out))
  }
  return(out)
}


