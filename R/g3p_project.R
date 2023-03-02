
g3p_project_rec <- function(par,recruitment,rec_pattern = 'project_rec', method='bootstrap',scale = 1e-4,...){
  
  n_years <- length(par[grepl(rec_pattern,par$switch),'value'])
  if (method == "AR") {
    rec <- 
      g3p_project_rec_arima(recruitment,
                           n_years = n_years, 
                           ...) 
  } else if (method == "bootstrap") {
    rec <- 
      g3p_project_rec_bootstrap(recruitment,n_years,...)
    
  } else if (method == "constant") {
    rec <- 
      g3p_project_rec_constant(recruitment,n_years,...)
    
  } else {
    stop("Method not valid, expected constant, AR or bootstrap")
  }
  
  par[grepl(rec_pattern,par$switch),'value'] <- scale*rec 
  
  return(par)
}



g3p_project_advice_error <- function(par,hr_target, advice_rho = 0.4, advice_cv = 0.2, hr_pattern = 'project_hr'){
  
  n_years <- 
    length(par[grepl(hr_pattern,par$switch),'value'] )
  
  
  if(advice_cv == 0){
    par[grepl(hr_pattern,par$switch),'value'] <- hr_target 
  } else {
    par[grepl(hr_pattern,par$switch),'value'] <- 
      hr_target * exp(stats::arima.sim(n = n_years, 
                                   list(ar = advice_rho), 
                                   sd = advice_cv/sqrt(1-advice_rho^2)))
  }
  
  return(par)
  
}


g3p_project_rec_arima <- function (recruitment, n_years) {
  mrec <- mean(recruitment$recruitment)
  recruitment %>% 
    dplyr::mutate(recruitment = log(recruitment)) %>% 
    stats::lm(head(recruitment, -1) ~ utils::tail(recruitment, -1), data = .) %>% 
    { list(variables = broom::tidy(.) %>% 
             { tibble::tibble(a = .$estimate[1], b = .$estimate[2]) }, 
           sigma = broom::glance(.) %>% 
             dplyr::select(sigma))
    } %>% 
    dplyr::bind_cols() %>% 
    dplyr::slice(rep(1,n_years)) %>% 
    dplyr::mutate(rec = stats::arima.sim(dplyr::n(), 
                                         model = list(ar = unique(.data$b)), 
                                         sd = unique(.data$sigma))/sqrt(1 -  unique(.data$b)^2), 
                  rec = mrec * exp(rec)) %>% 
    .$rec %>% 
    as.numeric() %>% 
    as.list()
}


g3p_project_rec_bootstrap <- function(recruitment,n_years,block_size = 7){
  tseries::tsbootstrap(recruitment$recruitment,
                       type = "block", 
                       nb = ceiling(n_years/length(recruitment$recruitment)), b = block_size) %>% 
    t() %>% 
    as.numeric() %>% 
    .[1:n_years] 
}

g3p_project_rec_constant <- function(recruitment,n_years){
  ## geometric mean
  exp(mean(log(recruitment$recruitment)))
}

## Function to setup reports for projects
## Can be merged with g3a_report_detail in the future
g3p_project_report <- function (actions,
                                abundance_run_at = 1,
                                run_at = 11) {
  c(
    gadget3::g3a_report_history(
      actions = actions,
      var_re = c('__num$', '__wgt$'),
      out_prefix = "proj_",
      run_f = ~cur_year_projection,
      run_at = abundance_run_at),
    gadget3::g3a_report_history(
      actions = actions,
      var_re = c('__renewalnum$', '__spawnednum$', '__predby_'),
      out_prefix = 'proj_',
      run_f = ~cur_year_projection,
      run_at = run_at),
    NULL
  )
}
