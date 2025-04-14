
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



g3p_project_advice_error <- function(par,hr_target, advice_rho = 0.423, 
                                     advice_cv = 0.212, hr_pattern = 'project_hr'){
  
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
    as.numeric()
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


g3p_setup_pars <- function(model, 
                           params, 
                           blim,
                           btrigger,
                           harvest_rates,
                           harvest_rate_trials,
                           fleet_proportions,
                           rec_list,
                           rec_years = NULL,
                           rec_scale = 1e-04,
                           rec_block_size = 7,
                           rec_method = 'bootstrap',
                           project_years = 100,
                           assess_err = FALSE,
                           advice_rho = 0.423,
                           advice_cv = 0.212,
                           blim_pattern = 'spawn_blim',
                           btrigger_pattern  = '*.hf.btrigger',
                           rec_pattern = 'project_rec.[0-9]',
                           hr_pattern = '*.hf.harvest_rate',
                           quota_prop_pattern = '*.quota.prop'){
  
  ## Check model
  if ('g3_r' %in% class(model)) model <- g3_to_tmb(attr(model, 'actions'))
  if (!('g3_cpp' %in% class(model))) stop("The 'model' argument should be a g3 model of class 'g3_r' or 'g3_cpp'")
  if (all(c('switch', 'value', 'optimise', 'parscale') %in% names(params))) params <- list(params)
  
  ## Turn optimise to TRUE for parameters we'll be changing, ie, btrigger,
  ## projected recruits and harvest rates, ensures g3_tmb_par() gets what we need
  base_par <- attr(model, 'parameter_template')
  
  ## Fleet proportions
  all_fleet_switches <- base_par[grepl(quota_prop_pattern, base_par$switch), 'switch']
  fleet_proportions$fleet <- paste0(fleet_proportions$fleet, gsub('\\*', '', quota_prop_pattern))
  if (!all(all_fleet_switches %in% fleet_proportions$fleet)){
    fleet_proportions <- rbind(fleet_proportions, 
                               expand.grid(fleet = all_fleet_switches[!(all_fleet_switches %in% fleet_proportions$fleet)],
                                           prop = 0))
  }
  if (sum(fleet_proportions$prop) != 1){
    warning('Scaling fleet proportions to sum to 1')
    fleet_proportions$prop <- fleet_proportions$prop/sum(fleet_proportions$prop)
  }
  
  ## BASE PARAMETERS FINISHED
  ## ONTO VARIANTS: recruitment, btrigger and harvest rates
  
  ## Check params, turn into list if its a data.frame
  if (is.data.frame(params))  params <- list('base' = params)
  if (!all(names(params[[1]]) %in% names(base_par))) stop("The 'params' argument should be a g3 parameter data.frame or list of g3 parameter data.frames")
  
  ## The setup:
  ## We will simulate the model with each harvest rate (harvest_rates argument)
  ## n number of times, where n = harvest_rate_trials. If assess_err = TRUE,
  ## each replicate will have a unique recruitment series and harvest rate, 
  ## if assess_err = FALSE, each replicate will have a unique recritment series.
  ## This will be setup for each of the input parameters (argument 'params')
  
  simdf <- expand.grid(hr = harvest_rates,
                       rep = 1:length(params),
                       trial = 1:harvest_rate_trials)
  simdf$id <- paste('h', simdf$hr, simdf$trial, simdf$rep, sep = '-')
  
  ## Loop over simdf to create list of input parameters
  out <- 
    lapply(split(simdf, simdf$id), function(x){
      
      ## Take values from estimated pars
      pars <- base_par
      switch_ind <- params[[x$rep]]$switch %in% pars$switch
      pars$value[params[[x$rep]]$switch[switch_ind]] <- params[[x$rep]]$value[switch_ind]
      
      ## Ensure all estimated pars plus harvest rates, projected recruitments
      ## and btrigger are optimise = TRUE. 
      pars$optimise <- params[[x$rep]]$optimise[match(pars$switch, params[[x$rep]]$switch)]
      pars[grepl(paste(btrigger_pattern, 
                       rec_pattern, 
                       hr_pattern, 
                       sep = '|'), base_par$switch), 'optimise'] <- TRUE
      
      pars <-  
        g3_init_val(pars, btrigger_pattern, btrigger, optimise = TRUE) |> 
        g3_init_val(blim_pattern, blim, optimise = TRUE) |> 
        g3_init_val('project_years', project_years, optimise = FALSE) |> 
        g3p_project_advice_error(hr_pattern = hr_pattern,
                                 hr_target = x$hr,
                                 advice_rho = advice_rho,
                                 advice_cv = ifelse(assess_err, advice_cv, 0))
      
      ## Merge fleet proportions into base parameters
      pars[match(fleet_proportions$fleet, pars$switch),'value'] <- fleet_proportions$prop
      
      ## Projected recruitment may come from a random walk which we will assume 
      ## if the recruitment pattern is not found.
      if (any(grepl(rec_pattern, pars$switch))){
        if (is.null(rec_years)) rec_years <- rec_list[[x$rep]]$year
        if (rec_method == 'bootstrap'){
          
          pars <- g3p_project_rec(pars, 
                                  rec_pattern = rec_pattern,
                                  recruitment = rec_list[[x$rep]][rec_list[[x$rep]]$year %in% rec_years,],
                                  method = rec_method,
                                  scale = rec_scale,
                                  block_size = rec_block_size)   
        }else{
          pars <- g3p_project_rec(pars, 
                                  rec_pattern = rec_pattern,
                                  recruitment = rec_list[[x$rep]][rec_list[[x$rep]]$year %in% rec_years,],
                                  method = rec_method,
                                  scale = rec_scale) 
        }
        pars$optimise <- TRUE
      }
      return(pars)
    })
  return(out)
}


