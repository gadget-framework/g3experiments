# # g3l_bounds_penalty
# 
# DEPRECATED: See gadget3::g3l_bounds_penalty
#
# Defines penalty functions for a given list of parameters with bounds
# define parameters as before but before final compilation and optimisation add
# the penalty to the list of actions:
# 
# actions <- c(actions,
#   list(g3l_bounds_penalty(tmb_param)))


g3l_bounds_penalty_old <- function(tmb_param, weight = 1){
  
  tmp_func <- function(d){
    param_name <- d$switch 
    upper_bound <- d$upper
    lower_bound <- d$lower
    tmp <- 
      list(gadget3:::f_substitute(g3_formula({
        if (cur_time == 0) {
          nll <- nll + weight * ((logspace_add(1e6*(g3_param(param)- upper_bound)/(upper_bound-lower_bound), 0)
                         + logspace_add(1e6*(lower_bound - g3_param(param) )/(upper_bound-lower_bound), 0))^2)
        }
      }), list(weight = weight, param = d$switch, upper_bound = d$upper, lower_bound = d$lower)))
    names(tmp) <- paste("010:bounds",d$switch, sep = ':')
    return(tmp)
  }
  
  bounds_action <- c()
  for(i in 1:nrow(tmb_param)){
    if(tmb_param$optimise[i]){
      bounds_action <- c(bounds_action,
                         tmp_func(tmb_param[i,]))
    }
  }
  return(bounds_action)
}

