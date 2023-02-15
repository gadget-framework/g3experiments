proj_stock_actions <- function(num_project_years,
                               mat,
                               imm = NULL,
                               prop_ogive = NULL,
                               comp_id = 'species'){
  
  if (is.null(imm)) imm <- mat
  
  ## Setup up a dummy stock
  dummy_stock <- 
    gadget3::g3_stock(c(species = gadgetutils::g3_stock_name(imm), 'dummy'),
                      lengthgroups = seq(min(gadget3::g3_stock_def(imm, 'minlen')),
                                         min(gadget3::g3_stock_def(mat, 'maxlen')),
                                         1)) %>% 
    gadget3::g3s_livesonareas(areas[c('1')]) %>% 
    gadget3::g3s_age(minage = 0, maxage = gadget3::g3_stock_def(imm, 'minage'))
  
  dummy_actions <- 
    list(
      gadget3::g3a_age(dummy_stock, output_stocks = list(imm))
    )
  
  if (is.null(prop_ogive)){
    pfunc <- 1
  }
  
  spawning_actions <- 
    list(
      gadget3::g3a_spawn(
        stock = mat,
        output_stocks = list(dummy_stock),
        recruitment_f = 
          g3a_spawn_recruitment_hockeystick(
            r0 = gadget3:::f_substitute(~g3_param_table('project_rec',
                                                        expand.grid(cur_year = seq(end_year - minage, 
                                                                                   end_year + py)), ifmissing = 0),
                                        list(py = num_project_years,
                                             minage = gadget3::g3_stock_def(imm, 'minage'))),
            blim = g3_parameterized('blim')), 
        proportion_f = pfunc,
        mean_f = g3_parameterized('recl', by_stock = comp_id),
        stddev_f = g3_parameterized('rec.sd', by_stock = comp_id),
        alpha_f = g3_parameterized('walpha', by_stock = comp_id),
        beta_f = g3_parameterized('wbeta', by_stock = comp_id),
        run_f = ~cur_year_projection && cur_step == 1) 
      
    )
  
  return(c(dummy_actions, spawning_actions))
  
}
