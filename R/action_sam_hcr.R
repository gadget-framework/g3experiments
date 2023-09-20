g3a_hcr_assess_sam <- function (
    stock_imm,
    stock_mat,
    fleets,
    conf, ## sam confs
    tac_f = sam_wrapper,              # hcr__fishable, hcr__trigger --> hcr__tac function
    #prevtacweight = 0,  # Weighting of previous TAC in hcr__tac
    assess_run_f = quote(cur_step == 3),  # When to assess & regenerate TAC
    gather_run_f = quote(cur_step == 1),     # When to gather data for TAC
    hcr_name = 'hcr_sam',
    run_at = 12) {
  
  stocks <- list(stock_imm,stock_mat)
  
  out <- new.env(parent = emptyenv())
  action_name <- gadget3:::unique_action_name()
  
  hcr <- 
    gadget3:::g3_storage(hcr_name) |> 
    gadget3::g3s_age(minage = 0, maxage = 15) |> 
    gadget3:::g3s_modeltime(by_year = TRUE)
  
  ## catch in numbers at age over all fleets
  hcr__cn <- gadget3::g3_stock_instance(hcr, 0)
  
  ## catch in biomass at age divided by cn
  hcr__cw <- gadget3::g3_stock_instance(hcr, 0)
  
  ## Abundance by age at step 1 (end)
  hcr__smb <- gadget3::g3_stock_instance(hcr, 0)
  ## Abundance by age at step 3
  hcr__smh <- gadget3::g3_stock_instance(hcr, 0)
  
  ## Biomass at age divided by abundance
  hcr__sw <- gadget3::g3_stock_instance(hcr, 0)
  
  ## total abundance by maturity at step 1 by age
  hcr__immtotal <- gadget3::g3_stock_instance(hcr, 0)
  hcr__mattotal <- gadget3::g3_stock_instance(hcr, 0)
  
  ## adviced catch from the assessment model
  hcr__tac <- 0
    
  out[[gadget3:::step_id(run_at, hcr, action_name, 1)]] <- gadget3:::f_concatenate(c(
    
    lapply(stocks, function (stock) {
      gadget3:::f_concatenate(lapply(fleets, function(fleet){
        # Copy hcr from parent environment
        hcr <- hcr
        hcr__cn <- hcr__cn
        hcr__cw <- hcr__cw
        stock <- stock
        fleet_stock_var <- as.symbol(paste0('stock__predby_', fleet$name))
        gadget3:::g3_step(gadget3:::f_substitute(~{
          debug_trace("Collecting data from ", stock)
          if (gather_run_f) stock_iterate(stock, stock_intersect(hcr, {
            
            stock_ss(hcr__cn) <- stock_ss(hcr__cn) + sum(stock_ss(fleet_stock_var)/stock_ss(stock__wgt))
            stock_ss(hcr__cw) <- stock_ss(hcr__cw) + sum(stock_ss(fleet_stock_var))/sum(stock_ss(fleet_stock_var)/stock_ss(stock__wgt))
          }))
        }, list(

          fleet_stock_var = fleet_stock_var,
          gather_run_f = gather_run_f)))
      }))
    }),
    
    
    lapply(stocks, function (stock) {
        # Copy hcr from parent environment
        hcr <- hcr
        hcr__smb <- hcr__smb
        hcr__smh <- hcr__smh
        hcr__sw <- hcr__sw
        
        gadget3:::g3_step(gadget3:::f_substitute(~{
          debug_trace("Collecting data from ", stock)
          if (gather_run_f && cur_step == 1) stock_iterate(stock, stock_intersect(hcr, {
              stock_ss(hcr__smb) <- stock_ss(hcr__smb) + sum(stock_ss(stock__num))
              stock_ss(hcr__sw) <- stock_ss(hcr__sw) + sum(stock_ss(stock__num)*stock_ss(stock__wgt))/sum(stock_ss(stock__num))
            } ))
            
            if (gather_run_f && cur_step == 3) stock_iterate(stock, stock_intersect(hcr, {
                stock_ss(hcr__smh) <- stock_ss(hcr__smh) + sum(stock_ss(stock__num))
              } ))
        }, list(
          gather_run_f = gather_run_f)))
      }),
  
    
    gadget3:::g3_step(gadget3:::f_substitute(~{
      debug_label("g3a_hcr_assess: Collect ", hcr, " data")
      if (gather_run_f && cur_step == 1) stock_iterate(stock_imm, stock_intersect(hcr, {
        stock_ss(hcr__immtotal) <- stock_ss(hcr__immtotal) + sum(stock_ss(stock_imm__num))
      }))
      if (gather_run_f && cur_step == 1) stock_iterate(stock_mat, stock_intersect(hcr, {
        stock_ss(hcr__mattotal) <- stock_ss(hcr__mattotal) + sum(stock_ss(stock_mat__num))
      }))
    }, list(
      gather_run_f = gather_run_f))),

    list()))
  
  tac_fff <- gadget3:::g3_native(r=tac_f,cpp=NULL)
  
  out[[gadget3:::step_id(run_at, hcr, action_name, 2)]] <- gadget3:::g3_step(gadget3:::f_substitute(~{
    debug_label("g3a_hcr_assess: Assess ", hcr, " and reset TAC")
    if (assess_run_f) stock_with(hcr, {
      stock_iterate(hcr,
                    stock_ssinv(hcr__mattotal,'year')
                    )
      
      
      hcr__tac <- tac_fff(
        conf,
        hcr__cn,
        hcr__smb,
        hcr__smh,
        hcr__sw,
        hcr__cw,
        hcr__immtotal,
        hcr__mattotal
      )
    })
  }, list(
    assess_run_f = assess_run_f)))
  
  return(as.list(out))
}