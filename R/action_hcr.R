# # g3a_hcr_assess
#
# Use g3a_hcr_assess() to simulate assessment procedure:
#
#    g3a_hcr_assess(
#        list(ling_imm, ling_mat),
#        fishable_fs = g3a_hcr_biomass_weighted(-100,~g3_param('ref.cm'),1),
#        trigger_fs = list(
#            ling_imm = g3a_hcr_biomass_weighted(0,0,0),
#            ling_mat = g3a_hcr_biomass_weighted(0,0,1)),
#        tac_f = g3a_hcr_tac_singletrigger(),
#        assess_run_f = ~cur_step == 2),
#
# NB: Separate gather_run_f & assess_run_f, the former is when the
# g3a_hcr_biomass_weighted() data is gathered, the latter is when
# g3a_hcr_tac_singletrigger() is run.
#
# Create proportions of TAC assigned to each fleet / stock:
#
#    hcr_stock_proportions <- list(0.5)
#    hcr_fleet_proportions <- quote( stock_param(fleet_stock, 'hcr.prop', value = 1/2) )
#
# Wrap default fleet predation with g3a_predate_catchability_hcr(), to scale predation
# based on TAC:
#
#    g3a_predate_fleet(list(ling_imm, ling_mat),
#        catchability_f = g3a_predate_catchability_hcr(
#        g3a_predate_catchability_totalfleet(g3_timeareadata('lln_landings', lln_landings_data)),
#            hcr_stock_proportions,
#            hcr_fleet_proportions)),
g3a_hcr_tac_singletrigger <- function(
        trigger = g3_parameterized('tac.trigger', by_stock = by_stock),
        hr_low = g3_parameterized('tac.hr.low', by_stock = by_stock),
        hr_high = g3_parameterized('tac.hr.high', by_stock = by_stock),
        by_stock = FALSE) {  # TODO: Ideally would be by_stock = hcr, but cannae do that.
    gadget3:::f_substitute(quote(
        (
            hr_low * bounded(stock_ss(hcr__trigger) - trigger, 0, 1) +
            hr_high * (1 - bounded(stock_ss(hcr__trigger) - trigger, 0, 1))
        ) * stock_ss(hcr__fishable)
    ), list(
        hr_low = hr_low,
        hr_high = hr_high,
        trigger = trigger))
}
# e.g. curve(
#     gadget3:::f_eval(
#         g3a_hcr_tac_singletrigger(trigger = 50, hr_low = 100, hr_high = 200),
#         list(hcr__trigger = x, hcr__fishable = 1, stock_ss = identity)), 0, 100)

g3a_hcr_biomass_weighted <- function(
        alpha,
        l50,
        scale,
        assesserr_f = 1,
        by_stock = TRUE) {
    gadget3:::f_substitute(quote(
        # TODO: This is just g3_suitability_exponentiall50(), slot for suitability?
        sum(
            scale * (stock_ss(stock__num) * stock_ss(stock__wgt))
                /
            ( 1.0 + exp(-alpha * (l50 - stock__midlen)) )
        ) * assesserr_f
    ), list(
        alpha = alpha,
        l50 = l50,
        scale = scale,
        assesserr_f = assesserr_f))
}

g3a_hcr_assess <- function (
        stocks,             # Stock(s) to assess
        fishable_fs,        # List of stock names -> formula to gather, or single value applied to all
        trigger_fs,         # List of stock names -> formula to gather, or single value applied to all
        tac_f,              # hcr__fishable, hcr__trigger --> hcr__tac function
        prevtacweight = 0,  # Weighting of previous TAC in hcr__tac
        assess_run_f = quote(cur_step == 1),  # When to assess & regenerate TAC
        gather_run_f = assess_run_f,     # When to gather data for TAC
        hcr_name = 'hcr',
        run_at = 12) {
    if (gadget3:::g3_is_stock(stocks)) stocks <- list(stocks)
    stopifnot(is.list(stocks) & all(sapply(stocks, gadget3:::g3_is_stock)))

    hcr <- gadget3:::g3_storage(hcr_name)
    hcr__trigger <- gadget3:::stock_instance(hcr, 0)
    hcr__fishable <- gadget3:::stock_instance(hcr, 0)
    hcr__tac <- gadget3:::stock_instance(hcr, 1)

    out <- new.env(parent = emptyenv())
    action_name <- gadget3:::unique_action_name()

    out[[gadget3:::step_id(run_at, hcr, action_name, 1)]] <- gadget3:::f_concatenate(c(

        gadget3:::g3_step(gadget3:::f_substitute(~{
            debug_label("g3a_hcr_assess: Collect ", hcr, " data")
            if (gather_run_f) stock_with(hcr, {
                hcr__trigger[] <- 0
                hcr__fishable[] <- 0
            })
        }, list(
            gather_run_f = gather_run_f))),

        lapply(stocks, function (stock) {
            # Copy hcr from parent environment
            hcr <- hcr
            hcr__trigger <- hcr__trigger
            hcr__fishable <- hcr__fishable
            hcr__tac <- hcr__tac
            gadget3:::g3_step(gadget3:::f_substitute(~{
                debug_trace("Collecting data from ", stock)
                if (gather_run_f) stock_iterate(stock, stock_intersect(hcr, {
                    stock_ss(hcr__trigger) <- stock_ss(hcr__trigger) + trigger_f
                    stock_ss(hcr__fishable) <- stock_ss(hcr__fishable) + fishable_f
                }))
            }, list(
                fishable_f = gadget3:::list_to_stock_switch(fishable_fs),
                trigger_f = gadget3:::list_to_stock_switch(trigger_fs),
                gather_run_f = gather_run_f)))
        }),

        list()))

    out[[gadget3:::step_id(run_at, hcr, action_name, 2)]] <- gadget3:::g3_step(gadget3:::f_substitute(~{
        debug_label("g3a_hcr_assess: Assess ", hcr, " and reset TAC")
        if (assess_run_f) stock_iterate(hcr, {
            stock_ss(hcr__tac) <- prevtacweight * stock_ss(hcr__tac) + (1 - prevtacweight) * tac_f
        })
    }, list(
        prevtacweight = prevtacweight,
        tac_f = tac_f,
        assess_run_f = assess_run_f)))

    return(as.list(out))
}

g3a_predate_catchability_hcr <- function (
    nonprojection_f = 0,
    fleet_prop_fs,
    implerr_f = 1,
    hcr_name = 'hcr') {

    # Define identical hcr stock to g3a_hcr_assess()
    hcr <- gadget3:::g3_storage(hcr_name)
    hcr__trigger <- gadget3:::stock_instance(hcr, 0)
    hcr__fishable <- gadget3:::stock_instance(hcr, 0)
    hcr__tac <- gadget3:::stock_instance(hcr, 1)

    return(gadget3:::f_substitute(~(
        # TODO: What we really want here is stock_intersect(hcr, stock_ss(hcr__tac)), but that's not easy.
        if (cur_year_projection) (stock_with(hcr, sum(hcr__tac)) * (1 / total_steps) * fleet_prop_f * implerr_f) else nonprojection_f
    ), list(
        fleet_prop_f = gadget3:::list_to_stock_switch(fleet_prop_fs, stock_var = "fleet_stock"),
        nonprojection_f = nonprojection_f)))
}
