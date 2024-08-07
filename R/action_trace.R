# g3a_trace_nan: Append "does variable contain NaN" trace statements to every action
# trace_actions <- g3experiments::g3a_trace_nan(actions, var_re = '__wgt$')
# actions <- c(actions, list(trace_actions))
g3a_trace_nan <- function (
        actions,
        check_positive = FALSE,
        on_error = quote({}),
        print_var = FALSE,
        var_re = c("__num$", "__wgt$")) {
    out <- new.env(parent = emptyenv())
    var_re <- paste(var_re, collapse = "|")
    if (on_error == "browse") on_error <- quote(browser())
    if (on_error == "stop") on_error <- quote(stop())

    # Generate code to test if var_name (contains any) NaN
    to_test_code <- function (var_name, var_val) {
        # NB: is.na(NaN) is TRUE, converse isn't true
        test_c <- substitute(
            is.na(var),
            list(var = as.symbol(var_name)))
        if (isTRUE(check_positive)) test_c <- substitute(
            test_c | var < 0,
            list(test_c = test_c, var = as.symbol(var_name)))

        if (is.array(target_vars[[var_name]])) test_c <- call("any", test_c)
        return(test_c)
    }

    # Form list of definitions as we would do when compiling
    collated_actions <- gadget3:::g3_collate(actions)
    all_actions <- gadget3:::f_concatenate(collated_actions, parent = g3_env, wrap_call = call("while", TRUE))
    code <- rlang::f_rhs(all_actions)
    env <- environment(all_actions)
    all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NULL))
    all_defns <- all_defns[!is.null(all_defns)]

    # Get variables we're interested in
    target_vars <- all_defns[grep(var_re, names(all_defns))]

    # Make map of var_name to nan_(var_name)
    nan_var_names <- structure(
        paste0('nan_', names(target_vars)),
        names = names(target_vars))

    # Environment for all trace steps, defining nan_vars
    trace_env <- as.environment(structure(
        rep(list(g3_global_formula(init_val = FALSE)), length(nan_var_names)),
        names = nan_var_names ))

    for (action_name in names(collated_actions)) {
        desc <- gadget3:::step_find_desc(collated_actions[[action_name]], minor_steps = TRUE)

        # Find variables this step alters (no point checking anything else)
        altered_vars <- intersect(names(target_vars), vapply(
            gadget3:::f_find(collated_actions[[action_name]], "<-"),
            function (x) as.character(if (is.call(x[[2]])) x[[2]][[2]] else x[[2]]),
            character(1) ))

        # Step to add after current step, checking NaN status for all vars
        trace_step <- as.call(c(
            as.symbol("{"),  # }
            substitute(
                debug_label(lbl), list(lbl = paste0("g3a_trace_nan: ", desc))),
            lapply(altered_vars, function (var_name) substitute(
                if (!nan_var && test_c) {
                    nan_var <- TRUE
                    Rprintf(warn_msg, cur_year, cur_step)
                    if (print_var) print(drop(var_sym))
                    on_error
                }, list(
                    warn_msg = paste0(var_name, " failed test at %d-%d, after '", desc, "'\n"),
                    test_c = to_test_code(var_name, target_vars[[var_name]]),
                    nan_var = as.symbol(nan_var_names[[var_name]]),
                    var_sym = as.symbol(var_name),
                    print_var = print_var,
                    on_error = on_error)))))

        out[[paste0(action_name, ":trace_nan")]] <- gadget3:::call_to_formula(trace_step, env = trace_env)
    }

    return(as.list(out))
}
