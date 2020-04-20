# a function to create table 1
#' @param dat inputted data frame 
#' @param which_variable what variables to include 
#' @param convert_to_rates variables in data set that need to be 
#' converted to rate per 100,000
#' @param rate_labels labels for converted rate variables
#' @param outbreak_thresh for death rates in places with "outbreaks"
#' @return a \code{data.frame} with two columns including a string of 
#' median (IQR) for each variable and rownames equal to variable labels
make_table1 <- function(dat, 
                        which_variable = c("population",
                                           "percentblack",
                                           "percentwhite",
                                           "percentover65",
                                           "percdiabetes",
                                           "heartdiseasedeathrate",
                                           "cerebroandhypertensiondeathrate",
                                           "HIVrate",
                                           "pctuninsured",
                                           "pctunemployed",
                                           "peroccgreaterthan1",
                                           "urbanicity2013",
                                           "pm25",
                                           "socialdistance",
                                           "time_since_first_case",
                                           "covidcases",
                                           "coviddeaths"),
                        convert_to_rates = NULL,
                        rate_labels = NULL,
                        outbreak_thresh = 200, 
                        pct_black_thresholds = c(13, 25)){
  if(!is.null(convert_to_rates[1])){
    # generate rate variables
    ct <- 0
    for(i in convert_to_rates){
      ct <- ct + 1
      dat[[paste0(i, "_rate")]] <- dat[[i]] / dat$population * 100000
      var_label(dat[[paste0(i,"_rate")]]) <- rate_labels[ct]
    }
  }
  # replace variables with rates
  final_variables <- which_variable
  final_variables[final_variables %in% convert_to_rates] <- paste0(convert_to_rates, "_rate")

  # hacky way to add endpoints
  pct_black_thresholds <- c(-Inf, pct_black_thresholds, Inf)
  # get median and IQR for each
  med_iqr_vars <- t(sapply(final_variables, function(v){
    tmp <- NULL
    for(ct in 2:(length(pct_black_thresholds))){
      tmp <- c(tmp, quantile(dat[[v]][dat$percentblack < pct_black_thresholds[ct] & dat$percentblack >= pct_black_thresholds[ct-1]], 
                             p = c(0.5, 0.25, 0.75), na.rm = TRUE))
    }
    tmp
  }))

  # add covid death rate in places with cases
  coviddeath_in_outbreaks <- NULL
  for(ct in 2:(length(pct_black_thresholds))){
      coviddeath_in_outbreaks <- c(coviddeath_in_outbreaks, 
                                   quantile(dat$coviddeaths_rate[dat$covidcases > outbreak_thresh & dat$percentblack < pct_black_thresholds[ct] & dat$percentblack >= pct_black_thresholds[ct-1]], 
                                            p = c(0.5, 0.25, 0.75), na.rm = TRUE))
  }
  med_iqr_vars <- rbind(
    med_iqr_vars, coviddeath_in_outbreaks
  )

  ct <- 0
  table_1 <- t(apply(med_iqr_vars, 1, function(x){
    ct <<- ct + 1
    # don't include decimals for population
    nums <- sprintf(ifelse(ct == which(which_variable == "population"), 
                           "%.0f", "%.1f"), x)
    tmp <- NULL
    for(j in seq(1, (length(pct_black_thresholds)-1)*3, by = 3)){
      tmp <- c(tmp, paste0(nums[j], " (", nums[j+1], ", ", nums[j+2], ")"))
    }
    return(tmp)
  }))

  row_names <- sapply(final_variables, function(v){
    var_label(dat[[v]])
  })

  row.names(table_1) <- c(row_names, paste0("COVID-19 death rate (cases > ", outbreak_thresh, ", n = ", sum(dat$covidcases > outbreak_thresh), ")"))

  tmp <- NULL
  for(ct in 2:(length(pct_black_thresholds))){
    if(ct == 2){
      tmp <- c(tmp, paste0("Proportion black < ", pct_black_thresholds[ct], "% (n = ", sum(dat$percentblack < pct_black_thresholds[ct]), ")"))
    }else if(ct == length(pct_black_thresholds)){
      tmp <- c(tmp, paste0("Proportion black >=", pct_black_thresholds[ct-1], "% (n = ", sum(dat$percentblack >= pct_black_thresholds[ct - 1]), ")"))
    }else{
      tmp <- c(tmp, paste0("Proportion black ", pct_black_thresholds[ct-1], "% - ", pct_black_thresholds[ct], "% (n = ", sum(dat$percentblack >= pct_black_thresholds[ct - 1] & dat$percentblack < pct_black_thresholds[ct]), ")"))
    }
  }
  colnames(table_1) <- tmp

  return(table_1)
}


make_table2 <- function(dat,
                        which_variable = c("percentover65",
                                           "percdiabetes",
                                           "heartdiseasedeathrate",
                                           "cerebroandhypertensiondeathrate",
                                           "HIVrate",
                                           "pctuninsured",
                                           "pctunemployed",
                                           "peroccgreaterthan1",
                                           "pm25",
                                           "socialdistance",
                                           "time_since_first_case"),
                        outbreak_thresh = 200){
  tab <- t(sapply(which_variable, function(x){
    # threshold for this variable
    cut_var <- quantile(dat[[x]], p = 0.75, na.rm = TRUE)
    strat_dat <- dat[dat[[x]] > cut_var,]

    # cases
    # disprop black
    n_dispblack <- sum(strat_dat$percentblack >= 13, na.rm = TRUE)
    pct_dispblack <- 100*mean(strat_dat$percentblack >= 13, na.rm = TRUE)
    form_n_dispblack <- paste0(n_dispblack, " (", sprintf("%.1f", pct_dispblack), ")")

    cases_dispblack <- quantile(strat_dat$covidcases_rate[strat_dat$percentblack >= 13], p = c(0.5, 0.25, 0.75), na.rm = TRUE)
    format_cases_dispblack <- paste0(sprintf("%.1f", cases_dispblack[1]), " (", sprintf("%.1f", cases_dispblack[2]), ", ", sprintf("%.1f", cases_dispblack[3]), ")")

    # other 
    n_other <- sum(strat_dat$percentblack < 13, na.rm = TRUE)
    pct_other <- 100*mean(strat_dat$percentblack < 13, na.rm = TRUE)
    form_n_other <- paste0(n_other, " (", sprintf("%.1f", pct_other), ")")

    cases_other <- quantile(strat_dat$covidcases_rate[strat_dat$percentblack < 13], p = c(0.5, 0.25, 0.75), na.rm = TRUE)
    format_cases_other <- paste0(sprintf("%.1f", cases_other[1]), " (", sprintf("%.1f", cases_other[2]), ", ", sprintf("%.1f", cases_other[3]), ")")

    # deaths
    # disprop black
    deaths_dispblack <- quantile(strat_dat$coviddeaths_rate[strat_dat$percentblack >= 13], p = c(0.5, 0.25, 0.75), na.rm = TRUE)
    format_deaths_dispblack <- paste0(sprintf("%.1f", deaths_dispblack[1]), " (", sprintf("%.1f", deaths_dispblack[2]), ", ", sprintf("%.1f", deaths_dispblack[3]), ")")

    # other 
    deaths_other <- quantile(strat_dat$coviddeaths_rate[strat_dat$percentblack < 13], p = c(0.5, 0.25, 0.75), na.rm = TRUE)
    format_deaths_other <- paste0(sprintf("%.1f", deaths_other[1]), " (", sprintf("%.1f", deaths_other[2]), ", ", sprintf("%.1f", deaths_other[3]), ")")

    # deaths where outbreaks occur
    # threshold for this variable
    cut_var <- quantile(dat[[x]][dat$covidcases > outbreak_thresh], p = 0.75, na.rm = TRUE)
    strat_dat <- dat[dat[[x]] > cut_var & dat$covidcases > outbreak_thresh,]

    # disprop black
    n_dispblack_ob <- sum(strat_dat$percentblack >= 13, na.rm = TRUE)
    pct_dispblack_ob <- 100*mean(strat_dat$percentblack >= 13, na.rm = TRUE)
    form_n_dispblack_ob <- paste0(n_dispblack_ob, " (", sprintf("%.1f", pct_dispblack_ob), ")")

    deaths_dispblack_ob <- quantile(strat_dat$coviddeaths_rate[strat_dat$percentblack >= 13], p = c(0.5, 0.25, 0.75), na.rm = TRUE)
    format_deaths_dispblack_ob <- paste0(sprintf("%.1f", deaths_dispblack[1]), " (", sprintf("%.1f", deaths_dispblack[2]), ", ", sprintf("%.1f", deaths_dispblack[3]), ")")

    # other 
    n_other_ob <- sum(strat_dat$percentblack < 13, na.rm = TRUE)
    pct_other_ob <- 100*mean(strat_dat$percentblack < 13, na.rm = TRUE)
    form_n_other_ob <- paste0(n_other_ob, " (", sprintf("%.1f", pct_other_ob), ")")
    
    deaths_other_ob <- quantile(strat_dat$coviddeaths_rate[strat_dat$percentblack < 13], p = c(0.5, 0.25, 0.75), na.rm = TRUE)
    format_deaths_other_ob <- paste0(sprintf("%.1f", deaths_other[1]), " (", sprintf("%.1f", deaths_other[2]), ", ", sprintf("%.1f", deaths_other[3]), ")")

    return(c(form_n_dispblack, form_n_other, format_cases_dispblack, format_cases_other,
      format_deaths_dispblack, format_deaths_other, form_n_dispblack_ob, form_n_other_ob,
      format_deaths_dispblack_ob, format_deaths_other_ob))
  }))
  colnames(tab) <- c("n disp. black", "n other", "case rate disp. black",
                     "case rate other", "death rate disp. black", "death rate other",
                     "n disp. black (cases > 200)", "n other (cases > 200)",
                     "death rate disp. black (cases > 200)", 
                     "death rate other (cases > 200)")
  row.names(tab) <- sapply(which_variable, function(v){
    var_label(dat[[v]])
  })
  return(tab)
}


#' summarize model output for table 3
#' @param mod a fitted model
#' @param dat the data used to fit the model
#' @return a matrix with strings of IQR for each variable and 
#' relative risks comparing third to first quartile with 95% credible interval
make_table3_column <- function(mod, dat){
  model_sum <- summary(mod)
  variable_names <- row.names(mod$summary.fixed)[-1]
  model_table <- t(sapply(variable_names, function(var_name){
    # present RR of going from 0.25 to 0.75 quantile
    var_quant <- quantile(dat[[var_name]], p = c(0.25, 0.75), na.rm = TRUE)
    var_jump <- diff(var_quant)
    log_rr_pt_est_ci <- mod$summary.fixed[var_name, c("mean", "0.025quant", "0.975quant")]
    rr_scale_pt_est_ci <- exp(log_rr_pt_est_ci*var_jump)
    return(c(var_quant, rr_scale_pt_est_ci))
  }))
  format_IQR <- apply(model_table[,1:2], 1, function(x){
    paste0("(",sprintf("%.3f", x[1]), ", ", sprintf("%.3f", x[2]),")")
  })
  format_RR <- apply(model_table[,3:5], 1, function(x){
    paste0(sprintf("%.3f", x[1]), " (", sprintf("%.3f", x[2]),", ", sprintf("%.3f", x[3]), ")")
  })
  formatted_model_table <- cbind(format_IQR, format_RR)
  row.names(formatted_model_table) <- sapply(variable_names, function(v){
    var_label(dat[[v]])
  })
  colnames(formatted_model_table) <- c("Inter-quartile range", "Rate ratio (95% CI)")
  return(formatted_model_table)
}

#' make table 3 based on model for cases and model for deaths.
#' see make_table3_column for format
#' @param mod_cases the fitted model for covid cases
#' @param mod_deaths the fitted model for covid deaths
#' @param dat the data used to fit the models
#' @return a matrix with strings of IQR for each variable and 
#' relative risks for cases and deaths comparing third to first 
#' quartile with 95% credible interval
make_table3 <- function(mod_cases, mod_deaths, dat){
  col_cases <- make_table3_column(mod = mod_cases, dat = dat)
  col_deaths <- make_table3_column(mod = mod_deaths, dat = dat)
  # remove IQR for deaths
  tmp <- cbind(col_cases, col_deaths[,-1])
  colnames(tmp) <- c("Inter-quartile range", "Rate ratio cases (95% CI)", 
                     "Rate ratio deaths (95% CI)")
  return(tmp)
}
