#' a function to draw from posterior of fitted model
#' @param n_draws number of posterior draws
#' @param model the fitted model
#' @return a list with name entries fixed_effects, random_effects,
#' and zero_probability

posterior_samples<-function(n_draws,model){
  samps<-inla.posterior.sample(n_draws,model)
  
  contents<-model$misc$configs$contents
  
  # -- fixed effects -- #
  effects<-rownames(model$summary.fixed)
  
  fixed_effects=which(contents$tag%in%effects)
  
  ind.effect = contents$start[fixed_effects]
  
  samples = lapply(samps, function(x) x$latent[ind.effect])
  
  fixed_effect_samples = matrix(unlist(samples), byrow = T, nrow = length(samples))
  
  colnames(fixed_effect_samples) = rownames(samps[[1]]$latent)[ind.effect]

  # -- random effects -- #  
  effect = "id"
  id.effect = which(contents$tag==effect)
  # - the numerical id of the effect
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])  
  samples.effect = lapply(samps, function(x) x$latent[ind.effect])
  r.eff = matrix(unlist(samples.effect), byrow = T, nrow = length(samples.effect))
  # - s.eff means samples.effect, just as a matrix
  colnames(r.eff) = rownames(samps[[1]]$latent)[ind.effect]

  # -- zero prob -- #
  zero_prob <- unlist(lapply(samps, function(x){ x$hyperpar["zero-probability parameter for zero-inflated nbinomial_1"] }),
                      use.names = FALSE)

  return(list(fixed_effects=fixed_effect_samples,
              random_effects = r.eff, zero_prob = zero_prob))
}

#' fit a bayesian hierarchical zero inflated negative binomial1 model with a particular 
#' error structure and return draws from the posterior of the fixed effects, random
#' effects and probability of zeros
#' @param dat the data used to fit the model
#' @param states the state shape file
#' @param model_outcome the LHS variable of model
#' @param model_formula the RHS formula of the model
#' @param err_str either "spatial" or "none"
#' @param n_draws how many draws from the posterior distribution to return
#' @return a list that includes the inla model object (mod) 
#' and posterior samples (post_samps, see posterior_samples for structure)

fit_model <- function(dat, states, 
                      model_outcome, 
                      model_formula,
                      err_str = "spatial",
                      n_draws = 10e3){
      model_formula <- formula(paste0(model_outcome, 
                                            "~ 1 + ",
                                            ifelse(err_str == "spatial", 
                                                   "f(id, model='bym2', graph=mat) + ",
                                                   "f(id) + "),
                                            model_formula))

      mod <- inla(model_formula,
                  family = "zeroinflatednbinomial1",
                  offset = log(population),
                  control.compute = list(config = TRUE),
                  data = dat)

      post_samps <- posterior_samples(n_draws = n_draws, model = mod)

      return(list(mod = mod,
                  post_samps = post_samps))
}