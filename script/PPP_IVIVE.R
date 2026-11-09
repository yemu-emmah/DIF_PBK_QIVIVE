library(httk)

###############################################################
# USER INPUTS
###############################################################

BMC   <- 0.04   # mg/L
sigma <- 0.05  ###
BW    <- 70

n_iter <- 5000

dose.conc <- 250000 #mg/l #in vitro experiment exposure scenario;;; Fskinexposre = 10% in this model 
Vvehicle <- 0.001 #L
initial.dose <- dose.conc*Vvehicle


###############################################################
# FORWARD MODEL
###############################################################

predict_Cmax <- function(dose_mgkg){
  
  dose_mg <- dose_mgkg * BW
  
  sim <- tryCatch({
    solve_dermal_pbtk(
      chem.name = "Difenoconazole",
      species = "Human",
      model.type = "dermal_1subcomp",
      method.permeability = "UK-Surrey",
      initial.dose = dose_mg,
      Vvehicle = Vvehicle,
      dose.duration = 6,
      dose.duration.units = "hr",
      days = 10,
      parameters = parms_human_dermal_1compt_PPP,
      input.units = "mg",
      output.units = "mg/L",
      suppress.messages = TRUE
    )
  }, error = function(e) return(NULL))
  
  if (is.null(sim)) return(NA)
  
  sim <- as.data.frame(sim)
  
  if (!"Cplasma" %in% names(sim)) return(NA)
  max(sim$Cplasma, na.rm = TRUE)
}

###############################################################
# LIKELIHOOD (LOG-NORMAL)
###############################################################

log_likelihood <- function(dose_mgkg, sigma, BMC){
  
  if (dose_mgkg <= 0) return(-Inf)
  
  pred <- predict_Cmax(dose_mgkg)
  
  if (!is.finite(pred) || pred <= 0) return(-Inf)
  
  # log-normal likelihood. Model assumption: log(BMC) ~ Normal(log(Cmax_pred(dose)), sigma^2)
  dnorm(log(BMC), mean = log(pred), sd = sigma, log = TRUE)
}

###############################################################
# PRIOR
###############################################################

log_prior <- function(dose_mgkg){
  if (dose_mgkg <= 0) return(-Inf)
  dunif(dose_mgkg, 1e-4, 500, log = TRUE)
}

###############################################################
# POSTERIOR
###############################################################

log_posterior <- function(dose_mgkg, sigma, BMC){
  log_likelihood(dose_mgkg, sigma, BMC) +
    log_prior(dose_mgkg)
}

###############################################################
# MCMC 
###############################################################

run_mcmc <- function(BMC, sigma, n_iter = 5000){
  
  set.seed(42)
  
  cat("====================================\n")
  cat(" Difenoconazole IVIVE MCMC\n")
  cat("====================================\n")
  
  chain <- numeric(n_iter)
  
  current <- 0.5
  current_post <- log_posterior(current, sigma, BMC)
  
  if (!is.finite(current_post))
    current_post <- -1e10
  
  accept <- 0
  
  prop_sd <- 0.2
  
  for (i in 1:n_iter){
    
    proposal <- rnorm(1, current, prop_sd)
    
    prop_post <- log_posterior(proposal, sigma, BMC)
    
    if (!is.finite(prop_post)) prop_post <- -Inf
    
    if (log(runif(1)) < (prop_post - current_post)) {
      current <- proposal
      current_post <- prop_post
      accept <- accept + 1
    }
    
    chain[i] <- current
    
    if (i %% 500 == 0) {
      cat(sprintf(
        "Iter %d | Dose = %.4f mg/kg | Accept = %.1f%%\n",
        i, current, 100 * accept / i
      ))
    }
  }
  
  chain
}

###############################################################
# RUN
###############################################################

dose_chain <- run_mcmc(BMC, sigma, n_iter)

burn <- 1000
post <- dose_chain[(burn+1):n_iter]

###############################################################
# RESULTS
###############################################################

cat("\nRESULTS (mg/kg bw)\n")
cat("-------------------\n")

cat("Median dose:", median(post), "mg/kg bw\n")
print(quantile(post, c(0.025, 0.975)))

###############################################################
# FORWARD VALIDATION 
###############################################################

best_dose <- median(post)

Cmax_pred <- predict_Cmax(best_dose)

validation_df <- data.frame(
  Dose_mgkg = best_dose,
  Cmax_pred = Cmax_pred,
  BMC_obs   = BMC,
  Fold_diff = Cmax_pred / BMC
)

print(validation_df)

###############################################################
# PLOT
###############################################################

hist(post,
     breaks = 40,
     main = "Posterior Distribution",
     xlab = "Dose (mg/kg BW)")


###############################################################
# PLOT WITH LINE
###############################################################

hist(post,
     breaks = 40,
     probability = TRUE,
     main = "Posterior Distribution",
     xlab = "Dose (mg/kg BW)")

lines(density(post),
      col = "red",
      lwd = 2)





