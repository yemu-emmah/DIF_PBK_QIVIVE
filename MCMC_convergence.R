################################################################ MCMC CONVERGENCE DIAGNOSTICS###############################################################
library(coda)

# Convert to MCMC object


post_mcmc <- mcmc(post)
nchain(post_mcmc)
################################################################ 1. TRACE PLOT (most important)###############################################################
plot(post_mcmc,     
     main = "Trace Plot (Dose MCMC)",     
     xlab = "Iteration",     
     ylab = "Dose (mg/kg bw)")
abline(v = burn, col = "red")
legend("topright",
       legend = "Burn-in cutoff",
       col = "red",
       lwd = 2)

plot(dose_chain, type = "l")
################################################################ 2. POSTERIOR DISTRIBUTION###############################################################

hist(post,     
     breaks = 40,     
     main = "Posterior Distribution of Dose",     
     xlab = "Dose (mg/kg bw)")

################################################################ 3. AUTOCORRELATION###############################################################

autocorr.plot(post_mcmc,              
              main = "Autocorrelation of MCMC Chain")


##########################################################################################################################################################