#install.packages("httk")
library("httk")
#install.packages("stringr")
#install.packages("stringi")
library("stringr")
library("stringi")
#install.packages("readxl")
library("readxl")
#install.packages("pksensi")
library("pksensi")


#Initial parameters: Rat
parms_rat = parameterize_pbtk(chem.name = "Difenoconazole", species = "Rat", default.to.human=TRUE)

out_DIF_rat_0.5 = solve_pbtk(chem.name = "Difenoconazole",
                             species = "Rat",
                             dose = 0.5,
                             input.units = "mg/kg",
                             output.units = "mg/L",
                             days = 168/24,
                             default.to.human=TRUE,
                             median.only = TRUE,
                             suppress.messages = TRUE,
                             iv.dose = FALSE)


plot(out_DIF_rat_0.5 [, "time"]*24, out_DIF_rat_0.5 [,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "Pre-optimisation",
     lwd = 2,
     ylim = c(0,0.6))

max(out_DIF_rat_0.5[, "Cplasma"])

my.data_0.5 <- data.frame(read_excel("I:/Yemu/PK/R_DIF_PPP model/R_scripts/DIF_0.5mg.xlsx"))


points(my.data_0.5[, "Time"], my.data_0.5[, "Plasma.Concentration"],
       col = "red",
       pch = 14,
       cex = 1.2)

legend("topright",
       legend = c("Simulation", "Observed data"),
       col = c("black", "red"),
       lty = c(1, NA),
       pch = c(NA, 0),
       lwd = c(2, NA))




#calculate AUC
auc_lin <- function(time, conc) {
  sum(diff(time) * (head(conc, -1) + tail(conc, -1)) / 2)
}

df_rat_pre <- as.data.frame(out_DIF_rat_0.5)
df_rat_pre$time <- df_rat_pre$time*24


auc_0_t_pre <- auc_lin(df_rat_pre$time, df_rat_pre$Cplasma)
print(auc_0_t_pre)










parms_rat_Kp_0.5 = parameterize_pbtk(chem.name = "Difenoconazole", species = "Rat", default.to.human=TRUE)
parms_rat_Kp_0.5$Kliver2pu <- 2.772784 #R&R_rat
parms_rat_Kp_0.5$Kgut2pu <- 5.454685 #R&R_rat
parms_rat_Kp_0.5$Klung2pu <- 3.378635 #R&R_rat
parms_rat_Kp_0.5$Kkidney2pu <- 2.519796 #R&R_rat
parms_rat_Kp_0.5$Krest2pu <- 15.13005 #R&R_rat
parms_rat_Kp_0.5$Clmetabolismc <- 0.015 #fitted parameter
parms_rat_Kp_0.5$Fabsgut <- 0.19 #fitted parameter 
parms_rat_Kp_0.5$kgutabs <- 0.9 #fitted parameter (U.S.EPA (3/23/2021))
parms_rat_Kp_0.5$pKa_Accept <- 1.07 

#httk model parameter: parms_rat_Kp_0.5$Funbound.plasma <- 0.008234  


out_DIF_rat_Kp_0.5 = solve_pbtk(chem.name = "Difenoconazole",
                                species = "Rat",
                                dose = 0.5,
                                parameters = parms_rat_Kp_0.5,
                                input.units = "mg/kg",
                                output.units = "mg/L",
                                days = 168/24,
                                default.to.human= FALSE,
                                suppress.messages = TRUE,
                                iv.dose = FALSE)

plot(out_DIF_rat_Kp_0.5[, "time"]*24, out_DIF_rat_Kp_0.5[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "Post-optimisation",
     lwd = 2,
     ylim = c(0,1.0))

points(my.data_0.5[, "Time"], my.data_0.5[, "Plasma.Concentration"],
       col = "red",
       pch = 14,
       cex = 1.2)

legend("topright",
       legend = c("Simulation", "Observed data"),
       col = c("black", "red"),
       lty = c(1, NA),
       pch = c(NA, 0),
       lwd = c(2, NA))





#calculate AUC
auc_lin <- function(time, conc) {
  sum(diff(time) * (head(conc, -1) + tail(conc, -1)) / 2)
}

df_rat <- as.data.frame(out_DIF_rat_Kp_0.5)
df_rat$time <- df_rat$time*24


auc_0_t <- auc_lin(df_rat$time, df_rat$Cplasma)
print(auc_0_t)




auc_0_t_obs <- auc_lin(my.data_0.5$Time, my.data_0.5$Plasma.Concentration)
print(auc_0_t_obs)



#######Plotting for paper#######

par(mfrow = c(2, 2), mar = c(4, 4, 4, 1)) 


plot(out_DIF_rat_0.5 [, "time"]*24, out_DIF_rat_0.5 [,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "A    Pre-optimisation",
     lwd = 2,
     ylim = c(0,0.6))
points(my.data_0.5[, "Time"], my.data_0.5[, "Plasma.Concentration"],
       col = "red",
       pch = 14,
       cex = 1.2)

legend("topright",
       legend = c("Simulation", "Observed data"),
       col = c("black", "red"),
       lty = c(1, NA),
       pch = c(NA, 0),
       lwd = c(2, NA))



plot(out_DIF_rat_Kp_0.5[, "time"]*24, out_DIF_rat_Kp_0.5[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "B    Post-optimisation",
     lwd = 2,
     ylim = c(0,1.0))
points(my.data_0.5[, "Time"], my.data_0.5[, "Plasma.Concentration"],
       col = "red",
       pch = 14,
       cex = 1.2)

legend("topright",
       legend = c("Simulation", "Observed data"),
       col = c("black", "red"),
       lty = c(1, NA),
       pch = c(NA, 0),
       lwd = c(2, NA))




#plotting graphs on one page 

df_rat <- as.data.frame(out_DIF_rat_Kp_0.5)
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))  

plot(df_rat$time, df_rat$Cplasma, type = "l", main = "Cplasma, oral 7D_rat",
     xlab = "time (hr)", ylab = "Cplasma (mg/L)")

plot(df_rat$time, df_rat$Cliver, type = "l", main = "Cliver, oral 7D_rat",
     xlab = "time (hr)", ylab = "Cliver (mg/L)")

plot(df_rat$time, df_rat$Cgut, type = "l", main = "Cgut, oral 7D_rat",
     xlab = "time (hr)", ylab = "Cgut (mg/L)")

plot(df_rat$time, df_rat$Clung, type = "l", main = "Clung, oral 7D_rat",
     xlab = "time (hr)", ylab = "Clung (mg/L)")

plot(df_rat$time, df_rat$Ckidney, type = "l", main = "Ckidney, oral 7D_rat",
     xlab = "time (hr)", ylab = "Ckidney (mg/L)")

plot(df_rat$time, df_rat$Cven, type = "l", main = "Cven, oral 7D_rat",
     xlab = "time (hr)", ylab = "Cven (mg/L)")

plot(df_rat$time, df_rat$Cart, type = "l", main = "Cart, oral 7D_rat",
     xlab = "time", ylab = "Cart")

plot(df_rat$time, df_rat$Crest, type = "l", main = "Crest, oral 7D_rat",
     xlab = "time (hr)", ylab = "Crest (mg/L)")


