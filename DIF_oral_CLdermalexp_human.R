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


#Initial parameters: Human 
parms_human = parameterize_pbtk(chem.name = "Difenoconazole", species = "Human")

out_DIF_human_0.5_dermal = solve_pbtk(chem.name = "Difenoconazole",
                               species = "Human",
                               dose = 0.5,
                               input.units = "mg/kg",
                               output.units = "mg/L",
                               days = 168/24,
                               median.only = TRUE,
                               suppress.messages = TRUE,
                               iv.dose = FALSE)


plot(out_DIF_human_0.5_dermal [, "time"]*24, out_DIF_human_0.5_dermal [,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma oncentration (mg/L)",
     main = "Pre-optimisation",
     lwd = 2,
     ylim = c(0,0.2))

parms_human_Kp_0.5_dermal = parameterize_pbtk(chem.name = "Difenoconazole", species = "Human")
parms_human_Kp_0.5_dermal$Kliver2pu <- 2.770786 #R&R_human
parms_human_Kp_0.5_dermal$Kgut2pu <- 3.589873 #R&R_human
parms_human_Kp_0.5_dermal$Klung2pu <- 0.3235977 #R&R_human
parms_human_Kp_0.5_dermal$Kkidney2pu <- 2.807159 #R&R_human
parms_human_Kp_0.5_dermal$Krest2pu <- 7.55033 #R&R_human, used Kp for adipose tissue
parms_human_Kp_0.5_dermal$Clint <- 42.42 #fitted parameter from in vitro dermal experiment
parms_human_Kp_0.5_dermal$Fabsgut <- 0.19 #fitted parameter from rat-optimised model. Hepatic Clearance, L/h/kg BW.
parms_human_Kp_0.5_dermal$kgutabs <- 0.9 #fitted parameter from rat-optimised model (U.S.EPA (3/23/2021))
parms_human_Kp_0.5_dermal$pKa_Accept <- 1.07 #fitted parameter from literature
parms_human_Kp_0.5_dermal$Funbound.plasma <- 0.004317 #httk default

calc_hep_clearance(chem.name = "Difenoconazole",
                   parameters = parms_human_Kp_0.5_dermal,
                   species = "Human",
                   default.to.human = FALSE,
                   hepatic.model = "well-stirred",
                   suppress.messages = FALSE,
                   well.stirred.correction = TRUE,
                   restrictive.clearance = TRUE,
                   adjusted.Funbound.plasma = TRUE)

parms_human_Kp_0.5_dermal$Clmetabolismc <- 0.02991 #calc_hep_clearance

#httk model parameter: parms_human_Kp_0.5$Funbound.plasma <- 0.004317  

out_DIF_human_Kp_0.5_dermal = solve_pbtk(chem.name = "Difenoconazole",
                                  species = "Human",
                                  dose = 0.5,
                                  parameters = parms_human_Kp_0.5_dermal,
                                  input.units = "mg/kg",
                                  output.units = "mg/L",
                                  days = 168/24,
                                  suppress.messages = TRUE,
                                  iv.dose = FALSE)


plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "Post-optimisation",
     lwd = 2,
     ylim = c(0,1))


max(out_DIF_human_Kp_0.5_dermal[, "Cplasma"])



#######Plotting for paper#######

par(mfrow = c(2, 2), mar = c(4, 4, 4, 1)) 


plot(out_DIF_human_0.5_dermal [, "time"]*24, out_DIF_human_0.5_dermal [,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "A    Pre-optimisation",
     lwd = 2,
     ylim = c(0,0.2))


plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "B    Post-optimisation
     (DIF)",
     lwd = 2,
     ylim = c(0,1))

plot(out_PPP_human_Kp_0.5_dermal[, "time"]*24, out_PPP_human_Kp_0.5_dermal[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "C    Post-optimisation
     (Formulated product)",
     lwd = 2,
     ylim = c(0,1))



#plotting graphs on one page 
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))  

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Cliver"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cliver, dermal_PPP/6days")

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Cgut"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cgut, dermal_PPP/6days")

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Clung"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Clung, dermal_PPP/6days")

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Ckidney"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Ckidney, dermal_PPP/6days")

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Cven"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cven, dermal_PPP/6days")

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Cart"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cart, dermal_PPP/6days")

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Crest"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Crest, dermal_PPP/6days")

plot(out_DIF_human_Kp_0.5_dermal[, "time"]*24, out_DIF_human_Kp_0.5_dermal[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cplasma, dermal_PPP/6days")



