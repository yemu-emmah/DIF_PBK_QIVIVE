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
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)

parms_human_dermal_1compt_dif <- parameterize_dermal_pbtk(chem.name = "Difenoconazole", species = "Human", model.type = "dermal_1subcomp", method.permeability =  "UK-Surrey", height = 175, skin.pH = 5.5, InfiniteDose = 0, Kvehicle2water = "octanol", totalSA = NULL,
                                                          tissuelist = list(liver = c("liver"), kidney = c("kidney"), lung = c("lung"), gut =
                                                                              c("gut"), skin = c("skin"))) # "UK-Surrey" (Sawyer et al., 2016 and Chen et al., 2015), which uses Fick’s law of diffusion to calculate P. 

dose.conc <- 250000 #mg/l #in vitro experiment exposure scenario;;; Fskinexposre = 10% in this model 
Vvehicle <- 0.001 #L
initial.dose <- dose.conc*Vvehicle
out_human_dermal_1compt_dif_pre = solve_dermal_pbtk(chem.name = "Difenoconazole",
                                                species = "Human",
                                                model.type = "dermal_1subcomp",
                                                method.permeability = "UK-Surrey",
                                                Vvehicle = Vvehicle,
                                                Kvehicle2water = NULL,
                                                initial.dose = initial.dose,
                                                dose.duration = 6,
                                                tsteps = 0.0001,
                                                dose.duration.units = "hr",
                                                washoff = TRUE,
                                                days = 10,
                                                parameters = parms_human_dermal_1compt_dif,
                                                input.units = "mg",
                                                output.units = "mg/L",
                                                suppress.messages = TRUE)


plot(out_human_dermal_1compt_dif_pre[, "time"]*24, out_human_dermal_1compt_dif_pre[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "Pre-optimisation",
     lwd = 2,
     ylim = c(0,0.006))



#Initial parameters: Human 
parms_human_dermal_1compt_dif <- parameterize_dermal_pbtk(chem.name = "Difenoconazole", species = "Human", model.type = "dermal_1subcomp", method.permeability =  "UK-Surrey", height = 175, skin.pH = 5.5, InfiniteDose = 0, Kvehicle2water = "octanol", totalSA = NULL,
                                                      tissuelist = list(liver = c("liver"), kidney = c("kidney"), lung = c("lung"), gut =
                                                                          c("gut"), skin = c("skin"))) # "UK-Surrey" (Sawyer et al., 2016 and Chen et al., 2015), which uses Fick’s law of diffusion to calculate P. 

parms_human_dermal_1compt_dif$Kliver2pu = parms_human_Kp_0.5_dermal$Kliver2pu #R&R_human
parms_human_dermal_1compt_dif$Klung2pu = parms_human_Kp_0.5_dermal$Klung2pu #R&R_human
parms_human_dermal_1compt_dif$Kgut2pu = parms_human_Kp_0.5_dermal$Kgut2pu #R&R_human
parms_human_dermal_1compt_dif$Kkidney2pu = parms_human_Kp_0.5_dermal$Kkidney2pu #R&R_human
parms_human_dermal_1compt_dif$Krest2pu = parms_human_Kp_0.5_dermal$Krest2pu #R&R_human
parms_human_dermal_1compt_dif$Kskin2pu = 3.496574 #R&R_human
parms_human_dermal_1compt_dif$Clint = parms_human_Kp_0.5_dermal$Clint #fitted parameter from human oral model
parms_human_dermal_1compt_dif$Clmetabolismc = parms_human_Kp_0.5_dermal$Clmetabolismc #calc_hep_metabolism
parms_human_dermal_1compt_dif$Fabsgut = parms_human_Kp_0.5_dermal$Fabsgut #fitted parameter from human oral model
parms_human_dermal_1compt_dif$kgutabs = parms_human_Kp_0.5_dermal$kgutabs #fitted parameter from human oral model
parms_human_dermal_1compt_dif$pKa_Accept = parms_human_Kp_0.5_dermal$pKa_Accept #fitted parameter from literature
parms_human_dermal_1compt_dif$Funbound.plasma = parms_human_Kp_0.5_dermal$Funbound.plasma #default httk
parms_human_dermal_1compt_dif$totalSA = 16600 #EFSA Guidance document




dose.conc <- 250000 #mg/l #in vitro experiment exposure scenario;;; Fskinexposre = 10% in this model 
Vvehicle <- 0.001 #L
initial.dose <- dose.conc*Vvehicle
out_human_dermal_1compt_dif = solve_dermal_pbtk(chem.name = "Difenoconazole",
                                     species = "Human",
                                     model.type = "dermal_1subcomp",
                                     method.permeability = "UK-Surrey",
                                     Vvehicle = Vvehicle,
                                     Kvehicle2water = NULL,
                                     initial.dose = initial.dose,
                                     dose.duration = 6,
                                     tsteps = 0.0001,
                                     dose.duration.units = "hr",
                                     washoff = TRUE,
                                     days = 10,
                                     parameters = parms_human_dermal_1compt_dif,
                                     input.units = "mg",
                                     output.units = "mg/L",
                                     suppress.messages = TRUE)


plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "Post-optimisation",
     lwd = 2,
     ylim = c(0,0.15))

max(out_human_dermal_1compt_dif[, "Aplasma"])
max(out_human_dermal_1compt_dif[, "Cplasma"])

#######Plotting for paper#######

par(mfrow = c(2, 2), mar = c(4, 4, 4, 1)) 


plot(out_human_dermal_1compt_dif_pre[, "time"]*24, out_human_dermal_1compt_dif_pre[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "A    Pre-optimisation",
     lwd = 2,
     ylim = c(0,0.006))

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "B    Post-optimisation
     (DIF)",
     lwd = 2,
     ylim = c(0,0.15))

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "C    Post-optimisation
     (Formulated product)",
     lwd = 2,
     ylim = c(0,0.15))











#plotting graphs on one page 

par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))  
plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cskin_exposed"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cskin_exposed, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cliver"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cliver, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cgut"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cgut, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Clung"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Clung, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Ckidney"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Ckidney, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cven"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cven, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cart"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cart, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Crest"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Crest, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Cskin_unexposed"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cskin_unexposed, dermal_dif/10days")

plot(out_human_dermal_1compt_dif[, "time"]*24, out_human_dermal_1compt_dif[,"Aplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Aplasma, dermal_dif/10days")





