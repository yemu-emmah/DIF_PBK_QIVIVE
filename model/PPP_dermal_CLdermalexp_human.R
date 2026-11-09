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

parms_human_dermal_1compt_pre <- parameterize_dermal_pbtk(chem.name = "Difenoconazole", species = "Human", model.type = "dermal_1subcomp", method.permeability =  "UK-Surrey", height = 175, skin.pH = 5.5, InfiniteDose = 0, Kvehicle2water = "octanol", totalSA = NULL,
                                                          tissuelist = list(liver = c("liver"), kidney = c("kidney"), lung = c("lung"), gut =
                                                                              c("gut"), skin = c("skin"))) # "UK-Surrey" (Sawyer et al., 2016 and Chen et al., 2015), which uses Fick’s law of diffusion to calculate P. 

dose.conc <- 250000 #mg/l #in vitro experiment exposure scenario;;; Fskinexposure = 10% in this model 
Vvehicle <- 0.001 #L
initial.dose <- dose.conc*Vvehicle
out_human_dermal_1compt_PPP_pre = solve_dermal_pbtk(chem.name = "Difenoconazole",
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
                                                parameters = parms_human_dermal_1compt_pre,
                                                input.units = "mg",
                                                output.units = "mg/L",
                                                suppress.messages = TRUE)


plot(out_human_dermal_1compt_PPP_pre[, "time"]*24, out_human_dermal_1compt_PPP_pre[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (mg/L)",
     main = "Pre-optimisation",
     lwd = 2,
     ylim = c(0,0.006))





#Initial parameters: Human 
parms_human_dermal_1compt_PPP <- parameterize_dermal_pbtk(chem.name = "Difenoconazole", species = "Human", model.type = "dermal_1subcomp", method.permeability =  "UK-Surrey", height = 175, skin.pH = 5.5, InfiniteDose = 0, Kvehicle2water = "octanol", totalSA = NULL,
                                                          tissuelist = list(liver = c("liver"), kidney = c("kidney"), lung = c("lung"), gut =
                                                                              c("gut"), skin = c("skin"))) # "UK-Surrey" (Sawyer et al., 2016 and Chen et al., 2015), which uses Fick’s law of diffusion to calculate P. 

parms_human_dermal_1compt_PPP$Kliver2pu = parms_human_Kp_0.5_dermal_PPP$Kliver2pu #R&R_human
parms_human_dermal_1compt_PPP$Klung2pu = parms_human_Kp_0.5_dermal_PPP$Klung2pu #R&R_human
parms_human_dermal_1compt_PPP$Kgut2pu = parms_human_Kp_0.5_dermal_PPP$Kgut2pu #R&R_human
parms_human_dermal_1compt_PPP$Kkidney2pu = parms_human_Kp_0.5_dermal_PPP$Kkidney2pu #R&R_human
parms_human_dermal_1compt_PPP$Krest2pu = parms_human_Kp_0.5_dermal_PPP$Krest2pu #R&R_human
parms_human_dermal_1compt_PPP$Kskin2pu =  3.496574 #R&R_human
parms_human_dermal_1compt_PPP$Clint = parms_human_Kp_0.5_dermal_PPP$Clint
parms_human_dermal_1compt_PPP$Clmetabolismc = parms_human_Kp_0.5_dermal_PPP$Clmetabolismc #calc_hep_metabolism
parms_human_dermal_1compt_PPP$Fabsgut = parms_human_Kp_0.5_dermal_PPP$Fabsgut #fitted parameter from human oral model
parms_human_dermal_1compt_PPP$kgutabs = parms_human_Kp_0.5_dermal_PPP$kgutabs #fitted parameter from human oral model
parms_human_dermal_1compt_PPP$pKa_Accept = parms_human_Kp_0.5_dermal_PPP$pKa_Accept #fitted parameter from literature
parms_human_dermal_1compt_PPP$Funbound.plasma = parms_human_Kp_0.5_dermal_PPP$Funbound.plasma #fitted parameter from human oral model
parms_human_dermal_1compt_PPP$totalSA = 16600 #EFSA Guidance document

parms_human_dermal_1compt_PPP$P = 4.6e-07

dose.conc <- 250000 #mg/l #in vitro experiment exposure scenario;;; Fskinexposre = 10% in this model 
Vvehicle <- 0.001 #L
initial.dose <- dose.conc*Vvehicle
out_human_dermal_1compt_PPP = solve_dermal_pbtk(chem.name = "Difenoconazole",
                                                species = "Human",
                                                model.type = "dermal_1subcomp",
                                                method.permeability = "UK-Surrey",
                                                Kvehicle2water = NULL,
                                                Vvehicle = Vvehicle,
                                                initial.dose = initial.dose,
                                                dose.duration = 6,
                                                dose.duration.units = "hr",
                                                washoff = TRUE,
                                                days = 10,
                                                tsteps = 0.0001,
                                                parameters = parms_human_dermal_1compt_PPP,
                                                input.units = "mg",
                                                output.units = "mg/L",
                                                suppress.messages = TRUE)


plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Plasma concentration (ug/L)",
     main = "Post-optimisation",
     lwd = 2,
     ylim = c(0,0.15))


max(out_human_dermal_1compt_PPP[, "Aplasma"])
max(out_human_dermal_1compt_PPP[, "Cplasma"])


#plotting graphs on one page 

par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))  
plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cskin_exposed"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cskin_exposed, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cliver"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cliver, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cgut"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cgut, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Clung"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Clung, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Ckidney"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Ckidney, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cven"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cven, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cart"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cart, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Crest"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Crest, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Cskin_unexposed"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Cskin_unexposed, dermal_PPP/6days")

plot(out_human_dermal_1compt_PPP[, "time"]*24, out_human_dermal_1compt_PPP[,"Aplasma"],
     type = "l",
     xlab = "Time (hr)",
     ylab = "Concentration (mg/L)",
     main = "Aplasma, dermal_PPP/6days")



