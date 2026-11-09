library(httk)
library(data.table)
library(ggplot2)
#install.packages("writexl")
library(writexl)

nsamp <- 1000
chem.name <- "Difenoconazole"
dose.conc <- 250000 #mg/l #in vitro experiment exposure scenario;;; Fskinexposre = 10% in this model 
Vvehicle <- 0.001 #L
initial.dose <- dose.conc*Vvehicle

# Base parameter set
pars_base <- parms_human_dermal_1compt_PPP


# Exclude physiological parameters 
fixed_pars <- c("pKa_Donor", 
                "pKa_Accept", 
                "MW", 
                "BW", 
                "hematocrit", 
                "InfiniteDose", 
                "Qcardiacc", 
                "Qgfrc", 
                "Qgutf",
                "Qalvc", 
                "Pow",
                "Qkidneyf", 
                "Qliverf", 
                "Qskinf", 
                "totalSA", 
                "Vartc", 
                "Vgutc", 
                "Vkidneyc", 
                "Vliverc", 
                "Vlungc", 
                "Vrestc", 
                "Vvenc", 
                "Vskinc", 
                "million.cells.per.gliver", 
                "liver.density",
                "skin_depth"
)

chem_pars <- setdiff(names(pars_base), fixed_pars)

chem_pars #checking if the correct parameters are being varied

#storage

out_list <- vector("list", nsamp)
param_samples <- vector("list", nsamp)

#Monte carlo
set.seed(1234) #ensure reproducibility

for(i in seq_len(nsamp)) {
  
  p <- pars_base
  
  # Vary each parameter cv = 0.2
  for(par in chem_pars) {
    
    val <- p[[par]]
    
    if(is.numeric(val) &&
       length(val) == 1 &&
       !is.na(val) &&
       is.finite(val) &&
       val > 0) {
      
      p[[par]] <- val * rlnorm (1, meanlog = 0, sdlog = 0.2) #lognormal distribution
      
    }
  }
  param_samples [[i]] <- unlist(p)
  # Run PBPK model
  out_list[[i]] <- solve_dermal_pbtk("Difenoconazole",
                                     species = "Human",
                                     model.type = "dermal_1subcomp",
                                     method.permeability = "UK-Surrey",
                                     Vvehicle = Vvehicle,
                                     Kvehicle2water = NULL,
                                     initial.dose = initial.dose,  
                                     dose.duration = 6,
                                     tsteps = 0.0001,
                                     dose.duration.units = "hr",
                                     days = 10,
                                     parameters = p,
                                     input.units = "mg",
                                     output.units = "mg/l",
                                     suppress.messages = TRUE
  )
}

#Extracting Cplasma

timevec <- out_list[[1]][, "time"]

Cmat <- sapply(
  out_list,
  function(x) x[, "Cplasma"]
)

#percentiles

plot_dt_PPP <- data.table(
  time = timevec,
  p5  = apply(Cmat, 1, quantile, probs = 0.05, na.rm = TRUE),
  p50 = apply(Cmat, 1, quantile, probs = 0.50, na.rm = TRUE),
  p95 = apply(Cmat, 1, quantile, probs = 0.95, na.rm = TRUE)
)

#Plotting

PPP = ggplot(plot_dt_PPP, aes(x = time*24)) +
  geom_ribbon(
    aes(ymin = p5, ymax = p95),
    alpha = 0.3
  ) +
  geom_line(
    aes(y = p50),
    linewidth = 1
  ) +
  labs(
    title = "C    Post-optimisation
    (Formulated product)",
    x = "Time (hr)",
    y = "Plasma concentration (mg/L)"
  ) +
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(),
    colour = "black",
    fill = NA,
    linewidth = 1.5,
    plot.title = element_text(hjust = 0.5)
  )

PPP

p5_PPP  = apply(Cmat, 1, quantile, probs = 0.05, na.rm = TRUE)
p50_PPP = apply(Cmat, 1, quantile, probs = 0.50, na.rm = TRUE)
p95_PPP = apply(Cmat, 1, quantile, probs = 0.95, na.rm = TRUE)

p5_PPP
p50_PPP
p95_PPP

p5_Cmax_PPP = max(p5_PPP)
p5_Cmax_PPP

p50_Cmax_PPP = max(p50_PPP)
p50_Cmax_PPP

p95_Cmax_PPP = max(p95_PPP)
p95_Cmax_PPP








#Creating parameter table

param_df <- do.call(
  rbind,
  lapply(seq_along(param_samples), function(i){
    
    vals <- unlist(param_samples[[i]])
    
    data.frame(
      Iteration = i,
      t(vals),
      check.names = FALSE
    )
  })
)

head(param_df)

View(param_df)

write_xlsx(param_df,
           "C:.xlsx") #save table as excel













