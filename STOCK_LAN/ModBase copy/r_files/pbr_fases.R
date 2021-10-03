rm(list=ls())
# Funciones y Directorios ####
library(rstudioapi)
library(ggplot2)
library(reshape)
library(ggpubr)
library(devtools)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

devtools::source_url("https://github.com/ale-yanez/RFunctions/blob/master/read.admb.R?raw=TRUE")

out1 <- read.admb("../output/Lam")
std1 <- read.table('../output/Lam.std', header = T, sep = '', na='NA', fill = T)


# # Para graficar ... ####
 yrs <- out1$YRS
 Brms <- out1$BDoLP*0.4
 Frms <- out1$Fpbr[3]
 B0 <- out1$BDoLP

 # #predichos y estimados 
 predD         <- out1$Desemb[2,]
 Rec_est1      <- subset(std1,name=='Restim')$value
 BT_est1       <- subset(std1,name=='BT')$value
 BD_est1       <- subset(std1,name=='BD')$value
 F_est1        <- exp(subset(std1,name=='log_Fh')$value)

 # # std 
 stdBD1        <- subset(std1,name=='BD')$std
 stdF1         <- subset(std1,name=='log_Fh')$std
 

 # # Confidence Intervals
 BD1_lwr       <-BD_est1-1.96*stdBD1
 BD1_upr       <-BD_est1+1.96*stdBD1
 #F1_lwr        <-exp(log(F_est1)-1.96*stdF1)
 #F1_upr        <-exp(log(F_est1)+1.96*stdF1)
 F1_lwr        <-F_est1-sqrt((F_est1^2)*(stdF1^2))
 F1_upr        <-F_est1+sqrt((F_est1^2)*(stdF1^2))
 
 
# Plots y Text ####
dir()

# PBRs y variables de estado ####

p11_2 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_est1, colour = 'actual', linetype = 'actual')) +
  geom_ribbon(data=NULL, aes(ymin=BD1_lwr, ymax=BD1_upr), fill = 'grey60', alpha = 0.4) + 
  geom_line(aes(y = c(rep(Brms,34)), colour = 'Brms', linetype = 'Brms')) +
  geom_line(aes(y = c(rep(B0,34)), colour = 'BDo', linetype = 'BDo')) +
   annotate("text", x=1992, y=4200, label="BDo") +
   annotate("text", x=1992, y=1600, label="Brms") +
   
  scale_color_manual(name = '',
                     values = c('royalblue3', 'chartreuse3', 'black'),
                     limits = c('actual',  'Brms', 'BDo'),
                     breaks = c('actual',  'Brms', 'BDo')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash','dotted'),
                        limits = c('actual', 'Brms', 'BDo'),
                        breaks = c('actual', 'Brms', 'BDo'))

p11_2 <- p11_2 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = '') + ylab('Biomasa Desovante (t)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2021, by = 2),1))

p11_2



p12_2 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1, colour = 'actual', linetype = 'actual')) +
  geom_ribbon(data=NULL, aes(ymin=F1_lwr, ymax=F1_upr), fill = 'grey60', alpha = 0.4) +
  geom_line(aes(y = c(rep(Frms,34)), colour = 'Frms', linetype = 'Frms')) +
  annotate("text", x=1996, y=0.34, label="Frms") +
  
  scale_color_manual(name = '',
                     values = c('royalblue3','red'),
                     limits = c('actual', 'Frms'),
                     breaks = c('actual', 'Frms')) +
  scale_linetype_manual(name = '',
                        values = c('solid','dotted'),
                        limits = c('actual', 'Frms'),
                        breaks = c('actual',  'Frms'))

p12_2 <- p12_2 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = '') + ylab('Mortalidad por Pesca (1/años)') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2021, by = 2),1))

p12_2

p_1 <- ggarrange(p11_2, p12_2,
                 ncol = 2, nrow = 1)



p13 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = BD_est1/Brms, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(rep(1,34)), colour = 'Brms', linetype = 'Brms')) +
  annotate("text", x=1992, y=0.9, label="Brms") +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'chartreuse3'),
                     limits = c('actual',  'Brms'),
                     breaks = c('actual',  'Brms')) +
  scale_linetype_manual(name = '',
                        values = c('solid', 'twodash'),
                        limits = c('actual', 'Brms'),
                        breaks = c('actual', 'Brms'))

p13 <- p13 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = '') + ylab('BD/BDrms') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2021, by = 2),1))

p13


p14 <- ggplot(data = NULL, aes(x = yrs)) + 
  geom_line(aes(y = F_est1/Frms, colour = 'actual', linetype = 'actual')) +
  geom_line(aes(y = c(rep(1,34)), colour = 'Frms', linetype = 'Frms')) +
  annotate("text", x=1986, y=0.9, label="Frms") +
  scale_color_manual(name = '',
                     values = c('royalblue3', 'red'),
                     limits = c('actual',  'Frms'),
                     breaks = c('actual',  'Frms')) +
   scale_linetype_manual(name = '',
                        values = c('solid', 'dotted'),
                        limits = c('actual', 'Frms'),
                        breaks = c('actual', 'Frms'))

p14 <- p14 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = '') + ylab('F/Frms') + xlab('Años') + 
  scale_x_continuous(breaks=round(seq(min(yrs), 2021, by = 2),1))

p14

p_2 <- ggarrange(p13, p14, ncol = 2, nrow = 1)

plot <- ggarrange(p_1, p_2, ncol = 1, nrow = 2, align = "h", common.legend = F, legend = "bottom")
#ggexport(plot, filename = "PBRs.pdf", width=7.5, height=6, dpi=300)
ggsave(plot, filename = "../figures/PBRs.png", width=7.5, height=6, dpi=300)


# txt ####

VarPobl<- cbind(years=yrs, BD=BD_est1, BT=BT_est1, R=Rec_est1, F_est=F_est1, "F/FRMS"=F_est1/Frms, "BD/BDRMS"=BD_est1/Brms, "Y/BT"=predD/BT_est1)
write.table(VarPobl, '../tables/Var_Pobl.txt', append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)


like<- cbind(CPUE=out1$LIKE[1], Crucero=out1$LIKE[2], Desemb=out1$LIKE[3], prop=out1$LIKE[4], 
             prop_mflo=out1$LIKE[5], prop_hflo=out1$LIKE[6], pobs_crum=out1$LIKE[7], pobs_cruh=out1$LIKE[8],
             Ro=out1$LIKE[9], No_m=out1$LIKE[10], No_h=out1$LIKE[11], Lo_m=out1$LIKE[12], Lo_h=out1$LIKE[13], cvage_m=out1$LIKE[14], cvage_h=out1$LIKE[15])
like
write.table(like, '../tables/verosimilitud.txt', append = FALSE, sep = " ", dec = ".", row.names = TRUE, col.names = TRUE)



# Diagrama de Fases ####

estatus <- "Asesoría de agosto 2021"

  years1       <- yrs
  Bo1           <- out1$BDoLP              # Paso 4: Obtenci?n de Bo
  BRMS1         <- Brms                       # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
  FRMS1         <- Frms
BLIM1         <- Bo1*0.275                        # Paso 6: Obtenci?n de Blim = 20%Bo 
FLIM1         <- 1.27                             # Paso 6: Obtenci?n de Flim = 30%SPRo
  SpB1          <- BD_est1                          # BD serie histórica de evaluación de stock 
  SpBSE1        <- stdBD1                         # desviaci?n estandar BD
  ln_Fyr1       <- subset(std1,name=='log_Fh')$value      # logaritmo de Ft
  ln_FSE1       <- subset(std1,name=='log_Fh')$std                           # logaritmo de la desviaci?n standar de Ft

source('~/Documents/Rwork/Functions/Funciones/Fn_DiagramaFase.R')
DiagramaFase(estatus,years1,SpB1,SpBSE1,ln_Fyr1,ln_FSE1,FRMS1,BRMS1,BLIM1,FLIM1,color=F,dir.1,etiqueta=F)


# Bo2           <- rep2$BDoLP              # Paso 4: Obtenci?n de Bo
# BRMS2         <- Bo2*0.4                        # Paso 5: Obtenci?n de Brms = 60%SPRo = 55%Bo
# FRMS2         <- 0.26
# BLIM2         <- Bo2*0.275                        # Paso 6: Obtenci?n de Blim = 20%Bo 
# FLIM2         <- 1.27                             # Paso 6: Obtenci?n de Flim = 30%SPRo
# SpB2          <- SSBt2                            # BD serie hist?rica de evaluaci?n de stock 
# SpBSE2        <- SSBt2std                         # desviaci?n estandar BD
# ln_Fyr2       <- Ft2                              # logaritmo de Ft
# ln_FSE2       <- Ft2std                           # logaritmo de la desviaci?n standar de Ft

# Buscar fases anterior en proyecto anterior

