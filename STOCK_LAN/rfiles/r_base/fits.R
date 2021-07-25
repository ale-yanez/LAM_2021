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

out1 <- read.admb("../../output/Lam")
#out1 <- read.admb("~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Lamnor2008cpue_zero/LAM_nor2008")

names(out1)
# Para graficar ... ####
 yrs <- out1$YRS
 nyrs <- length(yrs)
 tallas <- seq(10,52,1)

 # #Observado
 obsD <- out1$Desemb[1,]
 obsC <- out1$CPUE[1,] ; obsC[obsC <= 0.01]   <-NA
 obsS <- out1$BCRU[1,] ; obsS[obsS <= 1]  <-NA

# #predichos y estimados 
 predD         <- out1$Desemb[2,]
 predC         <- out1$CPUE[2,]
 predS         <- out1$BCRU[2,]

 Lobs_mf       <- out1$Lm_obs_pred[1,] ; Lobs_mf[Lobs_mf <=1]  <-NA
 Lpred_mf      <- out1$Lm_obs_pred[2,] ; Lpred_mf[Lpred_mf <=1]  <-NA
 Lobs_hf       <- out1$Lh_obs_pred[1,] ; Lobs_hf[Lobs_hf <=1]  <-NA
 Lpred_hf      <- out1$Lh_obs_pred[2,] ; Lpred_hf[Lpred_hf <=1]  <-NA
 Lobs_mc       <- out1$Lmc_obs_est[1,] ; Lobs_mc[Lobs_mc <=1]  <-NA
 Lpred_mc      <- out1$Lmc_obs_est[2,] ; Lpred_mc[Lpred_mc <=1]  <-NA
 Lobs_hc       <- out1$Lhc_obs_est[1,] ; Lobs_hc[Lobs_hc <=1]  <-NA
 Lpred_hc      <- out1$Lhc_obs_est[2,] ; Lpred_hc[Lpred_hc <=1]  <-NA

# Confidence Intervals
 cvdes         <- rep(0.1,nyrs)
 cvcpue        <- rep(0.15,nyrs)
 cvsurv        <- rep(0.30,nyrs)

 obsD95i   <- obsD*exp(-1.96*cvdes); obsD95s <- obsD*exp(1.96*cvdes)
 obsC95i   <- obsC*exp(-1.96*cvcpue); obsC95s <-obsC*exp(1.96*cvcpue)
 obsS95i   <- obsS*exp(-1.96*cvsurv); obsS95s <-obsS*exp(1.96*cvsurv)



#FITS LAMA MODEL ####

#  Desembarques, CPUE y Crucero ####

p1 <-  ggplot(NULL, aes(x=yrs)) +
  geom_point(aes(y= obsD, colour="Desemb Obs"), size = 2, shape = 21) +
  geom_line(aes(y= predD, colour="Desemb Est")) +
  geom_errorbar(aes(ymin = obsD95i, ymax = obsD95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Desemb Est'='royalblue3', 'Desemb Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('Desembarque (t)') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p1


p1_2 <-  ggplot(NULL, aes(x=yrs)) +
  geom_point(aes(y= obsD, colour="Observado"), size = 2, shape = 21) +
  geom_line(aes(y= predD, colour="Estimado")) +
  geom_errorbar(aes(ymin = obsD95i, ymax = obsD95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Estimado'='royalblue3', 'Observado'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('Desembarque (t)') + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p1_2


# Indice de Abundncia ###

p2 <-  ggplot(NULL, aes(x=yrs)) +
  geom_point(aes(y= obsC, colour="CPUE Obs"), size = 2, shape = 21) +
  geom_line(aes(y= predC, colour="CPUE Est")) +
  geom_errorbar(aes(ymin = obsC95i, ymax = obsC95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('CPUE Est'='royalblue3', 'CPUE Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('Indice Relativo') + scale_y_continuous(breaks=round(seq(min(predC), 3, by = 0.5),1)) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p2


#  Crucero ###

p3 <-  ggplot(NULL, aes(x=yrs)) +
  geom_point(aes(y= obsS, colour="Crucero Obs"), size = 2, shape = 21) +
  geom_line(aes(y= predS, colour="Crucero Est")) +
  geom_errorbar(aes(ymin = obsS95i, ymax = obsS95s), colour='black', width=0.6) +
  scale_colour_manual(name='', values=c('Crucero Est'='royalblue3', 'Crucero Obs'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('Biomasa (t)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank())

p3


p_1 <- ggarrange(p1, p2, p3,
                 ncol = 1, nrow = 3, align = "v")
plot <- ggarrange(p1_2, p2, p3,
                  ncol = 1, nrow = 3, align = "v", common.legend = TRUE, legend = "bottom")


ggexport(p_1, filename = "Fig1.jpeg", width=6.5, height=8, dpi=300)
ggexport(plot, filename = "Fig1_2.jpeg", width=6.5, height=8, dpi=300)
ggsave(plot, filename='Fig1.png', width=6.5, height=8, dpi=300)


# Composición de tallas Flota ####

# Machos Flota

df_mflobs <- data.frame(out1$pobs_mflo)
names <- c(tallas)
colnames(df_mflobs) <- names
df_mflobs$yr <- as.factor(yrs)
df_mflobs <- df_mflobs[-c(3:8, 29), ]

d_mflobs <- melt(df_mflobs)
colnames(d_mflobs) <- c('yr', 'Tallas', 'value')

#Adding fits
df_mfloest <- data.frame(out1$ppred_mflo)
names <- c(tallas)
colnames(df_mfloest) <- names
df_mfloest$yr <- as.factor(yrs)
df_mfloest <- df_mfloest[-c(3:8, 29), ]

dd_mfloest <- melt(df_mfloest)
colnames(dd_mfloest) <- c('yr2', 'Tallas2', 'value2')

#Gran data frame
d_mflo <- data.frame(d_mflobs$yr, d_mflobs$Tallas, d_mflobs$value, dd_mfloest$value2)
head(d_mflo)
colnames(d_mflo) <- c('yrs', 'Tallas', 'pobs', 'ppred')

#Plotting
p1 <- ggplot(data=d_mflo, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_mflo, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))

p1 <- p1 + facet_wrap(~ yrs, dir = 'v', scales='free')  + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.16))
p1

ggexport(p1, filename = "Fig2_TallasM_flo.jpeg")
ggsave(p1, filename = "Fig2_TallasM_flo.png")


# Hembras Flota

df_hflobs <- data.frame(out1$pobs_hflo)
names <- c(tallas)
colnames(df_hflobs) <- names
df_hflobs$yr <- as.factor(yrs)
df_hflobs <- df_hflobs[-c(3:8, 29), ]

d_hflobs <- melt(df_hflobs)
colnames(d_hflobs) <- c('yr', 'Tallas', 'value')

#Adding fits
df_hfloest <- data.frame(out1$Ppred_hflo)
names <- c(tallas)
colnames(df_hfloest) <- names
df_hfloest$yr <- as.factor(yrs)
df_hfloest <- df_hfloest[-c(3:8, 29), ]

dd_hfloest <- melt(df_hfloest)
colnames(dd_hfloest) <- c('yr2', 'Tallas2', 'value2')

d_hflo <- data.frame(d_hflobs$yr, d_hflobs$Tallas, d_hflobs$value, dd_hfloest$value2)
colnames(d_hflo) <- c('yrs', 'Tallas', 'pobs', 'ppred')


#Plotting
p2 <- ggplot(data=d_hflo, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_hflo, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))


p2 <- p2 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.22))
p2

ggexport(p2, filename = "Fig3_TallasH_flo.jpeg")
ggsave(p2, filename = "Fig3_TallasH_flo.png")


# Composición de tallas Crucero ####

# Machos Crucero

df_mcruobs <- data.frame(out1$pobs_mcru)
names <- c(tallas)
colnames(df_mcruobs) <- names
df_mcruobs$yr <- as.factor(yrs)
df_mcruobs <- df_mcruobs[-c(1:14, 16, 26), ]

d_mcruobs <- melt(df_mcruobs)
colnames(d_mcruobs) <- c('yr', 'Tallas', 'value')

#Adding fits
df_mcruest <- data.frame(out1$ppred_mcru)
names <- c(tallas)
colnames(df_mcruest) <- names
df_mcruest$yr <- as.factor(yrs)
df_mcruest <- df_mcruest[-c(1:14, 16, 26), ]

dd_mcruest <- melt(df_mcruest)
colnames(dd_mcruest) <- c('yr2', 'Tallas2', 'value2')

d_mcru <- data.frame(d_mcruobs$yr, d_mcruobs$Tallas, d_mcruobs$value, dd_mcruest$value2)
colnames(d_mcru) <- c('yrs', 'Tallas', 'pobs', 'ppred')

#Plotting
p3 <- ggplot(data=d_mcru, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_mcru, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))

p3 <- p3 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.13))
p3

ggexport(p3, filename = "Fig4_TallasM_cru.jpeg")
ggsave(p3, filename = "Fig4_TallasM_cru.png")


# Hembras Crucero

df_hcruobs <- data.frame(out1$pobs_hcru)
names <- c(tallas)
colnames(df_hcruobs) <- names
df_hcruobs$yr <- as.factor(yrs)
df_hcruobs <- df_hcruobs[-c(1:14, 16, 26), ]

d_hcruobs <- melt(df_hcruobs)
colnames(d_hcruobs) <- c('yr', 'Tallas', 'value')

#Adding fits
df_hcruest <- data.frame(out1$ppred_hcru)
names <- c(tallas)
colnames(df_hcruest) <- names
df_hcruest$yr <- as.factor(yrs)
df_hcruest <- df_hcruest[-c(1:14, 16, 26), ]

dd_hcruest <- melt(df_hcruest)
colnames(dd_hcruest) <- c('yr2', 'Tallas2', 'value2')

d_hcru <- data.frame(d_hcruobs$yr, d_hcruobs$Tallas, d_hcruobs$value, dd_hcruest$value2)
colnames(d_hcru) <- c('yrs', 'Tallas', 'pobs', 'ppred')

#Plotting
p4 <- ggplot(data=d_hcru, aes(x=Tallas, y=pobs)) +
  geom_bar(stat="identity", colour='grey') + 
  geom_line(data=d_hcru, aes(x=as.numeric(Tallas), y=ppred, colour = 'red')) + 
  #scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) +
  xlab('Tallas') + ylab('Proporción') + theme_bw() + theme(legend.position ='none') + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.text=element_text(size=8))

p4 <- p4 + facet_wrap(~ yrs, dir = 'v', scales = 'free') + scale_x_discrete('Tallas', breaks = seq(10, 52, by= 6)) + scale_y_continuous(limits=c(0,0.22))
p4

ggexport(p4, filename = "Fig5_TallasH_cru.jpeg")
ggsave(p4, filename = "Fig5_TallasH_cru.png")


# Tallas Medias Flota ####

#Machos
df_flo<-data.frame(cbind(yrs, Lobs_mf, Lpred_mf, Lobs_hf, Lpred_hf))
colnames(df_flo) <- c('yrs', 'Lobs_mf', 'Lest_m', 'Lobs_hf', 'Lest_h')

p5 <-  ggplot(df_flo, aes(x=yrs)) +
  geom_point(aes(y= Lobs_mf, colour="Talla Media Obs."), size = 2, shape = 21) +
  geom_line(aes(y= Lest_m, colour="Talla Media Est.")) +
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +

  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('LC machos (mm)') + ylim(24, 40) + 
  #scale_y_continuous(breaks=round(seq(24, 40, by = 4),1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))

p5

# Hembras
p6 <-  ggplot(df_flo, aes(x=yrs)) +
  geom_point(aes(y= Lobs_hf, colour="Talla Media Obs."), size = 2, shape = 21) +
  geom_line(aes(y= Lest_h, colour="Talla Media Est.")) +
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) + 
  ylab('LC hembras (mm)') + ylim(24, 40) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))

p6

plot1 <- ggarrange(p5, p6, ncol = 2, nrow = 1, align = "h", common.legend = TRUE, legend = "bottom")
plot2 <- ggarrange(p5, p6, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")

ggexport(plot1, filename = "Fig6_1.jpeg", width=9, height=6, dpi=300)
ggexport(plot2, filename = "Fig6_2.jpeg", width=6.5, height=8, dpi=300)

ggsave(plot1, filename = "Fig6_1.png", width=9, height=6, dpi=300)
ggsave(plot2, filename = "Fig6_2.png", width=6.5, height=8, dpi=300)


# Tallas Medias Crucero ####

#Machos crucero

df_cru<-data.frame(cbind(yrs, Lobs_mc, Lpred_mc, Lobs_hc, Lpred_hc))
colnames(df_cru) <- c('yrs', 'Lobs_mc', 'Lest_m', 'Lobs_hc', 'Lest_h')

p7 <-  ggplot(df_cru, aes(x=yrs)) +
  geom_point(aes(y= Lobs_mc, colour="Talla Media Obs."), size = 2, shape = 21)+
  geom_line(aes(y= Lest_m, colour="Talla Media Est."))+
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('LC machos (mm)') + ylim(24, 40) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))

p7

#Hembras crucero

p8 <-  ggplot(df_cru, aes(x=yrs)) +
  geom_point(aes(y= Lobs_hc, colour="Talla Media Obs."), size = 2, shape = 21) +
  geom_line(aes(y= Lest_h, colour="Talla Media Est.")) +
  scale_colour_manual(name='', values=c('Talla Media Est.'='royalblue3', 'Talla Media Obs.'='black'), guide='legend') +
  guides(colour = guide_legend(override.aes = list(linetype=c(1,0), shape=c(NA, 21)))) +
  
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(yrs), 2020, by = 5),1)) +
  ylab('LC hembras (mm)') + ylim(24,40) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.position = 'bottom')  + theme(legend.title=element_blank()) + theme(legend.text = element_text(size = 10))

p8

plot3 <- ggarrange(p7, p8, ncol = 2, nrow = 1, align = "v", common.legend = TRUE, legend = "bottom")
plot4 <- ggarrange(p7, p8, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")

ggexport(plot3, filename = "Fig7_1.jpeg", width=9, height=6, dpi=300)
ggexport(plot4, filename = "Fig7_2.jpeg", width=6.5, height=8, dpi=300)

ggsave(plot3, filename = "Fig7_1.png", width=9, height=6, dpi=300)
ggsave(plot4, filename = "Fig7_2.png", width=6.5, height=8, dpi=300)


# Selectividad ####

df_Sel <- data.frame(c(1:11), out1$Sflom_age[1,], out1$Sfloh_age[1,], out1$Scrum_age[1,], out1$Scruh_age[1,])
colnames(df_Sel) <- c('ages', 'S_flom','S_floh', 'S_crum', 'S_cruh')

#Plotting Flota

p9 <- ggplot(df_Sel, aes(x = ages)) + 
  geom_line(aes(y = S_flom, colour = 'Machos', linetype = 'Machos')) +
  geom_line(aes(y = S_floh, colour = 'Hembras', linetype = 'Hembras')) +
  scale_color_manual(name = '', values = c('black', 'black')) +     
  scale_linetype_manual(name = '', values = c('solid', 'dotted'))

p9 <- p9 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=10)) +
  theme(legend.title=element_blank()) + theme(legend.position = 'bottom') + theme(legend.text=element_text(size=12)) +
  ylab('Proporción') + scale_x_continuous('Edad (años)', breaks = seq(1, 11, by= 1)) + ggtitle('Flota')

p9

p10 <- ggplot(df_Sel, aes(x = ages)) + 
  geom_line(aes(y = S_crum, colour = 'Machos', linetype = 'Machos')) +
  geom_line(aes(y = S_cruh, colour = 'Hembras', linetype = 'Hembras')) +
  scale_color_manual(name = '', values = c('black', 'black')) + 
  scale_linetype_manual(name = '', values = c('solid', 'dotted'))

p10 <- p10 + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  theme(legend.title=element_blank()) + theme(legend.position = 'bottom') + theme(legend.text=element_text(size=12)) +
  ylab('Proporción') + scale_x_continuous('Edad (años)', breaks = seq(1, 11, by= 1)) + ggtitle('Crucero')

p10

psel <- ggarrange(p9, p10, ncol = 2, nrow = 1, align = "v", common.legend = TRUE, legend = "bottom")
ggexport(psel, filename = "Fig8.jpeg")
ggsave(psel, filename = "Fig8.png")

psel2 <- ggarrange(p9, p10, ncol = 1, nrow = 2, align = "v", common.legend = TRUE, legend = "bottom")
ggexport(psel2, filename = "Fig8_2.jpeg", width=7, height=8, dpi=300)
ggsave(psel2, filename = "Fig8_2.png", width=7, height=8, dpi=300)
