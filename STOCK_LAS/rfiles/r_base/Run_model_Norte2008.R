rm(list=ls())

# Funciones y Directorios ####  
 library(stringr)
# library(knitr)
# library(dplyr)
# library(ggplot2)
# library(reshape)
# library(ggthemes)
# library(R2admb)
# library(gridExtra)
# library(ggpubr)

 source('~/Documents/Rwork/Functions/Funciones/functions.R')
# source('~/Documents/Rwork/Functions/Funciones/read.report.R')
# source('~/Documents/Rwork/Functions/multiplot.R')
# source('~/Documents/Rwork/Functions/read.admb.R')

dir.1<-'~/Documents/Rwork/IFOP/LAm_2020/Estatus_2008/norte/'
dir.2<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/data_update/'
dir.3<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Lamnor2008/'


unlink(dir.3,recursive=T) #borra el dir3 del Mac
dir.create(file.path('~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/','Lamnor2008')) # crea el dir3 nuevo y vacío
#system('mkdir fig') otra opción
setwd(dir.2); file.copy(c('lamnor2008s5.dat', 'LAmN.tpl'), dir.3)


# Corre modelos ####

# Tamaños de muestra originales (50 50 25 25)
setwd(dir.3)

system('mv lamnor2008s5.dat lamnor2008.dat')

system('~/admb/admb LAmN.tpl')
system('./LAmN -ind lamnor2008.dat')


# NO SE ESTIMARON TAMAÑOS DE MUESTRAS PARA LA ZONA NORTE. SE PONDRA COMO UN ESCENARIO DEBIDO AL IMPORTANTE IMPACTO EN LA BIOMASA Y CBA




# ESTIMACIÓN TAMAÑO MUESTRAS ####

dat.file      = 'lamnor2008.dat'
data.0        <- lisread(paste(dir.3,dat.file, sep='/'));
names(data.0) <-  str_trim(names(data.0), side="right")
data.1        <- data.0
rep           <- reptoRlist('LAmN.rep')


# Lee datos
years1   <- data.1$Ind[,1]
nyears1  <- data.1$nyrs
tallas  <- seq(10,52,1)
ntallas <- data.1$ntallas

#Proporción observada                  
pobsFm  <-rep$pobs_mflo
pobsFh  <-rep$pobs_hflo
pobsRm  <-rep$pobs_mcru
pobsRh  <-rep$pobs_hcru
#Proporción predicha
ppredFm<-rep$ppred_mflo
ppredFh<-rep$Ppred_hflo
ppredRm<-rep$ppred_mcru
ppredRh<-rep$ppred_hcru

resflm <-matrix(ncol=ntallas,nrow=nyears1)
for(i in 1:nyears1){
  for(j in 1:ntallas){
    resflm[,j]<-pobsFm[,j]-ppredFm[,j]}}

#Proporciones                                                
pFm   <- c(pobsFm,ppredFm); pFm[pFm==0]  <-NA
pFh   <- c(pobsFh,ppredFh); pFh[pFh==0]  <-NA
pRm   <- c(pobsRm,ppredRm); pRm[pRm==0]  <-NA
pRh   <- c(pobsRh,ppredRh); pRh[pRh==0]  <-NA

#arreglos                                                    
talla <- rep(gl((length(tallas)),length(years1),label=tallas),4)
años <- rep(years1,length(tallas)*4)
ind  <- c(rep("capt_obs",length(years1)*length(tallas)),
          rep("capt_est",length(years1)*length(tallas)))
pro  <- data.frame(años,talla,ind,pFm,pFh,pRm,pRh)
# ==========================================================================

#=================================================================#
# Método de Ianelli 2002
#=================================================================#
#Flota machos
Ofl <-ppredFm[rowSums(pobsFm)>0,]*(1-ppredFm[rowSums(pobsFm)>0,])
Efl <-(pobsFm[rowSums(pobsFm)>0,]-ppredFm[rowSums(pobsFm)>0,])^2
wfl <-rep(0,length(Ofl[,1]))
for(i in 1:length(Ofl[,1])){
  wfl[i] <-sum(Ofl[i,])/sum(Efl[i,])}

nmfm_ari <-mean(wfl)                      # MEDIA ARITMETICA
nmfm_geo <-exp(sum(log(wfl))/length(wfl)) # MEDIA GEOMÉTRICA
nmfm_arm <-1/mean(1/wfl)                  # MEDIA ARMÓNICA

#Flota hembras
Oflh <-ppredFh[rowSums(pobsFh)>0,]*(1-ppredFh[rowSums(pobsFh)>0,])
Eflh <-(pobsFh[rowSums(pobsFh)>0,]-ppredFh[rowSums(pobsFh)>0,])^2
wflh <-rep(0,length(Oflh[,1]))
for(i in 1:length(Oflh[,1])){
  wflh[i] <-sum(Oflh[i,])/sum(Eflh[i,])}

nmf_ari <-mean(wflh)                      # MEDIA ARITMETICA
nmf_geo <-exp(sum(log(wflh))/length(wflh)) # MEDIA GEOMÉTRICA
nmf_arm <-1/mean(1/wflh)                  # MEDIA ARMÓNICA

#------------------------------------------------------------
#Crucero machos
Ore <-ppredRm[rowSums(pobsRm)>0,]*(1-ppredRm[rowSums(pobsRm)>0,])
Ere <-(pobsRm[rowSums(pobsRm)>0,]-ppredRm[rowSums(pobsRm)>0,])^2
wre <-rep(0,length(Ore[,1]))
for(i in 1:length(Ore[,1])){	
  wre[i] <-sum(Ore[i,])/sum(Ere[i,])}
nmrm_ari <-mean(wre)                      # MEDIA ARITMETICA
nmrm_geo <-exp(sum(log(wre))/length(wre)) # MEDIA GEOMÉTRICA
nmrm_arm <-1/mean(1/wre)                  # MEDIA ARMÓNICA

#Crucero hembras
Oreh <-ppredRh[rowSums(pobsRh)>0,]*(1-ppredRh[rowSums(pobsRh)>0,])
Ereh <-(pobsRh[rowSums(pobsRh)>0,]-ppredRh[rowSums(pobsRh)>0,])^2
wreh <-rep(0,length(Oreh[,1]))
for(i in 1:length(Oreh[,1])){	
  wreh[i] <-sum(Oreh[i,])/sum(Ereh[i,])}
nmr_ari <-mean(wreh)                      # MEDIA ARITMETICA
nmr_geo <-exp(sum(log(wreh))/length(wreh)) # MEDIA GEOMÉTRICA
nmr_arm <-1/mean(1/wreh)                  # MEDIA ARMÓNICA


#------------------------------------------------------------
NM_Ian <- data.frame(nmFm=c(nmfm_ari,nmfm_geo,nmfm_arm),nmFh=c(nmf_ari,nmf_geo,nmf_arm),
                     nmCm=c(nmrm_ari,nmrm_geo,nmrm_arm),nmCh=c(nmr_ari,nmr_geo,nmr_arm))

NM_Ian

# Reemplazo nm nuevo
#data.1$Ind[,10:13] <- c(rep(50,nyears1), rep(50,nyears1), rep(25,nyears1), rep(25,nyears1))
data.1$Ind[,10:13] <- c(rep(NM_Ian[3,1],nyears1), rep(NM_Ian[3,2],nyears1), rep(NM_Ian[3,3],nyears1), rep(NM_Ian[3,4],nyears1))

#system('cp -)
writeData(paste("lamnor1910s8.dat",sep=""), data.1, append=FALSE)
system('mv lamnor1910s8.dat lamnor1910s6.dat')



### Una vez estimados los tamaños de muestra...

# Corre el MODELO BASE ####
system('mv lamnor1910s6.dat lamnor1910.dat')
system('~/admb/admb LAM')
system('./LAM -ind lamnor1910.dat')

