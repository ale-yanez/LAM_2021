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
source('~/Documents/Rwork/Functions/Funciones/functions.R')


# dir.1<-'~/Documents/Rwork/IFOP/LAm_2020/Estatus_2008/norte/'
# dir.2<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/data_update/'
# dir.3<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Lamnor2008/'


# dir.create(file.path('~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/','Lamnor2008')) # crea el dir3 nuevo y vacío
system('mkdir ../../estim_nm') #otra opción
file.copy('../../input/stock_LAN.dat', '../../estim_nm')
file.copy('../../runfolder/Lam.tpl', '../../estim_nm')


# Corre modelos ####
# Tamaños de muestra originales (50 50 25 25)

system('mv ../../estim_nm/stock_LAN.dat ../../estim_nm/stock_LANnm.dat')

# ESTIMACIÓN TAMAÑO MUESTRAS ####
#system('cd ../','./run_nm.sh')
#system('admb ../../estim_nm/Lam.tpl')
#system('./LAmS -ind lamsur2008.dat') # Hessiana no parece estar definida positiva en la primera corrida, luego al estimar los nm converge



#dat1_nor        <- lisread("../../input/stock_LAN.dat");
dat1_nor        <- lisread("../../estim_nm/stock_LANnm.dat");
#dat1_nor        <- lisread("../../input/lamsur2008.dat");
names(dat1_nor) <- str_trim(names(dat1_nor), side="right")
dat_nor         <- dat1_nor
rep           <- reptoRlist('../../estim_nm/Lam.rep')
#rep           <- reptoRlist('../../output/LAmS.rep')


# Lee datos
years1   <- dat_nor$Ind[,1]
nyears1  <- dat_nor$nyrs
tallas  <- seq(10,52,1)
ntallas <- dat_nor$ntallas

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
#dat_nor$Ind[,10:13] <- c(rep(50,nyears1), rep(50,nyears1), rep(25,nyears1), rep(25,nyears1))
#dat_nor$Ind[,10:13] <- c(rep(89,nyears1), rep(54,nyears1), rep(168,nyears1), rep(93,nyears1))
dat_nor$Ind[,10:13] <- c(rep(NM_Ian[3,1],nyears1), rep(NM_Ian[3,2],nyears1), rep(NM_Ian[3,3],nyears1), rep(NM_Ian[3,4],nyears1))

#system('cp -)
writeData(paste("../../estim_nm/stock_LAN2020.dat",sep=""), dat_nor, append=FALSE)
system('mv ../../estim_nm/stock_LAN2020.dat ../../estim_nm/stock_LAN.dat')

### Una vez estimados los tamaños de muestra...####

file.copy('../../input/stock_LAN.dat', '../../estim_nm')

nm_nor        <- lisread("../../estim_nm/stock_LAN.dat");
names(nm_nor) <- str_trim(names(dat1_nor), side="right")
final_nor         <- nm_nor
final_nor$Ind[,10:13] <- dat_nor$Ind[,10:13]

writeData(paste("../../estim_nm/stock_LAN2020.dat",sep=""), final_nor, append=FALSE)
system('mv ../../estim_nm/stock_LAN2020.dat ../../estim_nm/stock_LAN.dat')


# # Corre el MODELO BASE ####
# system('mv lamnor1910s6.dat lamnor1910.dat')
# system('~/admb/admb LAM')
# system('./LAM -ind lamnor1910.dat')
# 
