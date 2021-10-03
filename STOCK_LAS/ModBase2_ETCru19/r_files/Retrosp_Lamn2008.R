rm(list=ls())

# Funciones y Directorios ####

 library(stringr)
 library(dplyr)
 library(ggplot2)
 library(reshape)

 source('~/Documents/Rwork/Functions/Funciones/functions.R')
 source('~/Documents/Rwork/Functions/Funciones/read.report.R')

dir.1<-'~/Documents/Rwork/IFOP/LAm_2020/Estatus_2008/norte/'
dir.2<-'~/Documents/ADMwork/IFOP/2019/Lama_model/Cons_2003/norte/Lamnor2003/'
dir.3<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Lamnor2008cpue_zero/'
dir.4<-'~/Documents/ADMwork/IFOP/2020/Lama_model/Estatus_2008/norte/Retrospectivo'


dir.create(file.path('../','Retrospectivo'))#crea la carpeta  nuevamente
#setwd(dir.3)
file.copy( '../input/stock_LAS.dat', '../Retrospectivo')
file.copy('../runfolder/Lam.tpl' , '../Retrospectivo')
file.copy(c('../output/Lam.std', '../output/Lam.rep'), '../Retrospectivo')


# Lectura de datos ####

setwd('../Retrospectivo/')

dat.file    = '../input/stock_LAS.dat'
dat        <- lisread(paste('../Retrospectivo/',dat.file, sep='/'));
names(dat) <- str_trim(names(dat), side='right')
rep        <- reptoRlist('Lam.rep')                                        
std        <- read.table('Lam.std',header=T,sep='',na='NA',fill=T)  


# Arreglo de datos ####

years  <- dat$Ind[,1]
nyears <- length(years)


# BIOMASA DESOVANTE marzo 2020
SSBt    <- subset(std,name=='BD')$value
SSBtstd <- subset(std,name=='BD')$std

# RECLUTAMIENTOS
Reclutas     <- subset(std,name=='Restim')$value
Reclutas_std  <- subset(std,name=='Restim')$std

# MORTALIDAD POR PESCA
Ft     <- subset(std,name=='log_Fh')$value
Ft_std  <- subset(std,name=='log_Fh')$std

# Retro Tradicional ####

data.1  <- dat
retros  <- c(0:4)
n       <- length(retros)

for(i in 1:length(retros)){
	data.1$nyrs    <- dat$nyrs-retros[i]
	data.1$Ind     <- dat$Ind[1:(dat$nyrs-retros[i]),]
	data.1$ETflom  <- dat$ETflom[1:(dat$nyrs-retros[i]),]
	data.1$ETfloh  <- dat$ETfloh[1:(dat$nyrs-retros[i]),]
	data.1$ETcrum  <- dat$ETcrum[1:(dat$nyrs-retros[i]),]
	data.1$ETcruh  <- dat$ETcruh[1:(dat$nyrs-retros[i]),]
	writeData(paste('Lam','s',i,'.dat',sep=''), data.1, append=F)}


# Corre todos los dat

run<-rbind('./Lam -ind $1.dat -r',' cp Lam.rep $1.rep','cp Lam.std $1.std')
cat(run,file = (can <- file('run.sh','wb',encoding='UTF-8')),sep='\n')
close(can)

n <- 5
casos <- rep(NA,n)
s     <- seq(1,n,1)
for(i in 1:n){
  casos[i]<-paste('./run.sh Lams',s[i],sep='')
  }
cat(casos,file=(can<-file('casos.sh','wb',encoding='UTF-8')),sep='\n');
close(can)

system("chmod 755 run.sh")

system('admb Lam.tpl')
system("bash ./casos.sh") 



# GRAFICO RETROSPECTIVO ####


retroR <- retroBD  <- retroF  <- matrix(nrow=length(years),ncol=length(retros))

for(i in 1:length(retros)){
  rep            <-reptoRlist(paste('Lams', i, '.rep',sep=''))
  retroBD[,i]  <-c(rep$BD, rep(0,i-1))
  retroR[,i]   <-c(rep$Rech_pre_est[2,], rep(0,i-1))
  retroF[,i]   <-c(rep$Fm_Fh[2,], rep(0,i-1))
}
retroBD[retroBD == 0] <- NA
retroR[retroR == 0] <- NA
retroF[retroF == 0] <- NA


rbd <- as.data.frame(retroBD) %>% mutate(year=years) %>% melt(id.vars='year') %>%
  mutate(type='B desovante') %>% mutate (Periodo=c(rep('t',nyears),rep('t-1',nyears),rep('t-2',nyears),rep('t-3',nyears),rep('t-4',nyears))) 
rbR <- as.data.frame(retroR) %>% mutate(year=years) %>% melt(id.vars='year') %>%
  mutate(type='Reclutas') %>% mutate (Periodo=c(rep('t',nyears),rep('t-1',nyears),rep('t-2',nyears),rep('t-3',nyears),rep('t-4',nyears))) 
rbF <- as.data.frame(retroF) %>% mutate(year=years) %>% melt(id.vars='year') %>%
  mutate(type='Mortalidad por pesca') %>% mutate (Periodo=c(rep('t',nyears),rep('t-1',nyears),rep('t-2',nyears),rep('t-3',nyears),rep('t-4',nyears))) 

matretro <- rbind(rbd,rbR,rbF)
names(matretro)


figretro <- ggplot(matretro) + geom_line(aes(x = year, y = value, color = Periodo)) + 
  xlab('Años') + scale_x_continuous(breaks=round(seq(min(years), max(years), by = 5),1)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=8)) +
  facet_wrap(~type, dir='v',scale='free') + labs(x = 'año', y = '') + theme(legend.position = 'bottom') + theme(legend.title=element_blank())

figretro

#setwd(dir.1)
#dir.create('Figs_Retro')
ggsave(figretro, filename = "../figures/Fig_Retro1.pdf", width=7, height=5.5, dpi=300)






# ANÁLISIS DE PATRONES RETROSPECTIVOS ####


#png(paste(getwd(), 'Fig3_Retrospectivo2.png',sep ="/"),width=350,height=520)
#figretro
#dev.off()


#C?digo de Francisco y Fernando 
# Mohn rho
years      <- years2
nyears     <- length(years)
nyrs.retro <- length(retros)

mohn.ssb     <- rep(NA, nyrs.retro) 
rel.diff.ssb <- matrix(NA, nrow=nyears, ncol=(nyrs.retro ))
mohn.f       <- rep(NA, nyrs.retro)
rel.diff.f   <- matrix(NA, nrow=nyears, ncol=(nyrs.retro ))
mohn.r       <- rep(NA, nyrs.retro)
rel.diff.r   <- matrix(NA, nrow=nyears, ncol=(nyrs.retro ))

for (j in 1:nyrs.retro) { 
	rel.diff.ssb[,j] <- (retroBD[,(j)]-retroBD[,1])/retroBD[,1]
	mohn.ssb[j]      <- rel.diff.ssb[(nyears-j),j]
	rel.diff.f[,j]   <- (retroF[,(j)]-retroF[,1])/retroF[,1]
	mohn.f[j]        <- rel.diff.f[(nyears-j),j]
        rel.diff.r[,j]   <- (retroR[,(j)]-retroR[,1])/retroR[,1]
        mohn.r[j]        <- rel.diff.r[(nyears-j),j]}


#biomasa desovante
ave.mohn.ssb  <- mean(mohn.ssb)
a0            <- which(rel.diff.ssb==-1,arr.ind=TRUE)
junk          <- rel.diff.ssb
a1            <- which(rel.diff.ssb==-1,arr.ind=TRUE)
ff            <- dim(a1)
for(l in 1:ff[1]){
        rel.diff.ssb[a1[l,1], a1[l,2]]<-NA }

#mortalidad por pesca
ave.mohn.f    <- mean(mohn.f)
a0f           <- which(rel.diff.f==-1,arr.ind=TRUE)
junkf         <- rel.diff.f
a1f           <- which(rel.diff.f==-1,arr.ind=TRUE)
fff           <- dim(a1f)
for(l in 1:fff[1]){
        rel.diff.f[a1f[l,1],a1f[l,2]]<-NA}

#reclutas
ave.mohn.r    <- mean(mohn.r)
a0r           <- which(rel.diff.r==-1,arr.ind=TRUE)
junkr         <- rel.diff.r
a1r           <- which(rel.diff.r==-1,arr.ind=TRUE)
ffr           <- dim(a1r)
for(l in 1:ffr[1]){
        rel.diff.r[a1r[l,1],a1r[l,2]]<-NA}

# Gr?fica 
png(paste(getwd(), 'Fig4_Mohn.png',sep ="/"),width=350,height=520)
par(mfrow=c(3,1),mar=c(2,4,2,2)+0.1)
plot(years,rel.diff.ssb[,1], type = 'p', ylab = 'rel.diff.ssb', xlab = 'a?os' , main='Biomasa desovante', ylim = c(-1.2,1.2))
   for (i in 1:nyears){
        lines(years,rel.diff.ssb[,i],col=i,lwd=1)}
legend(2012,0.0, c('t','t-1','t-2','t-3','t-4'), lwd=1, col=seq(1,5,1), bty='n')
text(1989, 0.9,'Rho = 0,063', bty='n', cex=1.2)

plot(years,rel.diff.f[,1], type = 'p', ylab = 'rel.diff.F', xlab = 'a?os' , main='Mortalidad por pesca', ylim = c(-1.2,1.2))
for (i in 1:nyears){
        lines(years,rel.diff.f[,i],col=i,lwd=1)}
#legend(2012,0.05, c('t','t-1','t-2','t-3','t-4'), lwd=1, col=seq(1,5,1), bty='n')
text(1989,0.9, 'Rho = -0,032', bty='n', cex=1.2)

plot(years,rel.diff.r[,1], type = 'p', ylab = 'rel.diff.R', xlab = 'a?os' , main='Reclutas', ylim = c(-1.2,1.2))
for (i in 1:nyears){
        lines(years,rel.diff.r[,i],col=i,lwd=1)}
#legend(2012,0.05, c('t','t-1','t-2','t-3','t-4'), lwd=1, col=seq(1,5,1), bty='n')
text(1989, 0.9, 'Rho = 0,239', bty='n', cex=1.2)
dev.off()


