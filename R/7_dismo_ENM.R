# Required packages
library(parallel)
library(foreach)
library(doParallel)
require(raster)
library(rgeos)
library(rgdal)
library(dismo)
library(rJava)
library(kernlab)
library(randomForest)
library(maptools)
library(plyr)
library(SDMTools)
#library(Rmpi)


indexOf <- function(v,findFor) {
  
  i=0
  for(i2 in v) {
    i = i + 1
    if (i2==findFor){
      return(i)
    }
  }
  return(0)
}


#Baixar o Maxent (Apenas rode a funcao abaixo se você não tiver baixado o Maxent)
#MaxEnt .jar
#jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
#if (file.exists(jar) != T) {
#  url = "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
#  download.file(url, dest = "maxent.zip", mode = "wb")
#  unzip("maxent.zip",
#        files = "maxent.jar",
#        exdir = system.file("java", package = "dismo"))
#  unlink("maxent.zip")
#  warning("Maxent foi colocado no diretorio")
#}


setwd("/home/users/luaratourinho/ENM_dismo/Caluromys_derbianus")


file = './spdata/Caluromys_derbianus_training.csv' # Name of .csv file with species occurrences under three columns: 'species', 'lon', 'lat'
#model <- pa ~ hr_breeding+ha_avg+r_sum05to09.crop+r_sum10to04.crop # incluir nomes das variaveis no modelo para GLM, RandomForest e SVM
model <- pa ~ tmax_avg+tmin_avg+r_sum05to09.crop+r_sum10to04.crop
k = 10 # numero de divisoes para validacao cruzada (4 = 75% treino e 25% teste; 5 = 80% treino e 20% teste; 10 = 90% treino e 10% teste)
bg.pt = 10000 # numero de pontos de background a serem criados
t.met = 'spec_sens' # metodo de selecao de threshold ('spec_sens' = max. spec + sens)
#lim = 51 # Minimum number of occurrences to perform pseudo-absence sampling
tss.lim = 0.6 # Minimum TSS value for ensemble

cont.maps = T # salvar mapas continuos por algoritmo?
bin.maps = T # salvar mapas binarios por algoritmo?
ens.maps = T # salvar apenas mapas ensemble - "continuos" e binarios

############ code

pres <- read.csv(file)
sp.names <- as.character(unique(pres$species))
sp.n = sp.names[1] # essa linha eh so para testar os passos do loop para uma especie


# predictors <- stack("./variables/current/hr_breeding.tif",
#                     "./variables/current/ha_avg.tif",
#                     "./variables/current/r_sum05to09.crop.tif",
#                     "./variables/current/r_sum10to04.crop.tif")
# 
# CANESM2 <- stack("./variables/future/hr_breeding.tif",
#                  "./variables/future/ha_avg.tif",
#                  "./variables/future/r_sum05to09.crop.tif",
#                  "./variables/future/r_sum10to04.crop.tif")


predictors <- stack("./variables/current/tmax_avg.tif",
                    "./variables/current/tmin_avg.tif",
                    "./variables/current/r_sum05to09.crop.tif",
                    "./variables/current/r_sum10to04.crop.tif")

CANESM2 <- stack("./variables/future/tmax_avg.tif",
                 "./variables/future/tmin_avg.tif",
                 "./variables/future/r_sum05to09.crop.tif",
                 "./variables/future/r_sum10to04.crop.tif")


# crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
# crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs") # projected, South America Albers Equal Area Conic
# 
# b = 200000
# 
# coordinates(pres) <- c("lon", "lat") # define quais colunas do data frame são lon e lat, transformando o objeto em "SpatialPoints"
# proj4string(pres) <- crs.wgs84  # define a projeção dos pontos de presença - wgs84
# pres.albers <- spTransform(pres, crs.albers)  # projeta os pontos em Albers Equal Area
# 
# # gerar pol??gono para amostragem de pseudo-ausências
# mpc <- gConvexHull(pres.albers) # desenha o minimo poligono convexo dos pontos de presença
# buf <- gBuffer(mpc, width = b) # desenha buffer de 200 km ao redor do minimo poligono convexo
# pol.wgs <- spTransform(buf, crs.wgs84) # converter de albers para wgs84
# 
# #  corta para pegar apenas os pixels que sao zero dentro do buffer
# predictors2 <- crop(predictors, extent(pol.wgs))
# pseudo.mask <- mask(predictors2, pol.wgs)
# #plot(pseudo.mask)
# predictors <- pseudo.mask
# 
# ####
# CANESM22 <- crop(CANESM2, extent(pol.wgs))
# pseudo.mask <- mask(CANESM22, pol.wgs)
# #plot(pseudo.mask)
# CANESM2 <- pseudo.mask
####


#cellStats(predictors[[1]], "min")


#sink('time.txt')
#print(Sys.time()-ini)
#sink()

#save.image('./myEnvironment.RData')
#for(sp.n in sp.names) {
ini = Sys.time()
pb <- txtProgressBar(min = 1, max = length(sp.names)+1, style = 3)

#foreach(sp.n = sp.names, .packages = c("dismo", "raster", "rgdal", "plyr", "randomForest", "kernlab")) %dopar%{
#setTxtProgressBar(pb, grep(pattern = sp.n, sp.names)) 

lim = count(pres$species)$freq[indexOf(count(pres$species)$x, sp.n)]; # Number of occurrences to perform pseudo-absence sampling

# prepare environment for species
started_time = Sys.time()
cat( format( started_time, "%a %b %d %X %Y"), '-', 'STARTED', '\n')
cat( format( started_time, "%a %b %d %X %Y"), '-', 'Preparing train and test datasets for', sp.n, 'with ', lim, 'lines...', '\n')

#target_dir = paste( './outputs_future', '/', sp.n, sep="" )
target_dir = paste( "./outputs", '/', sep="" )
dir.create( target_dir )

if(file.exists(paste(target_dir, '/STARTED.txt', sep="")))
  stop("You MUST DELETE output folder before continue")

#write(format( started_time, "%a %b %d %X %Y"), file=paste('./outputs_future/', sp.n, '/STARTED.txt', sep=""))
write(format( started_time, "%a %b %d %X %Y"), file=paste("./outputs", '/STARTED.txt', sep=""))


#sp.data <- read.csv(paste('./spdata/', sp.n, "_com_pseudo_aus", '.csv', sep=""), header=TRUE, sep=',')
sp.data <- read.csv(paste('./spdata/', "pres_pseudoabs_biomod_tmaxtmin", '.csv', sep=""), header=TRUE, sep=',')


#para bioclim e maxent:
pres <- sp.data[sp.data$pa==1,2:3]
abs <- sp.data[sp.data$pa!=1,2:3] 
bg <- randomPoints(predictors, bg.pt)
colnames(bg) <- c("lon", "lat")

set.seed(10) #caso queria que seja a mesma combinacao sempre
foldpres <- kfold(pres, 4)
set.seed(10) #caso queria que seja a mesma combinacao sempre
foldabs <- kfold(abs, 4)

prestrain <- list()
prestest <- list()
abstrain <- list()
abstest <- list()
for(i in 1:k){
  foldpres <- kfold(pres, 4) #aqui divide as presenças em 4 grupos, associando um número inteiro de 1 a 4 a cada uma delas
  foldabs <- kfold(abs, 4) #aqui divide as ausências em 4 grupos, associando um número inteiro de 1 a 4 a cada uma delas
  prestrain[[i]] <- pres[foldpres != 1,] #pegar todas as presenças cujos números associados pelo kfold sejam diferentes de 1, ou seja, 3/4 do conjunto
  prestest[[i]] <- pres[foldpres == 1,] #pegar todas as presenças cujos números associados pelo kfold sejam iguais a 1, ou seja, 1/4 do conjunto
  abstrain[[i]] <- abs[foldabs != 1,] #pegar todas as ausências cujos números associados pelo kfold sejam diferentes de 1, ou seja, 3/4 do conjunto
  abstest[[i]] <- abs[foldabs == 1,] #pegar todas as ausências cujos números associados pelo kfold sejam iguais a 1, ou seja, 1/4 do conjunto
} #no final do loop ele vai repetir a divisão aleatória em 4 grupos. As 10 réplicas terão alguns registros se repetindo aleatoriamente (bootstrap), mas assim você pode ter quantas réplicas quiser.

# para GLM, random Forest e SVM:  
train <- list()
pa_train <- list()
predtrain <- list()
testpres <- list()
testabs <- list()

for(i in 1:k){
  train[[i]] <- rbind(prestrain[[i]], abstrain[[i]])
  pa_train[[i]] <- c(rep(1, nrow(prestrain[[i]])), rep(0, nrow(abstrain[[i]])))
  predtrain[[i]] <- extract(predictors, train[[i]])
  predtrain[[i]] <- data.frame(cbind(pa=pa_train[[i]], predtrain[[i]]))
}

rm(train)
rm(pa_train)

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Bioclim model for', sp.n, '...', '\n')
bc <- list()
evbc <- list()
bcTSS <- list()
bcAUC <- list()
bckappa <- list()
bcthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Bioclim (', i, ') model for', sp.n, '...', '\n')
  bc[[i]] <- bioclim(predictors, prestrain[[i]])
  evbc[[i]] <- evaluate(prestest[[i]], abstest[[i]], bc[[i]], predictors)
  bcTSS[[i]] <- max(evbc[[i]]@TPR + evbc[[i]]@TNR)-1
  bcAUC[[i]] <- evbc[[i]]@auc
  bckappa[[i]] <- max(evbc[[i]]@kappa) 
  bcthres[[i]] <- threshold(evbc[[i]], t.met)
}

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running GLM (logistic regression) for', sp.n, '...', '\n')
gm <- list()
evgm <- list()
gmTSS <- list()
gmAUC <- list()
gmkappa <- list()
gmthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running GLM (logistic regression) (', i, ') for', sp.n, '...', '\n')
  gm[[i]] <- glm(model, family=binomial(link="logit"), data=predtrain[[i]]) # reescrever para que os nomes das vari?veis sejam genericos
  evgm[[i]] <- evaluate(prestest[[i]], abstest[[i]], gm[[i]], predictors) # ao inves de prestest e abstest(que vou ter que gerar a partir dos dados de presenca q eu nao vou usar). ai leio essas duas tabelas aqui.
  gmTSS[[i]] <- max(evgm[[i]]@TPR + evgm[[i]]@TNR)-1
  gmAUC[[i]] <- evgm[[i]]@auc
  gmkappa[[i]] <- max(evgm[[i]]@kappa)
  gmthres[[i]] <- threshold(evgm[[i]], t.met)
}

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Random Forest model for', sp.n, '...', '\n')
rf <- list()
evrf <- list()
rfTSS <- list()
rfAUC <- list()
rfkappa <- list()
rfthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Random Forest (', i, ') model for', sp.n, '...', '\n')
  rf[[i]] <- randomForest(model, data=predtrain[[i]], na.action=na.omit) # reescrever para que os nomes das vari?veis sejam genericos
  evrf[[i]] <- evaluate(prestest[[i]], abstest[[i]], rf[[i]], predictors)
  rfTSS[[i]] <- max(evrf[[i]]@TPR + evrf[[i]]@TNR)-1
  rfAUC[[i]] <- evrf[[i]]@auc
  rfkappa[[i]] <- max(evrf[[i]]@kappa)
  rfthres[[i]] <- threshold(evrf[[i]], t.met)
}

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Maxent model for', sp.n, '...', '\n')
mx <- list()
evmx <- list()
mxTSS <- list()
mxAUC <- list()
mxkappa <- list()
mxthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running Maxent (', i, ') model for', sp.n, '...', '\n')
  mx[[i]] <- maxent(predictors, prestrain[[i]], a=bg) # reescrever para que os nomes das vari?veis sejam genericos
  evmx[[i]] <- evaluate(prestest[[i]], abstest[[i]], mx[[i]], predictors)
  mxTSS[[i]] <- max(evmx[[i]]@TPR + evmx[[i]]@TNR)-1
  mxAUC[[i]] <- evmx[[i]]@auc
  mxkappa[[i]] <- max(evmx[[i]]@kappa)
  mxthres[[i]] <- threshold(evmx[[i]], t.met)
}

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running SVM model for', sp.n, '...', '\n')
sv <- list()
evsv <- list()
svTSS <- list()
svAUC <- list()
svkappa <- list()
svthres <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Running SVM (', i, ') model for', sp.n, '...', '\n')
  sv[[i]] <- ksvm(model, data=predtrain[[i]]) # reescrever para que os nomes das vari?veis sejam genericos
  evsv[[i]] <- evaluate(prestest[[i]], abstest[[i]], sv[[i]], predictors)
  svTSS[[i]] <- max(evsv[[i]]@TPR + evsv[[i]]@TNR)-1
  svAUC[[i]] <- evsv[[i]]@auc
  svkappa[[i]] <- max(evsv[[i]]@kappa)
  svthres[[i]] <- threshold(evsv[[i]], t.met)
}


cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Generating Validation table for models of', sp.n, '...', '\n')
bcTSSval <- unlist(bcTSS)
gmTSSval <- unlist(gmTSS)
rfTSSval <- unlist(rfTSS)
mxTSSval <- unlist(mxTSS)
svTSSval <- unlist(svTSS)
bcAUCval <- unlist(bcAUC)
gmAUCval <- unlist(gmAUC)
rfAUCval <- unlist(rfAUC)
mxAUCval <- unlist(mxAUC)
svAUCval <- unlist(svAUC)
bckappaval <- unlist(bckappa)
gmkappaval <- unlist(gmkappa)
rfkappaval <- unlist(rfkappa)
mxkappaval <- unlist(mxkappa)
svkappaval <- unlist(svkappa)
mod.names <- c(rep ('bc', k), rep('gm', k), rep('rf', k), rep('mx', k), rep('sv', k))
mod.sp <- c(rep(sp.n, k*5)) #conferir a planilha
TSS <- c(bcTSSval, gmTSSval, rfTSSval, mxTSSval, svTSSval)
AUC <- c(bcAUCval, gmAUCval, rfAUCval, mxAUCval, svAUCval)
kappa <- c(bckappaval, gmkappaval, rfkappaval, mxkappaval, svkappaval)
Valid <- data.frame(mod.sp, mod.names, TSS, AUC, kappa, stringsAsFactors=FALSE)
write.csv(Valid, file = paste( target_dir, '/Valid_', sp.n, '.csv', sep=""))


# Drop bad models, project under current and future conditions
# cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Bioclim model of', sp.n, '...', '\n')
# cur.bc <- list()
# cur.bc.bin <- list()
# CANESM2.bc <- list()
# CANESM2.bc.bin <- list()
# 
# 
# for(i in 1:k){
#   cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Bioclim (', i, ') model of', sp.n, '...', '\n')
#   if(bcTSS[[i]] >= tss.lim){
#     cur.bc[[i]] <- predict(predictors, bc[[i]])
#     cur.bc.bin[[i]] <- cur.bc[[i]] > bcthres[[i]]
#     CANESM2.bc[[i]] <- predict(CANESM2, bc[[i]])
#     CANESM2.bc.bin[[i]] <- CANESM2.bc[[i]] > bcthres[[i]]
#     
#   } else {
#     cur.bc[[i]] <- NULL
#     cur.bc.bin[[i]] <- NULL
#     CANESM2.bc[[i]] <- NULL
#     CANESM2.bc.bin[[i]] <- NULL
#   }
# }
# 
# 
# ##########
# cur.bc <- Filter(Negate(is.null), cur.bc) # remove os rasters nulos
# cur.bc.bin <- Filter(Negate(is.null), cur.bc.bin) # remove os rasters nulos
# 
# #Ensemble of binary models (Regra da maioria)
# cur.bc.ens <- Reduce('+', cur.bc.bin) #soma tds os pxels
# tval <- unique(cur.bc.ens) #para saber quais o valores que tem
# tval <- tval[tval != 0] #tira o zero
# tval <- median(tval) #faz a mediana sem o zero
# cur.bc.ens.bin <- cur.bc.ens >= tval #regra da maioria usando a mediana sem os zeros
# 
# 
# ##########
# CANESM2.bc <- Filter(Negate(is.null), CANESM2.bc) # remove os rasters nulos
# CANESM2.bc.bin <- Filter(Negate(is.null), CANESM2.bc.bin) # remove os rasters nulos
# 
# CANESM2.bc.ens <- Reduce('+', CANESM2.bc.bin)
# tval <- unique(CANESM2.bc.ens)
# tval <- tval[tval != 0]
# tval <- median(tval)
# CANESM2.bc.ens.bin <- CANESM2.bc.ens >= tval
# 
# ##########
# 
# # Taking out the zero values and normalizing the rasters
# 
# for(z in 1:length(cur.bc)){
#   adeq = cur.bc[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   cur.bc[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# 
# for(z in 1:length(CANESM2.bc)){
#   adeq = CANESM2.bc[[z]]
#   minimo <- min(adeq[], na.rm=T)
#   maximo <- max(adeq[], na.rm=T)
#   adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
#   CANESM2.bc[[z]] <- calc(adeq, adeq_norm)
# }
# 
# 
# #Ensemble of continuos models
# 
# x <- stack(cur.bc)
# cur.bc.cont <- calc(x, fun = mean)
# 
# x <- stack(CANESM2.bc)
# CANESM2.bc.cont <- calc(x, fun = mean)


#for(i in 1:k){
# min_max <- range(gcmmg.bc[[i]][], na.rm=T)
#  write.table(min_max, paste("gcmmg.bc", i, sp.n, ".txt",  sep="_"))
#}

#rm(cur.bc.bin, cur.bc.cont, cur.bc.ens)
#rm(CANESM2.bc.bin, CANESM2.bc.cont, CANESM2.bc.ens)


cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting GLMs of', sp.n, '...', '\n')
cur.gm <- list()
cur.gm.bin <- list()
CANESM2.gm <- list()
CANESM2.gm.bin <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting GLMs (', i, ') of', sp.n, '...', '\n')
  if(gmTSS[[i]] >= tss.lim){
    cur.gm[[i]] <- predict(predictors, gm[[i]])
    cur.gm.bin[[i]] <- cur.gm[[i]] > gmthres[[i]]
    CANESM2.gm[[i]] <- predict(CANESM2, gm[[i]])
    CANESM2.gm.bin[[i]] <- CANESM2.gm[[i]] > gmthres[[i]]
    
  } else {
    cur.gm[[i]] <- NULL
    cur.gm.bin[[i]] <- NULL
    CANESM2.gm[[i]] <- NULL
    CANESM2.gm.bin[[i]] <- NULL
    
  }
}

########## 
cur.gm <- Filter(Negate(is.null), cur.gm) # remove os rasters nulos
cur.gm.bin <- Filter(Negate(is.null), cur.gm.bin) # remove os rasters nulos

cur.gm.ens <- Reduce('+', cur.gm.bin)
tval <- unique(cur.gm.ens)
tval <- tval[tval != 0]
tval <- median(tval)
cur.gm.ens.bin <- cur.gm.ens >= tval

########## 
CANESM2.gm <- Filter(Negate(is.null), CANESM2.gm) # remove os rasters nulos
CANESM2.gm.bin <- Filter(Negate(is.null), CANESM2.gm.bin) # remove os rasters nulos

CANESM2.gm.ens <- Reduce('+', CANESM2.gm.bin)
tval <- unique(CANESM2.gm.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CANESM2.gm.ens.bin <- CANESM2.gm.ens >= tval


########## 

for(z in 1:length(cur.gm)){
  adeq = cur.gm[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  cur.gm[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(CANESM2.gm)){
  adeq = CANESM2.gm[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  CANESM2.gm[[z]] <- calc(adeq, adeq_norm)
}


x <- stack(cur.gm)
cur.gm.cont <- calc(x, fun = mean)

x <- stack(CANESM2.gm)
CANESM2.gm.cont <- calc(x, fun = mean)


#rm(cur.gm.bin, cur.gm.cont, cur.gm.ens)
#rm(CANESM2.gm.bin, CANESM2.gm.cont, CANESM2.gm.ens)

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Random Forest models of', sp.n, '...', '\n')
cur.rf <- list()
cur.rf.bin <- list()
CANESM2.rf <- list()
CANESM2.rf.bin <- list()


for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Random Forest (', i, ') models of', sp.n, '...', '\n')
  if(rfTSS[[i]] >= tss.lim){
    cur.rf[[i]] <- predict(predictors, rf[[i]])
    cur.rf.bin[[i]] <- cur.rf[[i]] > rfthres[[i]]
    
    CANESM2.rf[[i]] <- predict(CANESM2, rf[[i]])
    CANESM2.rf.bin[[i]] <- CANESM2.rf[[i]] > rfthres[[i]]
  } else {
    
    cur.rf[[i]] <- NULL
    cur.rf.bin[[i]] <- NULL
    CANESM2.rf[[i]] <- NULL
    CANESM2.rf.bin[[i]] <- NULL
    
  }
}

########## 
cur.rf <- Filter(Negate(is.null), cur.rf) # remove os rasters nulos
cur.rf.bin <- Filter(Negate(is.null), cur.rf.bin) # remove os rasters nulos

cur.rf.ens <- Reduce('+', cur.rf.bin)
tval <- unique(cur.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
cur.rf.ens.bin <- cur.rf.ens >= tval


########## 
CANESM2.rf <- Filter(Negate(is.null), CANESM2.rf) # remove os rasters nulos
CANESM2.rf.bin <- Filter(Negate(is.null), CANESM2.rf.bin) # remove os rasters nulos

CANESM2.rf.ens <- Reduce('+', CANESM2.rf.bin)
tval <- unique(CANESM2.rf.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CANESM2.rf.ens.bin <- CANESM2.rf.ens >= tval


########## 

for(z in 1:length(cur.rf)){
  adeq = cur.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    cur.rf[[z]] <- calc(adeq, adeq_norm)
  }
  
} #add essa corre??o para todos


for(z in 1:length(CANESM2.rf)){
  adeq = CANESM2.rf[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    CANESM2.rf[[z]] <- calc(adeq, adeq_norm)
  }
}

cur.rf <- Filter(Negate(is.null), cur.rf) # remove os rasters nulos #aplicar essa corre??o para todos
CANESM2.rf <- Filter(Negate(is.null), CANESM2.rf)

x <- stack(cur.rf)
cur.rf.cont <- calc(x, fun = mean)

x <- stack(CANESM2.rf)
CANESM2.rf.cont <- calc(x, fun = mean)

#rm(cur.rf.bin, cur.rf.cont, cur.rf.ens)
#rm(CANESM2.rf.bin, CANESM2.rf.cont, CANESM2.rf.ens)

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Maxent models of', sp.n, '...', '\n')
cur.mx <- list()
cur.mx.bin <- list()
CANESM2.mx <- list()
CANESM2.mx.bin <- list()

for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting Maxent (', i, ') models of', sp.n, '...', '\n')
  if(mxTSS[[i]] >= tss.lim){
    cur.mx[[i]] <- predict(predictors, mx[[i]])
    cur.mx.bin[[i]] <- cur.mx[[i]] > mxthres[[i]]
    
    CANESM2.mx[[i]] <- predict(CANESM2, mx[[i]])
    CANESM2.mx.bin[[i]] <- CANESM2.mx[[i]] > mxthres[[i]]
    
  } else {
    cur.mx[[i]] <- NULL
    cur.mx.bin[[i]] <- NULL
    CANESM2.mx[[i]] <- NULL
    CANESM2.mx.bin[[i]] <- NULL
    
  }
}


########## 
cur.mx <- Filter(Negate(is.null), cur.mx) # remove os rasters nulos
cur.mx.bin <- Filter(Negate(is.null), cur.mx.bin) # remove os rasters nulos

cur.mx.ens <- Reduce('+', cur.mx.bin)
tval <- unique(cur.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
cur.mx.ens.bin <- cur.mx.ens >= tval


########## 
CANESM2.mx <- Filter(Negate(is.null), CANESM2.mx) # remove os rasters nulos
CANESM2.mx.bin <- Filter(Negate(is.null), CANESM2.mx.bin) # remove os rasters nulos

CANESM2.mx.ens <- Reduce('+', CANESM2.mx.bin)
tval <- unique(CANESM2.mx.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CANESM2.mx.ens.bin <- CANESM2.mx.ens >= tval


##########


for(z in 1:length(cur.mx)){
  adeq = cur.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    cur.mx[[z]] <- calc(adeq, adeq_norm)
  }
}

for(z in 1:length(CANESM2.mx)){
  adeq = CANESM2.mx[[z]]
  if(sum(adeq[], na.rm=T)!=0){
    minimo <- min(adeq[], na.rm=T)
    maximo <- max(adeq[], na.rm=T)
    adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
    CANESM2.mx[[z]] <- calc(adeq, adeq_norm)
  }
}

x <- stack(cur.mx)
cur.mx.cont <- calc(x, fun = mean)

x <- stack(CANESM2.mx)
CANESM2.mx.cont <- calc(x, fun = mean)

#rm(cur.mx.bin, cur.mx.cont, cur.mx.ens)
#rm(CANESM2.mx.bin, CANESM2.mx.cont, CANESM2.mx.ens)


cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting SVM models of', sp.n, '...', '\n')
cur.sv <- list()
cur.sv.bin <- list()
CANESM2.sv <- list()
CANESM2.sv.bin <- list()


for(i in 1:k){
  cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting SVM (', i, ') models of', sp.n, '...', '\n')
  if(svTSS[[i]] >= tss.lim){
    cur.sv[[i]] <- predict(predictors, sv[[i]])
    cur.sv.bin[[i]] <- cur.sv[[i]] > svthres[[i]]
    
    CANESM2.sv[[i]] <- predict(CANESM2, sv[[i]])
    CANESM2.sv.bin[[i]] <- CANESM2.sv[[i]] > svthres[[i]]
    
  } else {
    cur.sv[[i]] <- NULL
    cur.sv.bin[[i]] <- NULL
    CANESM2.sv[[i]] <- NULL
    CANESM2.sv.bin[[i]] <- NULL
    
  }
}


########## 
cur.sv <- Filter(Negate(is.null), cur.sv) # remove os rasters nulos
cur.sv.bin <- Filter(Negate(is.null), cur.sv.bin) # remove os rasters nulos

cur.sv.ens <- Reduce('+', cur.sv.bin)
tval <- unique(cur.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
cur.sv.ens.bin <- cur.sv.ens >= tval


########## 
CANESM2.sv <- Filter(Negate(is.null), CANESM2.sv) # remove os rasters nulos
CANESM2.sv.bin <- Filter(Negate(is.null), CANESM2.sv.bin) # remove os rasters nulos

CANESM2.sv.ens <- Reduce('+', CANESM2.sv.bin)
tval <- unique(CANESM2.sv.ens)
tval <- tval[tval != 0]
tval <- median(tval)
CANESM2.sv.ens.bin <- CANESM2.sv.ens >= tval


########## 

for(z in 1:length(cur.sv)){
  adeq = cur.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  cur.sv[[z]] <- calc(adeq, adeq_norm)
}


for(z in 1:length(CANESM2.sv)){
  adeq = CANESM2.sv[[z]]
  minimo <- min(adeq[], na.rm=T)
  maximo <- max(adeq[], na.rm=T)
  adeq_norm <- function(x) {(x-minimo)/(maximo-minimo)}
  CANESM2.sv[[z]] <- calc(adeq, adeq_norm)
}


x <- stack(cur.sv)
cur.sv.cont <- calc(x, fun = mean)

x <- stack(CANESM2.sv)
CANESM2.sv.cont <- calc(x, fun = mean)

#rm(cur.sv.bin, cur.sv.cont, cur.sv.ens)
#rm(CANESM2.sv.bin, CANESM2.sv.cont, CANESM2.sv.ens)

#rm(x)

# ensemble current
# binarios
cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Projecting ensemble models of', sp.n, '...', '\n')
#algo.cur <- list(cur.bc.ens.bin, cur.gm.ens.bin, cur.rf.ens.bin, cur.mx.ens.bin, cur.sv.ens.bin)
algo.cur <- list(cur.gm.ens.bin, cur.rf.ens.bin, cur.mx.ens.bin, cur.sv.ens.bin)
algo.cur <- algo.cur[is.na(algo.cur) == FALSE]
ens.cur <- Reduce('+', algo.cur)
tval <- unique(ens.cur)
tval <- tval[tval != 0]
tval <- median(tval)
ens.cur.bin <- ens.cur >= tval

#rm(algo.cur)
#rm(ens.cur)

# continuo - media ponderada pelo TSS
#w <- c(bcTSSval, gmTSSval, rfTSSval, mxTSSval, svTSSval)
w <- c(gmTSSval, rfTSSval, mxTSSval, svTSSval)

# st1 <- stack(cur.bc)
# for(i in 1:length(cur.bc)){
#   min_max <- range(cur.bc[[i]][], na.rm=T)
#   write.table(min_max, paste("cur.bc", i, sp.n, ".txt",  sep="_"))
# }
#rm(cur.bc)

st2 <- stack(cur.gm)
for(i in 1:length(cur.gm)){
  min_max <- range(cur.gm[[i]][], na.rm=T)
  #write.table(min_max, paste("cur.gm", i, sp.n, ".txt",  sep="_"))
}
#rm(cur.gm)

st3 <- stack(cur.rf)
for(i in 1:length(cur.rf)){
  min_max <- range(cur.rf[[i]][], na.rm=T)
  #write.table(min_max, paste("cur.rf", i, sp.n, ".txt",  sep="_"))
}
#rm(cur.rf)

st4 <- stack(cur.mx)
for(i in 1:length(cur.mx)){
  min_max <- range(cur.mx[[i]][], na.rm=T)
  #write.table(min_max, paste("cur.mx", i, sp.n, ".txt",  sep="_"))
}
#rm(cur.mx)

st5 <- stack(cur.sv)
for(i in 1:length(cur.sv)){
  min_max <- range(cur.sv[[i]][], na.rm=T)
  #write.table(min_max, paste("cur.sv", i, sp.n, ".txt",  sep="_"))
}
#rm(cur.sv)

#st <- stack(st1, st2, st3, st4, st5)
st <- stack(st2, st3, st4, st5)
w.sem.os.nulos <- w[w >= tss.lim]
ens2.cur <- weighted.mean(st, w.sem.os.nulos)
ens2.cur.sd.w <- sum(w.sem.os.nulos * (st - ens2.cur)^2)
ens2.cur.sd.w <- sqrt(ens2.cur.sd.w)



# Uncertainty -------------------------------------------------------------

#library(SDMTools)

# wbc <- c(bcTSSval)
# wbc.sem.os.nulos <- wbc[wbc >= tss.lim]
# cur.bc.mean.w <- weighted.mean(st2, wbc.sem.os.nulos)
# #cur.bc.sd.w <- wt.sd(st2, wbc.sem.os.nulos)
# cur.bc.sd.w <- sum(wbc.sem.os.nulos * (st2 - cur.bc.mean.w)^2)
# cur.bc.sd.w <- sqrt(cur.bc.sd.w)

wgm <- c(gmTSSval)
wgm.sem.os.nulos <- wgm[wgm >= tss.lim]
cur.gm.mean.w <- weighted.mean(st2, wgm.sem.os.nulos)
#cur.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
cur.gm.sd.w <- sum(wgm.sem.os.nulos * (st2 - cur.gm.mean.w)^2)
cur.gm.sd.w <- sqrt(cur.gm.sd.w)

wrf <- c(rfTSSval)
wrf.sem.os.nulos <- wrf[wrf >= tss.lim]
cur.rf.mean.w <- weighted.mean(st3, wrf.sem.os.nulos)
#cur.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
cur.rf.sd.w <- sum(wrf.sem.os.nulos * (st3 - cur.rf.mean.w)^2)
cur.rf.sd.w <- sqrt(cur.rf.sd.w)

wmx <- c(mxTSSval)
wmx.sem.os.nulos <- wmx[wmx >= tss.lim]
cur.mx.mean.w <- weighted.mean(st4, wmx.sem.os.nulos)
#cur.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
cur.mx.sd.w <- sum(wmx.sem.os.nulos * (st4 - cur.mx.mean.w)^2)
cur.mx.sd.w <- sqrt(cur.mx.sd.w)

wsv <- c(svTSSval)
wsv.sem.os.nulos <- wsv[wsv >= tss.lim]
cur.sv.mean.w <- weighted.mean(st5, wsv.sem.os.nulos)
#cur.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
cur.sv.sd.w <- sum(wsv.sem.os.nulos * (st5 - cur.sv.mean.w)^2)
cur.sv.sd.w <- sqrt(cur.sv.sd.w)


##### 
# ensemble CANESM2
# binarios
#algo.CANESM2 <- list(CANESM2.bc.ens.bin, CANESM2.gm.ens.bin, CANESM2.rf.ens.bin, CANESM2.mx.ens.bin, CANESM2.sv.ens.bin)
algo.CANESM2 <- list(CANESM2.gm.ens.bin, CANESM2.rf.ens.bin, CANESM2.mx.ens.bin, CANESM2.sv.ens.bin)
algo.CANESM2 <- algo.CANESM2[is.na(algo.CANESM2) == FALSE]
ens.CANESM2 <- Reduce('+', algo.CANESM2)
tval <- unique(ens.CANESM2)
tval <- tval[tval != 0]
tval <- median(tval)
ens.CANESM2.bin <- ens.CANESM2 >= tval

#rm(algo.CANESM2)
#rm(ens.CANESM2)

# continuos - media TSS

# st1 <- stack(CANESM2.bc)
# for(i in 1:length(CANESM2.bc)){
#   min_max <- range(CANESM2.bc[[i]][], na.rm=T)
#   write.table(min_max, paste("CANESM2.bc", i, sp.n, ".txt",  sep="_"))
# }
#rm(CANESM2.bc)

st2 <- stack(CANESM2.gm)
for(i in 1:length(CANESM2.gm)){
  min_max <- range(CANESM2.gm[[i]][], na.rm=T)
  #write.table(min_max, paste("CANESM2.gm", i, sp.n, ".txt",  sep="_"))
}
#rm(CANESM2.gm)

st3 <- stack(CANESM2.rf)
for(i in 1:length(CANESM2.rf)){
  min_max <- range(CANESM2.rf[[i]][], na.rm=T)
  #write.table(min_max, paste("CANESM2.rf", i, sp.n, ".txt",  sep="_"))
}
#rm(CANESM2.rf)

st4 <- stack(CANESM2.mx)
for(i in 1:length(CANESM2.mx)){
  min_max <- range(CANESM2.mx[[i]][], na.rm=T)
  #write.table(min_max, paste("CANESM2.mx", i, sp.n, ".txt",  sep="_"))
}
#rm(CANESM2.mx)

st5 <- stack(CANESM2.sv)
for(i in 1:length(CANESM2.sv)){
  min_max <- range(CANESM2.sv[[i]][], na.rm=T)
  #write.table(min_max, paste("CANESM2.sv", i, sp.n, ".txt",  sep="_"))
}
#rm(CANESM2.sv)

#st <- stack(st1, st2, st3, st4, st5)
st <- stack(st2, st3, st4, st5)
w.sem.os.nulos <- w[w >= tss.lim]
ens2.CANESM2 <- weighted.mean(st, w.sem.os.nulos)
ens.fut.cont <- ens2.CANESM2
ens2.fut.sd.w <- sum(w.sem.os.nulos * (st - ens.fut.cont)^2)
ens2.fut.sd.w <- sqrt(ens2.fut.sd.w)


# Uncertainty future ------------------------------------------------------

# wbc <- c(bcTSSval)
# wbc.sem.os.nulos <- wbc[wbc >= tss.lim]
# CANESM2.bc.mean.w <- weighted.mean(st2, wbc.sem.os.nulos)
# #CANESM2.bc.sd.w <- wt.sd(st2, wbc.sem.os.nulos)
# CANESM2.bc.sd.w <- sum(wbc.sem.os.nulos * (st2 - CANESM2.bc.mean.w)^2)
# CANESM2.bc.sd.w <- sqrt(CANESM2.bc.sd.w)

wgm <- c(gmTSSval)
wgm.sem.os.nulos <- wgm[wgm >= tss.lim]
CANESM2.gm.mean.w <- weighted.mean(st2, wgm.sem.os.nulos)
#CANESM2.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
CANESM2.gm.sd.w <- sum(wgm.sem.os.nulos * (st2 - CANESM2.gm.mean.w)^2)
CANESM2.gm.sd.w <- sqrt(CANESM2.gm.sd.w)

wrf <- c(rfTSSval)
wrf.sem.os.nulos <- wrf[wrf >= tss.lim]
CANESM2.rf.mean.w <- weighted.mean(st3, wrf.sem.os.nulos)
#CANESM2.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
CANESM2.rf.sd.w <- sum(wrf.sem.os.nulos * (st3 - CANESM2.rf.mean.w)^2)
CANESM2.rf.sd.w <- sqrt(CANESM2.rf.sd.w)

wmx <- c(mxTSSval)
wmx.sem.os.nulos <- wmx[wmx >= tss.lim]
CANESM2.mx.mean.w <- weighted.mean(st4, wmx.sem.os.nulos)
#CANESM2.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
CANESM2.mx.sd.w <- sum(wmx.sem.os.nulos * (st4 - CANESM2.mx.mean.w)^2)
CANESM2.mx.sd.w <- sqrt(CANESM2.mx.sd.w)

wsv <- c(svTSSval)
wsv.sem.os.nulos <- wsv[wsv >= tss.lim]
CANESM2.sv.mean.w <- weighted.mean(st5, wsv.sem.os.nulos)
#CANESM2.gm.sd.w <- wt.sd(st2, wgm.sem.os.nulos)
CANESM2.sv.sd.w <- sum(wsv.sem.os.nulos * (st5 - CANESM2.sv.mean.w)^2)
CANESM2.sv.sd.w <- sqrt(CANESM2.sv.sd.w)

####

ens.fut <- ens.CANESM2.bin 
tval <- unique(ens.fut)
tval <- tval[tval != 0]
tval <- median(tval)
ens.fut.bin <- ens.fut >= tval


#if(ens.maps == T){
cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Saving ensemble maps of', sp.n, '...', '\n')
writeRaster(ens2.cur, file = paste(target_dir, '/CUR.cont_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(ens2.CANESM2, file = paste(target_dir, '/MPI8570.cont_', sp.n, '.asc', sep=""),overwrite=TRUE)

writeRaster(ens.cur.bin, file = paste(target_dir, '/CUR.bin_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(ens.CANESM2.bin, file = paste(target_dir, '/MPI8570.bin_', sp.n, '.asc', sep=""),overwrite=TRUE)

#weighted.mean and weighted.sd
writeRaster(ens2.cur.sd.w, file = paste(target_dir, '/ens.cur.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(ens2.fut.sd.w, file = paste(target_dir, '/ens.mpi8570.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(cur.bc.mean.w, file = paste(target_dir, '/cur.bc.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(cur.bc.sd.w, file = paste(target_dir, '/cur.bc.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(cur.gm.mean.w, file = paste(target_dir, '/cur.gm.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(cur.gm.sd.w, file = paste(target_dir, '/cur.gm.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(cur.rf.mean.w, file = paste(target_dir, '/cur.rf.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(cur.rf.sd.w, file = paste(target_dir, '/cur.rf.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(cur.mx.mean.w, file = paste(target_dir, '/cur.mx.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(cur.mx.sd.w, file = paste(target_dir, '/cur.mx.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(cur.sv.mean.w, file = paste(target_dir, '/cur.sv.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(cur.sv.sd.w, file = paste(target_dir, '/cur.sv.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(CANESM2.bc.mean.w, file = paste(target_dir, '/mpi8570.bc.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(CANESM2.bc.sd.w, file = paste(target_dir, '/mpi8570.bc.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(CANESM2.gm.mean.w, file = paste(target_dir, '/mpi8570.gm.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(CANESM2.gm.sd.w, file = paste(target_dir, '/mpi8570.gm.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(CANESM2.rf.mean.w, file = paste(target_dir, '/mpi8570.rf.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(CANESM2.rf.sd.w, file = paste(target_dir, '/mpi8570.rf.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(CANESM2.mx.mean.w, file = paste(target_dir, '/mpi8570.mx.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE) 
writeRaster(CANESM2.mx.sd.w, file = paste(target_dir, '/mpi8570.mx.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
#writeRaster(CANESM2.sv.mean.w, file = paste(target_dir, '/mpi8570.sv.mean.w_', sp.n, '.asc', sep=""),overwrite=TRUE)
writeRaster(CANESM2.sv.sd.w, file = paste(target_dir, '/mpi8570.sv.sd.w_', sp.n, '.asc', sep=""),overwrite=TRUE)

############ projecao para sad69 ################

# proj4string(ens2.cur) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# crs.albers <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")
# cur.cont.proj <- projectRaster(ens2.cur, crs=crs.albers)
# writeRaster(cur.cont.proj, filename=paste(target_dir, '/cur.cont.proj_', sp.n, '.tif', sep=""), format="GTiff")
# 
# proj4string(ens2.CANESM2) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# crs.albers <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")
# ens2cont.CANESM2.proj <- projectRaster(ens2.CANESM2, crs=crs.albers)
# writeRaster(ens2cont.CANESM2.proj, filename=paste(target_dir, '/ens2cont.CANESM2.proj_', sp.n, '.tif', sep=""), format="GTiff")
# 
# proj4string(ens.fut.bin) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# crs.albers <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")
# ens.fut.bin.proj <- projectRaster(ens.fut.bin, crs=crs.albers)
# writeRaster(ens.fut.bin.proj, filename=paste(target_dir, '/ens.fut.bin.proj_', sp.n, '.tif', sep=""), format="GTiff")
# 
# proj4string(ens.fut.cont) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# crs.albers <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")
# ens.fut.cont.proj <- projectRaster(ens.fut.cont, crs=crs.albers)
# writeRaster(ens.fut.cont.proj, filename=paste(target_dir, '/ens.fut.cont.proj_', sp.n, '.tif', sep=""), format="GTiff")

#} 

cat( format( Sys.time(), "%a %b %d %X %Y"), '-', 'Finished train and test datasets for', sp.n, 'with ', lim, 'lines...', '\n')

save.image("./outputs/Caluromys_derbianus.rData")

finished_time = Sys.time()
cat( format( finished_time, "%a %b %d %X %Y"), '-', 'FINISHED', '\n')
write(format( finished_time, "%a %b %d %X %Y"), file=paste('./outputs/', '/FINISHED.txt', sep="")) 

# Reset and prepare next species
#rm(list = ls(all=TRUE))