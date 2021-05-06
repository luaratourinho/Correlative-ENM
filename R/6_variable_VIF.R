
# Variance inflation factor (VIF)  ----------------------------------------


# Script was created by
# Daphne Spier (https://github.com/#########)

# Script was edited by
# Luara Tourinho

# Date: 0? abr 2021



if(!require(pacman)) install.packages("pacman")
pacman::p_load(gdm, raster, maptools, rgdal, psych, plyr, devtools)

# Packages
#para trabalhar com dados espaciais
library(sp)

#para trabalhar com raster
library(raster)

# to crop shp
library(rgdal)

#preparando o raster com os dados abioticos; juntando as variaveis num raster só com várias camadas
#tif <- list.files("./data/raster/wc5_amsul" , patt = ".tif")
#tif

clim <- list.files(path="./data/raster/wc5_amsul/", ".*.tif$",
                   full.names = TRUE)

#setwd("./Data/Abiotic/wc2.1_2.5m_bio")#tem q estar dentro da pasta abiotic

clim.stack <- stack(clim)
names(clim.stack)

plot(clim.stack[[20]])
#setwd("../../..") #volta diretorio
#setwd("./GDM") #volta diretorio
#getwd()


# extract values of cells
clim_v <- values(clim.stack)
clim_v <- na.omit(clim_v) #omit NAs

########################## correlacao - pearson é default ##############################
# it writes tables to check the correlation between variables
corr <- cor(clim_v)
abs(round(corr, 2)) # funçao abs, transforma em módulo (tira o negativo)
ifelse(corr >= 0.7, "sim", "nao")
ifelse(corr >= 0.7, 1, 0)
write.table(abs(round(corr, 2)), "./results/cor_pres_2.5.xls", row.names = T, sep = "\t")
write.table(ifelse(corr >= 0.7, "sim", "nao"), "./results/Selected_variables/cor_pres_afirmacao_2.5.xls",            row.names = T, sep = "\t")


#plot correlation matrix
library(corrplot) # To do correlation
##The function colorRampPalette() is very convenient for generating color spectrum.
corrplot(corr, 
         type = "upper", 
         diag = FALSE,)

corr_var(clim_v, # name of dataset
         amsul_bio01, # name of variable to focus on
         top = 5 # display top 5 correlations
) 

###################### VIF: Variance of inflation factor ##########################
#outro jeito de determinar quais variaveis estao correlacionadas ou nao.


source("./R/vif_conc.R")
vif<-vif_func(clim_v, thresh=10, trace=T) #usa a funçao vif_func (que está em outro script)
vif
#vif é uma lista das variaveis que nao estao correlacionadas; ou menos correlacionadas.

# significado das bios
# BIO01 = Temperatura media anual
# BIO02 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO03 = Isotermalidade (BIO02/BIO07) (* 100)
# BIO04 = Sazonalidade da temperatura (desvio padrao deviation *100)
# BIO05 = Temperatura maxima do mes mais quente
# BIO06 = Temperatura minima do mes mais frio
# BIO07 = Variacao da temperatura anual (BIO5-BIO6)
# BIO08 = Temperatura media do trimestre mais chuvoso
# BIO09 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio



#para selecionar as camadas correspondentes dessas variaveis do raster que contem todas as camadas - todas as variaveis e salvar um novo raster q contem so essas camadas que interessam.
names(clim.stack)
lista <- c(2, 3, 8, 9, 13, 14, 15, 18, 19, 20)
getwd()
#Dirsave <- "./data/raster/selection_vif"
#dir.create("data/raster/selection_vif")
setwd("./data/raster/selection_vif") # de novo tem q estar na pasta raster

for(i in lista){
  writeRaster(clim.stack[[i]], 
              ifelse(i < 10, paste0("wc_bio0", i, ".tif"),
                     paste0("wc_bio", i, ".tif")),
              format = "GTiff", overwrite=TRUE)}

#setwd("./data/raster/selection_vif") # de novo tem q estar na pasta raster
tif <- list.files(patt = ".tif")
wc <- grep("wc_bio", tif, value = T)
wc <-stack(wc)
plot(wc[[2]])
writeRaster(wc,"wc_vif_2.5.grd", format = "raster", overwrite=TRUE)

setwd("../../..") #volta diretorio
getwd()