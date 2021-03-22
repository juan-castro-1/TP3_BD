rm(list=ls())
library(rstudioapi)
dir <- getActiveDocumentContext()$path 
setwd(dirname(dir))
dir()

## 1
####################
## CARGAR LA EPH ##
#loc.url <- "https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_1_Trim_2020_xls.zip"
#td <- tempdir() 
#tf <- tempfile(tmpdir=td, fileext=".zip") 
#download.file(loc.url, tf) 
#fname <- unzip(tf, list=TRUE)$Name[1] 
#unzip(tf, files=fname, exdir=td, overwrite=TRUE) 
#fpath <- file.path(td, fname)
library("readxl")
dataset <- read_excel('usu_individual_T120.xlsx')
####################

## 2
dataset['ninios']<- 0
dataset$ninios <- ifelse(dataset$CH06 <11, 1,0 )

agg <- aggregate(dataset$ninios,
                 by= list(dataset$CODUSU),
                 FUN = sum
)

names(agg)[names(agg) == "Group.1"] <- "CODUSU"

dataset <- merge(dataset,agg, by ="CODUSU")
dataset <- subset(dataset, select= -c(ninios))
names(dataset)[names(dataset) == "x"] <- "ninios"

# 3
dataset <- subset(dataset, dataset$CH06>=16)
dataset <- subset(dataset, dataset$H15<2)

# 4
dataset['trabaja']<- 0
dataset$trabaja <- ifelse(dataset$ESTADO == 1, 1,0 )

# 5
dataset_mujeres <- subset(dataset, dataset$CH04 == 2)
dataset_varones <- subset(dataset, dataset$CH04 == 1)

# 6
#tabla_missin <- sapply(sis, function(x) sum(is.na(x)))
#tabla_missin <-data.frame(tabla_missin)
#View(tabla_missin)

#borro cols con mas de 20.000 NAs
dataset <- dataset[,sapply(dataset, function(x) sum(!is.na(x))>20000)]
tabla_missin_1 <- sapply(dataset, function(x) sum(is.na(x)))
tabla_missin_1 <-data.frame(tabla_missin_1)
#View(tabla_missin_1) 


sis <- dataset
#sis <- subset(dataset, select = c(MAS_500, CH05,CODUSU))
lm.imp.1 <- lm (IPCF ~ REGION + AGLOMERADO + CH03 + CH04 + CH06 + CH07 +CH08 + CH09+CH10+CH11+CH12+CH13+CH15+CH16+NIVEL_ED+ninios+trabaja,
                data=sis , subset=IPCF>0)

pred.1 <- predict (lm.imp.1, sis) 

#dataset$IPCF <- ifelse(dataset$IPCF==0, NA, dataset$IPCF)

#impute <- function (a, a.impute){ 
#  ifelse (is.na(a), a.impute, a)
#}

impute <- function (a, a.impute){ 
  ifelse (a==0, a.impute, a)
}

table(dataset$IPCF==0)
#FALSE  TRUE ---- antes de la reg
#32207  7277 

IPCF <- impute (dataset$IPCF, pred.1)
dataset$IPCF <- ifelse(dataset$IPCF==0, IPCF, dataset$IPCF)






#library("ggplot2")
#library("naniar")
#
#ggplot( bind_shadow(dataset),
#        aes(x = IPCF,
#            fill = PP07K_NA)) + 
#  geom_density(alpha=0.5) + xlim(0,30000)
#ggplot( bind_shadow(dataset),
#        aes(x = CH06,
#            fill = PP07K_NA)) + 
#  geom_density(alpha=0.5)
#ggplot( bind_shadow(dataset),
#        aes(x = ESTADO,
#            fill = PP07K_NA)) + 
#  geom_density(alpha=0.5)

#NO RESPONDEN LOS QUE NO ESTAN OCUPADOS EN TODAS ESTAS VARIABLES

#ggplot( bind_shadow(dataset),
#        aes(x = ITF,
#            fill = IDECINDR_NA)) + 
#  geom_density(alpha=0.5) + xlim(0,30000)


dataset <- subset(dataset, select = -c(CH05) )

library(zoo)
dataset <- replace(dataset, TRUE, lapply(dataset, na.aggregate))

tabla_missin_1 <- sapply(dataset, function(x) sum(is.na(x)))
tabla_missin_1 <-data.frame(tabla_missin_1)
View(tabla_missin_1) 




# 7
library(ggplot2)
dataset$CH04 <- as.factor(dataset$CH04)
library(jpeg)
options(scipen=999)

# IPCF
jpeg("Output/plot_7a_T.jpg")
ggplot(dataset[(dataset$trabaja==1),], aes(x=IPCF, group=CH04, fill=CH04)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Hombre", "Mujer")) +
  xlim(0,100000) 
dev.off()

jpeg("Output/plot_7a_NT.jpg")
ggplot(dataset[(dataset$trabaja==0),], aes(x=IPCF, group=CH04, fill=CH04)) +
  geom_density(alpha=0.5) + scale_fill_discrete(name = "Sexo", labels = c("Hombre", "Mujer")) +
  xlim(0,100000)
dev.off()


# EDAD
jpeg("Output/plot_7b_T.jpg")
ggplot(dataset[(dataset$trabaja==1),], aes(x=CH06, fill=CH04 )) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Hombre", "Mujer"))+xlab("Edad")
scale_color_manual(values=c("#999999", "#E69F00")) 
dev.off()

jpeg("Output/plot_7b_NT.jpg")
ggplot(dataset[(dataset$trabaja==0),], aes(x=CH06, fill=CH04)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Hombre", "Mujer")) +xlab("Edad")
dev.off()

table(dataset[(dataset$trabaja==0),]$CH06)
table(dataset[(dataset$trabaja==1),]$CH06)



# NINIOS
table(dataset[(dataset$trabaja==0),]$ninios)
table(dataset[(dataset$trabaja==1),]$ninios)

table(dataset_mujeres[(dataset_mujeres$trabaja==0),]$ninios)
table(dataset_mujeres[(dataset_mujeres$trabaja==1),]$ninios)

table(dataset_varones[(dataset_varones$trabaja==0),]$ninios)
table(dataset_varones[(dataset_varones$trabaja==1),]$ninios)


# AMA DE CASA
table(dataset_mujeres$trabaja)
table(dataset_mujeres[(dataset_mujeres$trabaja==0 ),]$CAT_INAC==4)

table(dataset_varones$trabaja)
table(dataset_varones[(dataset_varones$trabaja==0 ),]$CAT_INAC==4)


# JUBILADO
table(dataset_mujeres$trabaja)
table(dataset_mujeres[(dataset_mujeres$trabaja==0 ),]$CAT_INAC==1)

table(dataset_varones$trabaja)
table(dataset_varones[(dataset_varones$trabaja==0 ),]$CAT_INAC==1)

# INACTIVIDAD
dataset_mujeres$CAT_INAC[dataset_mujeres$CAT_INAC  == 6] <- 5
dataset_mujeres$CAT_INAC[dataset_mujeres$CAT_INAC  == 7] <- 6

names(dataset_mujeres)[names(dataset_mujeres) == "CAT_INAC"] <- "Inactividad"

jpeg("Output/plot_Inactividad_M.jpg")
ggplot(dataset_mujeres[(dataset_mujeres$trabaja==0 ),], aes(x= Inactividad)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop..  ), stat= "count", vjust = -.2) +
  labs(y = "Porcentaje", fill="Categoria") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Categoria", labels = c("Indeterminado",
                                                     "Jubilado",
                                                     "Rentista",
                                                     "Estudiante",
                                                     "Ama de Casa",
                                                     "Discapacitado",
                                                     "Otro")) +
  scale_x_discrete(limits = factor()) +
  theme_bw()
dev.off()

names(dataset_mujeres)[names(dataset_mujeres) == "Inactividad"] <- "CAT_INAC"

dataset_varones$CAT_INAC[dataset_varones$CAT_INAC  == 6] <- 5
dataset_varones$CAT_INAC[dataset_varones$CAT_INAC  == 7] <- 6

names(dataset_varones)[names(dataset_varones) == "CAT_INAC"] <- "Inactividad"

jpeg("Output/plot_Inactividad_H.jpg")
ggplot(dataset_varones[(dataset_varones$trabaja==0 ),], aes(x= Inactividad)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop..  ), stat= "count", vjust = -.2) +
  labs(y = "Porcentaje", fill="Categoria") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Categoria", labels = c("Indeterminado",
                                                     "Jubilado",
                                                     "Rentista",
                                                     "Estudiante",
                                                     "Ama de Casa",
                                                     "Discapacitado",
                                                     "Otro")) +
  scale_x_discrete(limits = factor()) +
  theme_bw()
dev.off()

names(dataset_varones)[names(dataset_varones) == "Inactividad"] <- "CAT_INAC"



#######################
#######################
#---> PARTE II <-----#
#######################
#######################
library(rpart)
library(rattle)
library(rpart.plot)
library(RGtk2)

options(scipen=999)

## 1
ls_1<- c("ESTADO",
         "CAT_OCUP",
         "CAT_INAC",
         "IMPUTA",
         "PP02C1",
         "PP02C2",
         "PP02C3",
         "PP02C4",
         "PP02C5",
         "PP02C6",
         "PP02C7",
         "PP02C8",
         "PP02E",
         "PP02H",
         "PP02I",
         "PP03C",  
         "PP03D",
         "PP2_TOT",
         "PP3F_TOT",
         "PP03G",
         "PP03H",
         "PP03I",  
         "PP03J",
         "INTENSI",
         "PP04A",  
         "PP04A",
         "PP04B_COD",
         "PP04BA",
         "PP04B2",
         "PP04B3_MES",
         "PP04B3_ANO",
         "PP04C",
         "PP04C99",
         "PP04D_COD",
         "PP04G",
         "PP05B2_MES",
         "PP05B2_ANO",
         "PP05B2_DIA",
         "PP05C_1",
         "PP05C_2",
         "PP05C_3",
         "PP05E",
         "PP05F",
         "PP05H",
         "PP06A",
         "PP06C",
         "PP06D",
         "PP06E",
         "PP06H",
         "PP07A",
         "PP07C",
         "PP07D",
         "PP07E",
         "PP07F1",
         "PP07F2",
         "PP07F3",
         "PP07F4",
         "PP07F5",
         "PP07G1",
         "PP07G2",
         "PP07G3",
         "PP07G4",
         "PP07G_59",
         "PP07H",
         "PP07I",
         "PP07J",
         "PP07K",
         "PP08D1",
         "PP08D4",
         "PP08F1",
         "PP08F2",
         "PP08J1",
         "PP08J2",
         "PP08J3",
         "PP09A",
         "PP09A_ESP",
         "PP09B",
         "PP09C",
         "PP09C_ESP",
         "PP10A",
         "PP10C",
         "PP10D",
         "PP10E",
         "PP11A",
         "PP11B_COD",
         "PP11B1",
         "PP11B2_MES",
         "PP11B2_ANO",
         "PP11B2_DIA",
         "PP11C",
         "PP11C99",
         "PP1D_COD",
         "PP11G_ANO",
         "PP11G_MES",
         "PP11G_DIA",
         "PP11L",
         "PP11L1",
         "PP11M",
         "PP11N",
         "PP11O",
         "PP11P",
         "PP11Q",
         "PP11R",
         "PP11S",
         "PP11T",
         "PP3E_TOT",
         "PP04B1",
         "PP04B3_DIA",
         "PP11D_COD",
         "TOT_P12",
         "RDECCFR"
)

# ingresos de la ocupacion ppal
ls_2 <- c("P21",
          "DECOCUR",
          "IDECOCUR",
          "RDECOCUR",
          "GDECOCUR",
          "PDECOCUR",
          "ADECOCUR",
          "PONDIIO")

# ingresos de otras ocupaciones
ls_3 <- c("TOT_P21"
)

# ingresos  total individual
ls_4 <- c("P47T",
          "DECINDR",
          "IDECINDR",
          "RDECINDR",
          "GDENCIDR",
          "PDECINDR",
          "ADECINDR",
          "PONDII")
# ingresos no laborales
ls_5 <- c("V2_M",
          "V3_M",
          "V4_M",
          "V5_M",
          "V8_M",
          "V9_M",
          "V10_M",
          "V11_M",
          "V12_M",
          "V18_M",
          "V19_AM",
          "V21_M",
          "T_VI")
# ingreso total familiar
ls_6 <- c("ITF",
          "DECIFR",
          "IDECIFR",
          "RDECIFR",
          "GDECIFR",
          "ADECIFR"
)
# ingreso per capita familiar
ls_7 <- c(#"IPCF",
          "DECCFR",
          "IDECCFR",
          "RFECCFR",
          "GDECCFR",
          "PDECCFR",
          "ADECCFR",
          "PONDIH"
)


dataset_mujeres <- subset(dataset, dataset$CH04 == 2)
dataset_varones <- subset(dataset, dataset$CH04 == 1)

tabla_missin <- sapply(dataset, function(x) sum(is.na(x)))
View(tabla_missin)

tabla_missin <- sapply(dataset_mujeres, function(x) sum(is.na(x)))
View(tabla_missin)


dataset_mujeres <- dataset_mujeres[ , ! apply( dataset_mujeres , 2 , function(x) all(is.na(x)) ) ]
#dataset_mujeres <- subset(dataset_mujeres, select = -c(27:156))
dataset_mujeres <- dataset_mujeres[,!(names(dataset_mujeres) %in% c(ls_1,
                                                                    ls_2,
                                                                    ls_3,
                                                                    ls_4,
                                                                    ls_5,
                                                                    ls_6,
                                                                    ls_7))]

dataset_mujeres <- subset(dataset_mujeres, select = -c(CODUSU,ANO4,TRIMESTRE,H15,PONDERA))



dataset_varones <- dataset_varones[ , ! apply( dataset_varones , 2 , function(x) all(is.na(x)) ) ]
#dataset_mujeres <- subset(dataset_mujeres, select = -c(27:156))
dataset_varones <- dataset_varones[,!(names(dataset_varones) %in% c(ls_1,
                                                                    ls_2,
                                                                    ls_3,
                                                                    ls_4,
                                                                    ls_5,
                                                                    ls_6,
                                                                    ls_7))]

dataset_varones <- subset(dataset_varones, select = -c(CODUSU,ANO4,TRIMESTRE,H15,PONDERA))

#dataset_mujeres <- lapply(dataset_mujeres, as.integer)
#dataset_mujeres$IPCF <- as.numeric(dataset_mujeres$IPCF)
#dataset_mujeres$ITF <- as.numeric(dataset_mujeres$ITF)

#dataset_mujeres[] <- lapply(dataset_mujeres, as.factor)
#str(dataset_mujeres)
#dataset_mujeres$CH06 <- as.integer(dataset_mujeres$CH06)
#dataset_mujeres$CH12 <- as.numeric(dataset_mujeres$CH12)
#dataset_mujeres$NIVEL_ED <- as.factor(dataset_mujeres$NIVEL_ED)

names(dataset_mujeres)[names(dataset_mujeres) == "CH06"] <- "Edad"
names(dataset_mujeres)[names(dataset_mujeres) == "CH03"] <- "Relación de Parentesco"
names(dataset_mujeres)[names(dataset_mujeres) == "NIVEL_ED"] <- "Nivel Educativo"
names(dataset_mujeres)[names(dataset_mujeres) == "CH08"] <- "Tipo de covertura médica"

arbol_1M <- rpart(trabaja ~. , dataset_mujeres, method="class" )

jpeg("Output/tree_1_M.jpg",width = 900, height = 900)
fancyRpartPlot(arbol_1M,sub = "")
dev.off()


names(dataset_varones)[names(dataset_varones) == "CH06"] <- "Edad"
names(dataset_varones)[names(dataset_varones) == "CH10"] <- "Asiste a establecimiento educativo"

arbol_1H <- rpart(trabaja ~. , dataset_varones, method="class" )

jpeg("Output/tree_1_H.jpg",width = 900, height = 900)
fancyRpartPlot(arbol_1H,sub = "")
dev.off()


## 4
# MUJERES
set.seed(666)
train_M <- sample(1:nrow(dataset_mujeres), nrow(dataset_mujeres)/2)
test_M  <- dataset_mujeres[-train_M,]
trabaja_test_M <- dataset_mujeres$trabaja[-train_M]

arbol_4M <- rpart(trabaja ~. , dataset_mujeres[train_M,], method="class" )
hat_M <- predict(arbol_4M, test_M, type="class")
table(hat_M,trabaja_test_M)

#HOMBRES
set.seed(666)
train_H <- sample(1:nrow(dataset_varones), nrow(dataset_varones)/2)
test_H  <- dataset_varones[-train_H,]
trabaja_test_H <- dataset_varones$trabaja[-train_H]

arbol_4H <- rpart(trabaja ~. , dataset_varones[train_H,], method="class" )
hat_H <- predict(arbol_4H, test_H, type="class")
table(hat_H,trabaja_test_H)


## 5
#-> LASSO <-#
library(Matrix) # Estos dos paquetes se necesitan para el paquete glmnet
library(foreach)
library(glmnet)
library(class)

set.seed(101) # para reproducir los resultados

grid=10^seq(-3,10,length=100)


# HOMBRES
y_H <- dataset_varones$trabaja
x_H <- model.matrix(dataset_varones$trabaja ~. ,dataset_varones)[,-1]

cv.out.lasso_H=cv.glmnet(x_H ,y_H ,alpha=1, lambda = grid)

plot(cv.out.lasso_H, ylab="Error Cuadrático Medio")
title("LASSO", line = 2.5)

lam_H=cv.out.lasso_H$lambda.min
lam_H

# Base de entrenamiento
L_train=sample(1:nrow(x_H), nrow(x_H)/2)
# Base de prueba
L_test=L_train[-L_train]
# Vector de respuesta para testear el error de prediccion
y.test_H=y_H[L_test]

lasso_H <- glmnet(x_H[L_train,],y_H[L_train], alpha = 1, lambda = grid)
lasso.coef_H=predict(lasso_H,type="coefficients",s=lam_H)
length(round(lasso.coef_H[lasso.coef_H!=0],4))
#View(round(lasso.coef_H[lasso.coef_H!=0],4))
abcd <- round(lasso.coef_H[lasso.coef_H==0])


#hat_lasso_H <- predict(lasso_H, s=lam_H, newx = x_H[L_test,])

hat_lasso_H <- predict(lasso_H, s=lam_H, newx = x_H[-L_train,])

#hat_lasso_H <- floor(hat_lasso_H)
hat_lasso_H[hat_lasso_H<=0.5] <- 0
hat_lasso_H[hat_lasso_H>0.5] <- 1
#table(hat_lasso_H,y.test_H)

table(hat_lasso_H, y_H[-L_train])


# MUJERES
y_M <- dataset_mujeres$trabaja
x_M <- model.matrix(trabaja ~.,dataset_mujeres)[,-1]

cv.out.lasso_M=cv.glmnet(x_M ,y_M ,alpha=1, lambda = grid)

plot(cv.out.lasso_M, ylab="Error Cuadrático Medio")
title("LASSO", line = 2.5)

lam_M=cv.out.lasso_M$lambda.min
lam_M

# Base de entrenamiento
L_train=sample(1:nrow(x_M), nrow(x_M)/2)
# Base de prueba
L_test=L_train[-L_train]
# Vector de respuesta para testear el error de prediccion
y.test_M=y_M[L_test]

lasso_M <- glmnet(x_M[L_train,],y_M[L_train], alpha = 1, lambda = grid)
lasso.coef_M=predict(lasso_M,type="coefficients",s=lam_M)
length(round(lasso.coef_M[lasso.coef_M!=0],4))
#hat_lasso_M <- predict(lasso_M, s=lam_M, newx = x_M[L_test,])

hat_lasso_M <- predict(lasso_M, s=lam_M, newx = x_M[-L_train,])

#hat_lasso_M <- floor(hat_lasso_M)
hat_lasso_M[hat_lasso_M<=0.5] <- 0
hat_lasso_M[hat_lasso_M>0.5] <- 1
#table(hat_lasso_M,y.test_M)
table(hat_lasso_M, y_M[-L_train])

## 6
clean_dataset <- dataset[ , ! apply( dataset , 2 , function(x) all(is.na(x)) ) ]
#dataset_mujeres <- subset(dataset_mujeres, select = -c(27:156))
clean_dataset <- clean_dataset[,!(names(clean_dataset) %in% c(ls_1,
                                                              ls_2,
                                                              ls_3,
                                                              ls_4,
                                                              ls_5,
                                                              ls_6,
                                                              ls_7))]

clean_dataset <- subset(clean_dataset, select = -c(CODUSU,ANO4,TRIMESTRE,H15,PONDERA))


#clean_dataset[] <- lapply(clean_dataset, as.factor)
#str(clean_dataset)
#
#clean_dataset$CH04 <- as.factor(clean_dataset$CH04)
#
#clean_dataset$CH06 <- as.integer(clean_dataset$CH06)
#clean_dataset$CH12 <- as.numeric(clean_dataset$CH12)
#clean_dataset$NIVEL_ED <- as.factor(clean_dataset$NIVEL_ED)

## -- > factor
#NRO_HOGAR
#COMPONENTE
#REGION
#AGLOMERADO
#CH03
#CH04
#CH07
#CH08
#CH09
#CH10
#CH11
#CH12
#CH13
#CH15
#CH16

# --> numeric
#CH06
#CH12 (PODRIA IR)
#CH14
#NIVEL_EDU
names(clean_dataset)[names(clean_dataset) == "CH06"] <- "Edad"

arbol_6 <- rpart(trabaja ~. , clean_dataset, method="class" )

jpeg("Output/tree_6.jpg",width = 900, height = 900)
fancyRpartPlot(arbol_6,sub = "")
dev.off()


set.seed(666)
train <- sample(1:nrow(clean_dataset), nrow(clean_dataset)/2)
test  <- clean_dataset[-train,]
trabaja_test <- clean_dataset$trabaja[-train]



arbol_6 <- rpart(trabaja ~. , clean_dataset[train,], method="class" )
hat <- predict(arbol_6, test, type="class")
table(hat,trabaja_test)
