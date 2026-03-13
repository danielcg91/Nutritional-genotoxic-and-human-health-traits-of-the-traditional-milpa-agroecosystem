#ANALISIS DE COMPONENTES PRINCIPALES PARA SUELOS, RAIZES Y PLANTAS
#10/09/2025
#LABORATORIO DE GEOQUIMICA

#Instalar paqueterias
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("ggplot2")
install.packages("vegan")
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("vegan")

#Importar datos
setwd("~/Estadistica")

Suelo1 <- read.csv("Suelo.txt",header=TRUE, sep="\t")


#convercion de datos de con valor de 0 a na
Suelo1[is.na(Suelo1)]<-0
View(Suelo1)

#Ajustar los datos, crear tabla sin la columna de nombres
data_PCA <- data.matrix(Suelo1[,2:37])

#Indicar a la nueva tabla de donde tomar los nombres
rownames(data_PCA) <- Suelo1[,1:1]

#Informacion sin normalizar

#1er PCA, cor = normalizado
res.pca1 <- princomp(data_PCA, cor = TRUE, scores = TRUE)
summary(res.pca1)
print(res.pca1)

##1.2 PCA cuando no funciona princomp####
res.pca <- PCA(data_PCA)

#2do PCA, Scale.unit = normalizado
PCA (data_PCA, scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca2 <- PCA(data_PCA, scale.unit = TRUE, ncp = 16, graph = TRUE)
print(res.pca2)

#Graficar componentes principales
fviz_pca_biplot(res.pca2, axes = c(1, 2), geom = c("point", "text"),labelsize = 3, pointsize = 1, arrowsize = 0.8, repel = TRUE)
fviz_pca_biplot(res.pca2, axes = c(2, 3), geom = c("point", "text"),labelsize = 5, pointsize = 2, arrowsize = 0.3, repel = TRUE)

#Extraer datos del PCA

Eig_values.csv <- res.pca2[["eig"]]
write.csv(x = Eig_values.csv, file = "eig_Suelo.csv", sep = ",", row.names = TRUE, col.names = TRUE)
dir()

loadings.csv <- res.pca1[["loadings"]]
write.csv(x = loadings.csv, file = "loadings_Suelo.csv", sep = ",", row.names = TRUE, col.names = TRUE)

# Contribucion de variables a los componentes 1 y 2
fviz_contrib(res.pca2, choice = "var", axes = 1)
fviz_contrib(res.pca2, choice = "var", axes = 2)

var <- get_pca_var(res.pca1)
Contribucion.csv <- var[["contrib"]]
write.csv(x = Contribucion.csv, file = "contrib_Suelo.csv", sep = ",", row.names = TRUE, col.names = TRUE)


#Normalizar datos (https://search.r-project.org/CRAN/refmans/vegan/html/decostand.html)

data_N_PCA <- round(decostand(data_PCA, method="hellinger"), 5)

#1er PCA, cor = normalizado
res.pca1N <- princomp(data_N_PCA, cor = TRUE, scores = TRUE)
summary(res.pca1N)
print(res.pca1N)

#2do PCA, Scale.unit = normalizado
PCA (data_N_PCA, scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca2N <- PCA(data_N_PCA, scale.unit = TRUE, ncp = 16, graph = TRUE)
print(res.pca2N)

#Graficar componentes principales
fviz_pca_biplot(res.pca2N, axes = c(1, 2), geom = c("point", "text"),labelsize = 3, pointsize = 1, arrowsize = 0.8, repel = TRUE)
fviz_pca_biplot(res.pca2N, axes = c(2, 3), geom = c("point", "text"),labelsize = 5, pointsize = 2, arrowsize = 0.3, repel = TRUE)

#Extraer datos del PCA

Eig_values.csv <- res.pca2N[["eig"]]
write.csv(x = Eig_values.csv, file = "eig_Suelo_hellinger.csv", sep = ",", row.names = TRUE, col.names = TRUE)
dir()

loadings.csv <- res.pca1N[["loadings"]]
write.csv(x = loadings.csv, file = "loadings_Suelo_hellinger.csv", sep = ",", row.names = TRUE, col.names = TRUE)

# Contribucion de variables a los componentes 1 y 2
fviz_contrib(res.pca2N, choice = "var", axes = 1)
fviz_contrib(res.pca2N, choice = "var", axes = 2)

var <- get_pca_var(res.pca1)
Contribucion.csv <- var[["contrib"]]
write.csv(x = Contribucion.csv, file = "contrib_Suelo_ hellinger.csv", sep = ",", row.names = TRUE, col.names = TRUE)


#Combinar análisis de cluster al PCA

#Euclidean, observar la compocision de los cruster que se formaron en el PCA

hcpc_ward<-HCPC(res.pca2,metric="euclidean",method="ward",nb.clust= "",graph=T)
plot(hcpc_ward,choice="map",ind.names=TRUE)
hcpc$call$t$nb.clus


#######################
#Modificar el PCA

hcpc<-HCPC(res.pca2N,metric="euclidean", method="ward",nb.clust= "",graph=F) 

grupos<-factor(hcpc$data.clust$clust);grupos


# Biplot of individuals and variables

library("factoextra")

fviz_pca_biplot(res.pca2N, label = c("var","ind"), 
                addEllipses=TRUE,habillage=grupos,
                title="Suelo_Helliger_euclidean_ward",pointsize = 1.5,labelsize = 4)+ 
  scale_color_brewer(palette="Dark2")