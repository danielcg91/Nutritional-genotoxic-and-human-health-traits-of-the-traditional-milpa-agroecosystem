####Correlación de Pearson
###10/10/2025
#Laboratorio de Geoquímica 

#Dirección de trabojo 

setwd("~/Estadistica Lili/Suelo_Raíz_Fruto_Cometa")

#Cargar base de datos

TT <- read_excel("~/Estadistica Lili/Correlograma suelo/Suelo.xlsx", 
                 sheet = "Hoja1")

#------------------------------------------------------------
# 1. Librerías
#------------------------------------------------------------
library(Hmisc)       # Para rcorr()
library(corrplot)    # Para correlogramas
library(openxlsx)    # Para exportar a Excel

#Normalización 

TT <- TT %>%
  mutate(across(everything(),
                ~as.numeric(as.character(.))))
TT [is.na(TT)]<-0
TT <- TT + 1
TT <- log10(TT)


# Correlaciones (Pearson)

Corr  <- cor(TT, method = "pearson")
round(Corr, 3)

corr   <- rcorr(as.matrix(TT), type = "pearson")
valor_r  <- corr$r
valor_p  <- corr$P

### sihay error de numero de columas usar este script
corr <- corr.test(TT,
                  method = "pearson",
                  use = "complete")  # ← MISMO n para todo

valor_r <- corr$r
valor_p <- corr$p


#------------------------------------------------------------
# 3. Correlogramas
#------------------------------------------------------------

# Correlograma clásico
corrplot(valor_r, method = "circle")
corrplot(valor_r, method = "pie")
corrplot(valor_r, method = "color")

# Solo parte superior (upper)
corrplot(valor_r,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)

# Con significancia (oculta valores p > 0.05)
corrplot(valor_r,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.6,      # ⬅ tamaño del texto (labels)
         cl.cex = 0.6,      # ⬅ tamaño de la leyenda
         number.cex = 0.6,
         p.mat = valor_p,
         sig.level = 0.05,
         insig = "blank")