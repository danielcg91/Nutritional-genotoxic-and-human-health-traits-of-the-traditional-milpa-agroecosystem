# Carga del paquete
library(ppcor)
library(rio)
library(dplyr)
library(corrplot)
library(corpcor)
library(readxl)
library(tidyverse)
library(tidyHeatmap)
library(Hmisc)
library(tidyr)
library(tibble)
library(stats)
library(bbmle)
library(performance)
library(sjPlot)
library(broom)

# Cálculo de la Correlación Simple: Metales Suelo vs Raiz 
#Squash_C <-read_excel(file.choose(),sheet="Sq_S&R_PC",range="G1:AF15")
#Squash_C <- read_excel(file.choose(),sheet="Sq_S&Fr_PC",range="G1:AF15")
Squash_C <- read_excel(file.choose(),sheet="Sq_R&Fr_PC",range="A1:Z15")
#Squash_R <- read_excel(file.choose(),sheet="Sq_S&R_PR",range="G1:AF15")
#Squash_R <- read_excel(file.choose(),sheet="Sq_S&Fr_PR",range="G1:AF15")
Squash_R <- read_excel(file.choose(),sheet="Sq_R&Fr_PR",range="A1:Z15")
#Squash_All <- read_excel(file.choose(),sheet="Step_RL",range="A1:X29")



##transformacion
Squash_C <- Squash_C %>% 
  mutate(across(everything(), ~as.numeric(as.character(.))))
Squash_C [is.na(Squash_C)]<-0
Squash_C <- Squash_C + 1
Squash_C <- log10(Squash_C)

##quitar elementos
#   -Fe_S, -Fe_Fr
Squash_C <- Squash_C %>% select(-Ca_S, -Mg_S, -Na_S,-K_S, 
                                -Ca_Fr, -Mg_Fr,-Na_Fr, -K_Fr)

###both
Squash_R <- Squash_R %>% 
  mutate(across(everything(), ~as.numeric(as.character(.))))
Squash_R [is.na(Squash_R)]<-0
Squash_R <- Squash_R + 1
Squash_R <- log10(Squash_R)

###quitar elementos
#-Ba_S, -Fe_S,-Ba_Fr, -Fe_Fr
Squash_R <- Squash_R %>% select(-Ca_S, -Mg_S, -Na_S,-K_S,
                                -Ca_Fr, -Mg_Fr,-Na_Fr, -K_Fr)
####juntar
Squash_CR <- rbind(Squash_C,Squash_R)
Squash_CR <- log(Squash_CR)
#Squash_CR_std <- scale(Squash_CR)
#Squash_CR_std <- as.data.frame(Squash_CR_std)

mis_datos <- Squash_CR
#mis_datos <- Squash_All


# Añade aquí todos los que quieras analizar
metal_lista <- c("Ca_Fr", "Mg_Fr",  "Na_Fr", "K_Fr", 
  "Ba_Fr", "Cr_Fr", "Mn_Fr", "Fe_Fr", "As_Fr", "Pb_Fr", "Cd_Fr", "Cu_Fr", "Zn_Fr")

# Lista de metales en el SUELO (Variables X - Predictores)


suelo_vars <- c("Ca_R", "Mg_R", "Na_R", "K_R", 
"Ba_R", "Cr_R",  "Mn_R", "Fe_R", "As_R", "Pb_R", "Cd_R", "Cu_R", "Zn_R")


# Creamos la tabla vacía donde guardaremos todo
tabla_acumulada <- data.frame()


# --- 2. BUCLE AUTOMÁTICO ---
for (metal_actual in metal_lista) {
  
  message(paste("Procesando:", metal_actual, "..."))
  
  # A. PREPARAR EL TERRENO (Truco para evitar el error de filas)
  # Creamos vectores vacíos (NA) del tamaño exacto de las variables de suelo.
  # Así, si el cálculo falla, ya tenemos el espacio reservado.
  n_vars <- length(suelo_vars)
  mis_correlaciones <- rep(NA, n_vars) 
  mis_p_values      <- rep(NA, n_vars)
  
  # B. INTENTAR EL CÁLCULO
  try({
    # Seleccionamos las columnas: Metal Actual + Todo el Suelo
    # na.omit() borra filas vacías para evitar errores matemáticos
    vars_interes <- na.omit(mis_datos[, c(metal_actual, suelo_vars)])
    
    # Calculamos Correlación Parcial
    pcor_res <- pcor(vars_interes, method = "pearson")
    
    # Si logramos resultados, sobrescribimos los NAs con los datos reales
    # [-1, 1] toma la primera columna (metal fruto) quitando la primera fila (él mismo)
    if (!is.null(pcor_res$estimate)) {
      mis_correlaciones <- pcor_res$estimate[-1, 1]
      mis_p_values      <- pcor_res$p.value[-1, 1]
    }
    
  }, silent = TRUE) # Si falla, no pasa nada, seguimos con los NAs
  
  # C. GUARDAR RESULTADOS
  # Creamos la fila temporal usando 'suelo_vars' que es fijo y seguro.
  tabla_temporal <- data.frame(
    Metal_Analizado = rep(metal_actual, n_vars), # Repite el nombre "Ca_Fr" para todas las filas
    Variable_Suelo  = suelo_vars,                # Lista fija de nombres de suelo
    Correlacion     = mis_correlaciones,         # Datos (o NAs si falló)
    P_Value         = mis_p_values,              # Datos (o NAs si falló)
    stringsAsFactors = FALSE
  )
  
  # Pegamos a la tabla principal
  tabla_acumulada <- rbind(tabla_acumulada, tabla_temporal)
}

# --- 3. RESULTADO FINAL ---
print("--- ¡Listo! ---")
print(head(tabla_acumulada))
###
options(scipen = 999)
# Ahora imprime tu tabla y verás números normales
print(head(tabla_acumulada))

write.csv(tabla_acumulada, "Correlacion_Parcial_BOTH_Soil_Fruit_2.csv", row.names = FALSE)

###GRAFICA
# --- PASO PREVIO: Forzar el orden basado en la aparición en la tabla ---

 ##############CORPLOT

matriz_R <-  t(tabla_acumulada %>%
  select(Variable_Suelo, Metal_Analizado, Correlacion) %>% # Seleccionamos columnas clave
  pivot_wider(names_from = Metal_Analizado, values_from = Correlacion) %>%
  column_to_rownames("Variable_Suelo") %>%
  as.matrix())


#2. Transformar la tabla de P-VALUES (P) a Matriz (Igual que arriba)
matriz_P <- t(tabla_acumulada %>%
  select(Variable_Suelo, Metal_Analizado, P_Value) %>%
  pivot_wider(names_from = Metal_Analizado, values_from = P_Value) %>%
  column_to_rownames("Variable_Suelo") %>%
  as.matrix())
  #as.data.frame()

mis_colores <- colorRampPalette(c("firebrick", "white", "forestgreen"))(30) 


# tiff("corr_S&F_bot.tiff", 
#      width = 1080, height = 1080, units = "px", 
#      res = 200)

corrplot(matriz_R,                 # La matriz de correlaciones
        method = "color", 
        type = "full",             # Círculos
        col = mis_colores,        # Tus colores definidos arriba
        tl.col = "black",         # Color del texto (etiquetas)         
        p.mat = matriz_P,       # La matriz de P-values para filtrar
        sig.level = 0.05,         # Nivel de significancia
        insig = "label_sig",    # Activa las etiquetas de significancia
        pch.cex = 1.5,          # Tamaño de las estrellas
        pch.col = "black",       # "blank" oculta los no significativos
)

#dev.off()


################ MODELO LINEAL MULTIPLE PASO A PASO

#que metales en el suelo predicen la concentracion de metales en fruto
# Ejemplo con Cr_S como variable dependiente

# 1. Definir el "Modelo Completo" (con todo lo que tienes en el suelo)
modelo_completo <- lm(Pb_Fr ~ Ca_R + Mg_R + Na_R + K_R +  Ba_R + Cr_R + Mn_R + Fe_R + As_R + Pb_R + Cd_R + Cu_R + Zn_R, data = mis_datos)

# 2. Definir el "Modelo Nulo" (Vacío, solo el promedio)
modelo_nulo <- lm(Pb_Fr ~ 1, data = mis_datos)

# 3. Ejecutar la selección automática (Both = quita y pone variables)
# Esto probará miles de combinaciones en segundos.
mejor_modelo_auto <- step(modelo_completo, 
                          scope = list(lower = modelo_nulo, upper = modelo_completo), 
                          direction = "both", 
                          trace = 0) # trace=0 para que no llene la pantalla de texto

# 4. Ver el ganador
summary(mejor_modelo_auto)


a= lm(Pb_Fr ~ K_R + Cr_R + Fe_R + Zn_R, 
      data = mis_datos)


summary(a)

############################################################################
############################################################################
############################################################################

save.image(file = "modeloslineales.RData") 
load(file = "modeloslineales.RData")

# SELECCIONAR EL MEJOR MODELO
# AIC: CRITERIO DE INFORMACION DE AKAIKE
# ENTRE MAS PEQUEÑO, MEJOR
AICctab(Cr_1,Cr_2,Cr_3,Cr_4,Cr_5,Cr_6,Cr_7,
        base = TRUE,  # AIC
        delta = TRUE, # AICtab
        sort = TRUE,  
        weights = TRUE)

# Esto elimina variables una por una basándose en el AIC hasta encontrar el mejor modelo
modelo_step_Cr<- step(Cr_1, direction = "both", trace = 0)

# C. Ver Resultados Finales
summary(modelo_step_Cr)





