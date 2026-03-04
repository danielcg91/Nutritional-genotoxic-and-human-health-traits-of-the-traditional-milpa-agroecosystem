# Carga del paquete
library(ppcor)
library(MASS)
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
#Maize_C <-read_excel(file.choose(),sheet="Ma_S&R_PC",range="G1:AF15")
#Maize_C <- read_excel(file.choose(),sheet="Ma_S&Fr_PC",range="G1:AF15")
Maize_C <- read_excel(file.choose(),sheet="Ma_R&Fr_PC",range="A1:Z15")
#Maize_R <- read_excel(file.choose(),sheet="Ma_S&R_PR",range="G1:AF15")
#Maize_R <- read_excel(file.choose(),sheet="Ma_S&Fr_PR",range="G1:AF15")
Maize_R <- read_excel(file.choose(),sheet="Ma_R&Fr_PR",range="A1:Z15")
#Maize_All <- read_excel(file.choose(),sheet="Step_RL",range="A1:X29")



##transformacion
Maize_C <- Maize_C %>% 
  mutate(across(everything(), ~as.numeric(as.character(.))))
Maize_C [is.na(Maize_C)]<-0
Maize_C <- Maize_C + 1
Maize_C <- log10(Maize_C)

##quitar elementos
#   -Fe_S, -Fe_Fr
Maize_C <- Maize_C %>% select(-Ca_R, -Mg_R, -Na_R,-K_R, -Fe_R,
                                -Ca_Fr, -Mg_Fr,-Na_Fr, -K_Fr, -Fe_Fr)

###both
Maize_R <- Maize_R %>% 
  mutate(across(everything(), ~as.numeric(as.character(.))))
Maize_R [is.na(Maize_R)]<-0
Maize_R <- Maize_R + 1
Maize_R <- log10(Maize_R)

###quitar elementos
#-Ba_S, -Fe_S,-Ba_Fr, -Fe_Fr
Maize_R <- Maize_R %>% select(-Ca_R, -Mg_R, -Na_R,-K_R, -Fe_R,
                                -Ca_Fr, -Mg_Fr,-Na_Fr, -K_Fr, -Fe_Fr)
####juntar
Maize_CR <- rbind(Maize_C,Maize_R)
Maize_CR <- log(Maize_CR)
#Maize_CR <- as.data.frame(Maize_CR)
#Maize_CR_std <- scale(Maize_CR)
#Maize_CR_std <- as.data.frame(Maize_CR_std)

mis_datos <- Maize_CR
#mis_datos <- Maize_All


# Añade aquí todos los que quieras analizar
metal_lista <- c("Ca_Fr", "Mg_Fr", "Na_Fr",  "K_Fr", 
  "Ba_Fr", "Cr_Fr", "Mn_Fr", "Fe_Fr", "As_Fr", "Pb_Fr", "Cd_Fr", "Cu_Fr", "Zn_Fr")

#metal_lista <- c("Ba_R", "Cr_R", "Mn_R", "Fe_R", "As_R", "Pb_R", 
  #                 "Cd_R", "Cu_R", "Zn_R")

# Lista de metales en el SUELO (Variables X - Predictores)


suelo_vars<- c(#"Ca_S", "Mg_S", #"K_S", "Ba_S", "Cr_S",  "Mn_S", Fe_S", "As_S", "Pb_S", "Cd_S", "Cu_S", "Zn_S"

suelo_vars <- "Ca_R", "Mg_R", "Na_R", "K_R", 
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
modelo_completo <- lm(Cu_Fr ~ Ca_R + Mg_R + Na_R + K_R + Ba_R + Cr_R + Mn_R + Fe_R + As_R + Pb_R + Cd_R + Cu_R + Zn_R, data = mis_datos)
#modelo_completo <- lm(Ca_Fr ~., data = mis_datos)

# 2. Definir el "Modelo Nulo" (Vacío, solo el promedio)
modelo_nulo <- lm(Cu_Fr ~ 1, data = mis_datos)

# 3. Ejecutar la selección automática (Both = quita y pone variables)
# Esto probará miles de combinaciones en segundos.
mejor_modelo_auto <- step(modelo_completo, 
                          scope = list(lower = modelo_nulo, upper = modelo_completo), 
                          direction = "both", 
                          trace = 0) # trace=0 para que no llene la pantalla de texto

# 4. Ver el ganador
summary(mejor_modelo_auto)


##################


a= lm(Cu_Fr ~ Mg_R+ K_R + Mn_R + Zn_R, data = mis_datos)

summary(a)

#####################################ln#######################################
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


# REVISAR SUPUESTOS

check_heteroscedasticity(mejor_modelo_auto) # NO HAY HETEROCEDASTICIDAD
check_normality(mejor_modelo_auto$residuals) # RESIDUALES NORMALES
shapiro.test(mejor_modelo_auto$residuals)
check_outliers(mejor_modelo_auto) # NO HAY OUTLIERS
# VISUALIZAR MODELO
plot_model(mejor_modelo_auto)


a= lm(As_Fr ~ Na_R + K_R + Ba_R + As_R + Cd_R + Cu_R, data = mis_datos)
summary(a)
