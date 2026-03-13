#Shaphiro wilK. t-studen y Boxplot
#Fecha:04/08/25
#Laboratorio de Geoquímica
#La tecnica de shaphira will es para ver la normalidad de los datos
#T-studen es para verificar la diferencias estadiscamente significativa de dos ppblaciones
#Boxplot es para ver la distibucion de los datos en vase a su mediana

#####
#Paquetes a utilizar 

install.packages("readr")
library(readr)
install.packages("tidyverse")
library(tidyverse) ## Para manipulación y visualización de datos
library(ggplot2)
install.packages("ggpubr")
library(ggpubr) ##para crear gr?ficas
install.packages("rstatix")
library(rstatix) ##para an?lisis estad?sticos
install.packages("datarium")
library(datarium) ##contiene algunas bases de datos para ejercicios
library(carData)
library(car) ##para análisis estadísticos
library(stats)
library(dplyr)
install.packages("readxl")
library(readxl)

####Establecer la dirección de trabajo
setwd("~/Estadistica Lili")

####Base de datos
library(readxl)
Tabla_para_estadistica <- read_excel("Tabla para estadistica.xlsx")
View(Tabla_para_estadistica)

planta <- Tabla_para_estadistica

#converSión de chr a factor
planta$ID <- factor(planta$ID)
str(planta)

#####Shaphiro wills 
# Aplicar la prueba de Shapiro-Wilk para verificar la normalidad de varias columnas

# Prueba de Shapiro-Wilk para cada variable
shapiro_test_results <- planta %>%
  select(Ca, Mg, Na, K, Ba, Cr, Mn, Fe, As, Pb, Cd, Cu, Zn) %>% # Seleccionar las columnas de interés
  gather(key = "Metal", value = "Concentration") %>%  # Convertir las columnas en formato largo
  group_by(Metal) %>%
  shapiro_test(Concentration)  # Aplicar la prueba de Shapiro-Wilk

# Mostrar los resultados de la prueba
print(shapiro_test_results)

## Anova, T-studen y Boxplot

library(ggpubr)
library(dplyr)
library(rstatix)
library(ggplot2)

analisis_metal <- function(metal) {
  cat("\n\nAnálisis de:", metal, "\n")
  
  formula <- as.formula(paste(metal, "~ ID"))
  niveles <- unique(planta$ID)
  
  # ANOVA
  anova_res <- aov(formula, data = planta)
  print(summary(anova_res))
  
  # Supuestos del ANOVA
  par(mfrow = c(1, 2))
  plot(anova_res, 1) # Homocedasticidad
  plot(anova_res, 2) # Normalidad
  par(mfrow = c(1, 1))
  print(shapiro.test(anova_res$residuals))
  
  # Base del gráfico (solo boxplot)
  g_base <- ggplot(planta, aes(x = ID, y = .data[[metal]], fill = ID)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
    geom_jitter(width = 0.1, size = 1.5, alpha = 0.7) +
    stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "blue", fatten = 1.5) +
    theme_minimal() +
    labs(title = metal, y = paste(metal, "(Conc)"), x = "Grupo") +
    theme(legend.position = "none")
  
  if (length(niveles) == 2) {
    # t-test
    t_test_res <- t.test(formula, data = planta)
    print(t_test_res)
    
    p_value <- t_test_res$p.value
    signif_label <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", "ns")))
    
    y_max <- max(planta[[metal]], na.rm = TRUE)
    
    g <- g_base +
      annotate("text", x = 1.5, y = y_max * 1.05,
               label = paste("p =", signif(p_value, 3), signif_label),
               size = 5)
    print(g)
    
  } else {
    # Kruskal-Wallis + Dunn
    print(kruskal_test(formula, data = planta))
    
    dunn <- planta %>%
      dunn_test(formula, p.adjust.method = "bonferroni") %>%
      add_xy_position(x = "Matriz", y = metal)
    
    print(dunn)
    
    g <- g_base +
      stat_pvalue_manual(dunn, label = "p.adj.signif", tip.length = 0.01)
    
    print(g)
  }
}

# Ejecutar para cada variable
for (metal in c("As", "Ba", "Ca", "Cd", "Cr", "Cu", "Fe", 
                "K", "Mg", "Mn", "Na", "Pb", "Zn")) {
  analisis_metal(metal)
}



###Agrupación de Boxplot

library(ggpubr)
library(dplyr)
library(rstatix)
library(ggplot2)

# Lista donde se guardarán los gráficos
graficas_boxplot <- list()

analisis_metal <- function(metal) {
  cat("\n\nAnálisis de:", metal, "\n")
  
  formula <- as.formula(paste(metal, "~ ID"))
  niveles <- unique(planta$ID)
  
  # ANOVA
  anova_res <- aov(formula, data = planta)
  print(summary(anova_res))
  
  # Supuestos del ANOVA
  par(mfrow = c(1, 2))
  plot(anova_res, 1) # Homocedasticidad
  plot(anova_res, 2) # Normalidad
  par(mfrow = c(1, 1))
  print(shapiro.test(anova_res$residuals))
  
  # Base del gráfico (solo boxplot)
  g_base <- ggplot(planta, aes(x = ID, y = .data[[metal]], fill = ID)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.6, color = "black") +
    geom_jitter(width = 0.1, size = 1.5, alpha = 0.7) +
    stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "blue", fatten = 1.5) +
    theme_minimal() +
    labs(title = metal, y = paste(metal, "(Conc)"), x = "Grupo") +
    theme(legend.position = "none")
  
  if (length(niveles) == 2) {
    # t-test
    t_test_res <- t.test(formula, data = planta)
    print(t_test_res)
    
    p_value <- t_test_res$p.value
    signif_label <- ifelse(p_value < 0.001, "***",
                           ifelse(p_value < 0.01, "**",
                                  ifelse(p_value < 0.05, "*", "ns")))
    
    y_max <- max(planta[[metal]], na.rm = TRUE)
    
    g <- g_base +
      annotate("text", x = 1.5, y = y_max * 1.05,
               label = paste("p =", signif(p_value, 3), signif_label),
               size = 4)
    
  } else {
    # Kruskal-Wallis + Dunn
    print(kruskal_test(formula, data = planta))
    
    dunn <- planta %>%
      dunn_test(formula, p.adjust.method = "bonferroni") %>%
      add_xy_position(x = "Matriz", y = metal)
    
    print(dunn)
    
    g <- g_base +
      stat_pvalue_manual(dunn, label = "p.adj.signif", tip.length = 0.01)
  }
  
  # Guardar gráfico en la lista
  graficas_boxplot[[metal]] <<- g
}

# Ejecutar para cada variable
metales <- c("Ca", "Mg", "Na", "K", 
             "Ba", "Cr", "Mn", "Fe", "As", "Pb", "Cd", "Cu", "Zn")

for (metal in metales) {
  analisis_metal(metal)
}

# Mostrar todos los gráficos juntos (por ejemplo, 4 columnas)
panel_final <- ggarrange(plotlist = graficas_boxplot, 
                         ncol = 4, nrow = ceiling(length(graficas_boxplot)/4),
                         labels = names(graficas_boxplot),
                         font.label = list(size = 10))

# Mostrar panel en visor
print(panel_final)

# Opcional: guardar en archivo
# ggsave("boxplots_todos_juntos.png", panel_final, width = 18, height = 12, dpi = 300)

