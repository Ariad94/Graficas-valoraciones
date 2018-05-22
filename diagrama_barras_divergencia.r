### Diagramas de barras de divergencia

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de barras por empleado de la diferencia entre su puntuación final y el nivel requerido
# Adecuacion del empleado al perfil

Requerido <- 80

programadores$diferencia <- programadores$FINAL - Requerido
programadores$diferencia_tipo <- ifelse(programadores$diferencia <0, "Por debajo del nivel requerido", "Por encima del nivel requerido")
programadores <- programadores[order(programadores$diferencia), ]
programadores$Nombre.Empleado <- factor(programadores$Nombre.Empleado, levels = programadores$Nombre.Empleado) 

programadores %>% ggplot(aes(Nombre.Empleado,diferencia, label = diferencia)) + 
 geom_bar(stat = "identity", aes(fill = diferencia_tipo), width =.5) + coord_flip() + 
 labs(title ="Adecuación al perfil por empleado", x = "Empleados", y = "Diferencia respecto al nivel requerido") +
 scale_fill_discrete(name = "") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0))
