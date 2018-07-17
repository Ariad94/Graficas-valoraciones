### Diagramas de barras 

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de barras por empleado de la diferencia entre su puntuación final y el nivel requerido
# Adecuacion del empleado al perfil

Requerido <- 57.125
#Requerido <- 57.5

programadores$diferencia <- programadores$FINAL - Requerido
programadores$diferencia_tipo <- ifelse(programadores$diferencia <0, "Por debajo del nivel requerido", "Por encima del nivel requerido")
programadores <- programadores[order(programadores$diferencia), ]
programadores$Nombre.Empleado <- factor(programadores$Nombre.Empleado, levels = programadores$Nombre.Empleado) 

min <- round(min(programadores$diferencia),0)-1
max <- round(max(programadores$diferencia),0)+1

programadores %>% ggplot(aes(Nombre.Empleado,diferencia, label = diferencia)) + 
 scale_y_continuous(limits=c(min,max),breaks = seq(min,max, by = 3)) +
 geom_bar(stat = "identity", aes(fill = diferencia_tipo), width =.5) + coord_flip() + 
 labs(title ="Adecuación al perfil por empleado", x = "Empleados", y = "Diferencia de las puntuaciones finales respecto al nivel requerido") +
 scale_fill_discrete(name = "") + theme_minimal() + 
 theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=10))

### ### Diagrama de barras por empleado de la diferencia entre su puntuación final y el nivel requerido modificado
# Adecuacion del empleado al perfil

Requerido <- 57.125
Requerido_prueba <- 75

diferencia_gris <- programadores$FINAL - Requerido

diferencia_prueba <- programadores$FINAL - Requerido_prueba

diferencia_azul <- ifelse(diferencia_prueba >= 0, diferencia_prueba, 0)

diferencia_roja <- ifelse(diferencia_prueba < 0, diferencia_prueba, 0)

datos <- data.frame(Nombre.Empleado = programadores$Nombre.Empleado, diferencia_gris, diferencia_prueba, diferencia_azul, diferencia_roja)

datos <- datos[order(datos$diferencia_prueba),]

datos$Nombre.Empleado <- factor(datos$Nombre.Empleado, levels = datos$Nombre.Empleado) 

min <- min(round(min(diferencia_gris),0)-1, round(min(diferencia_prueba),0)-1)
max <- max(round(max(diferencia_gris),0)+1, round(max(diferencia_prueba),0)+1)

datos %>% ggplot() + 
 geom_bar(aes(Nombre.Empleado, diferencia_roja, fill = "Por debajo del nivel requerido modificado"), stat = "identity", , width =.5) + 
 geom_bar(aes(Nombre.Empleado, diferencia_azul, fill = "Por encima del nivel requerido modificado"), stat = "identity", , width =.5) + 
 geom_bar(aes(Nombre.Empleado, diferencia_gris, fill = "Diferencia con el nivel requerido real"), alpha = 0.5, stat = "identity", , width =.5) +
 scale_fill_manual("", values = c("grey","lightcoral","turquoise3")) + scale_y_continuous(limits=c(min,max),breaks = seq(min,max, by = 3)) +
 coord_flip() + 
 labs(title ="Adecuación al perfil por empleado si se modifica el nivel requerido", x = "Empleados", 
 y = "Diferencia de las puntuaciones finales respecto al nivel requerido modificado") +
 theme_minimal() + 
 theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=10))







## Diagrama de barras de porcentajes por debajo, por encima y en el nivel requerido

# Competencias 

n <- 55
nr <- c(2,2,2,3,2)
#nr <- c(2,2,2,2,3)

Competencias <- c(rep("Calidad y mejora continua",3),rep("Pensamiento analítico",3),
 rep("Capacidad de aprendizaje",3),rep("Solución de problemas",3), rep("Competencia digital",3))
Porcentaje <- rep(c("por debajo del nivel requerido", "en el nivel requerido", "por encima del nivel requerido"),5)
Valores <- c(length(which(programadores$C1 <nr[1]))*100/n, length(which(programadores$C1 == nr[1]))*100/n, length(which(programadores$C1 > nr[1]))*100/n,
 length(which(programadores$C2 <nr[2]))*100/n, length(which(programadores$C2 == nr[2]))*100/n, length(which(programadores$C2 > nr[2]))*100/n,
 length(which(programadores$C3 <nr[3]))*100/n, length(which(programadores$C3 == nr[3]))*100/n, length(which(programadores$C3 > nr[3]))*100/n,
 length(which(programadores$C4 <nr[4]))*100/n, length(which(programadores$C4 == nr[4]))*100/n, length(which(programadores$C4 > nr[4]))*100/n,
 length(which(programadores$C5 <nr[5]))*100/n, length(which(programadores$C5 == nr[5]))*100/n, length(which(programadores$C5 > nr[5]))*100/n)

datos <- data.frame(Competencias, Porcentaje, Valores)

ggplot(datos, aes(Competencias, Valores)) +  
 geom_bar(aes(fill = Porcentaje), position = "dodge", stat="identity", color = "black")+ 
 scale_fill_manual(values = c("palegreen3", "salmon", "skyblue")) +
 geom_text(aes(x = Competencias, y = Valores, label = round(Valores,0), group = Porcentaje),position = position_dodge(width = 0.9),
 vjust = -0.5, size = 4) + 
 labs(title="Competencias respecto al nivel requerido ", y = "Porcentaje del total") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"))


# Conocimientos 

n <- 55
nr <- c(3,2)

Conocimientos <- c(rep("Lenguajes de programación",3),rep("Herramientas de desarrollo (id)",3))
Porcentaje <- rep(c("por debajo del nivel requerido", "en el nivel requerido", "por encima del nivel requerido"),2)
Valores <- c(length(which(programadores$CN1 <nr[1]))*100/n, length(which(programadores$CN1 == nr[1]))*100/n, length(which(programadores$CN1 > nr[1]))*100/n,
 length(which(programadores$CN2 <nr[2]))*100/n, length(which(programadores$CN2 == nr[2]))*100/n, length(which(programadores$CN2 > nr[2]))*100/n)

datos <- data.frame(Conocimientos, Porcentaje, Valores)

datos %>% ggplot(aes(Conocimientos, Valores)) +   
 geom_bar(aes(fill = Porcentaje), position = "dodge", stat="identity", color = "black")+ 
 scale_fill_manual(values = c("palegreen3", "salmon", "skyblue")) +
 geom_text(aes(x = Conocimientos, y = Valores, label = round(Valores,0), group = Porcentaje),position = position_dodge(width = 0.9),
 vjust = -0.5, size = 4) + 
 labs(title="Conocimientos respecto al nivel requerido ", y = "Porcentaje del total") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"))


# Funciones 

n <- 55

Funciones <- c(rep("Programar aplicaciones",3),rep("Manejar bases de datos",3),
 rep("Asesorar a los usuarios",3),rep("Documentar los módulos",3), rep("Aplicar las normas",3))

#Funciones <- c(rep("Aplicaciones informáticas",3),rep("Pruebas del código",3),
 #rep("Documentar actividad",3),rep("Elaborar manual técnico",3), rep("Interpretar aplicaciones",3))

Porcentaje <- rep(c("por debajo del nivel requerido", "en el nivel requerido", "por encima del nivel requerido"),5)
Valores <- c(length(which(programadores$F1 <2))*100/n, length(which(programadores$F1 == 2))*100/n, length(which(programadores$F1 > 2))*100/n,
 length(which(programadores$F2 <2))*100/n, length(which(programadores$F2 == 2))*100/n, length(which(programadores$F2 > 2))*100/n,
 length(which(programadores$F3 <2))*100/n, length(which(programadores$F3 == 2))*100/n, length(which(programadores$F3 > 2))*100/n,
 length(which(programadores$F4 <2))*100/n, length(which(programadores$F4 == 2))*100/n, length(which(programadores$F4 > 2))*100/n,
 length(which(programadores$F5 <2))*100/n, length(which(programadores$F5 == 2))*100/n, length(which(programadores$F5 > 2))*100/n)

datos <- data.frame(Funciones, Porcentaje, Valores)

ggplot(datos, aes(Funciones, Valores)) +  
 geom_bar(aes(fill = Porcentaje), position = "dodge", stat="identity", color = "black")+ 
 scale_fill_manual(values = c("palegreen3", "salmon", "skyblue")) +
 geom_text(aes(x = Funciones, y = Valores, label = round(Valores,0), group = Porcentaje),position = position_dodge(width = 0.9),
 vjust = -0.5, size = 4) + 
 labs(title="Funciones respecto al nivel requerido ", y = "Porcentaje del total") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"))



# Módulos-Final 


n <- 55
nr <- c(56.25,65,50,57.125)
#nr <- c(57.5,65,50,57.5)


Modulos <- c(rep("Módulo de competencias",3),rep("Módulo de conocimientos",3), rep("Módulo de funciones",3), rep("PUNTUACIONES FINALES",3))
Porcentaje <- rep(c("por debajo del nivel requerido", "en el nivel requerido", "por encima del nivel requerido"),2)
Valores <- c(length(which(programadores$M1 <nr[1]))*100/n, length(which(programadores$M1 == nr[1]))*100/n, length(which(programadores$M1 > nr[1]))*100/n,
 length(which(programadores$M2 <nr[2]))*100/n, length(which(programadores$M2 == nr[2]))*100/n, length(which(programadores$M2 > nr[2]))*100/n,
 length(which(programadores$M3 <nr[3]))*100/n, length(which(programadores$M3 == nr[3]))*100/n, length(which(programadores$M3 > nr[3]))*100/n,
 length(which(programadores$FINAL <nr[4]))*100/n, length(which(programadores$FINAL == nr[4]))*100/n, length(which(programadores$FINAL > nr[4]))*100/n)

datos <- data.frame(Modulos, Porcentaje, Valores)

ggplot(datos, aes(Modulos, Valores)) +   
 geom_bar(aes(fill = Porcentaje), position = "dodge", stat="identity", color = "black")+ 
 scale_fill_manual(values = c("palegreen3", "salmon", "skyblue")) +
 geom_text(aes(x = Modulos, y = Valores, label = round(Valores,0), group = Porcentaje),position = position_dodge(width = 0.9),
 vjust = -0.5, size = 4) + 
 labs(title="Módulos y puntuaciones finales respecto al nivel requerido ", x = "Puntuaciones totales", y = "Porcentaje del total") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"))

# Módulos-Final con mas nivel requerido

Modulos <- c(rep("Módulo de competencias",3),rep("Módulo de conocimientos",3), rep("Módulo de funciones",3), rep("PUNTUACIONES FINALES",3))
Porcentaje <- rep(c("por debajo del nivel requerido modificado", "en el nivel requerido modificado", "por encima del nivel requerido modificado"),2)
Valores <- c(length(which(programadores$M1 <56.25))*100/55, length(which(programadores$M1 == 56.25))*100/55, length(which(programadores$M1 > 56.25))*100/55,
 length(which(programadores$M2 <65))*100/55, length(which(programadores$M2 == 65))*100/55, length(which(programadores$M2 > 65))*100/55,
 length(which(programadores$M3 <100))*100/55, length(which(programadores$M3 == 100))*100/55, length(which(programadores$M3 > 100))*100/55,
 length(which(programadores$FINAL <74.625))*100/55, length(which(programadores$FINAL == 74.625))*100/55, length(which(programadores$FINAL > 74.625))*100/55)
Valores.prueba <- c(length(which(programadores$M1 <75))*100/55, length(which(programadores$M1 == 75))*100/55, length(which(programadores$M1 > 75))*100/55,
 length(which(programadores$M2 <75))*100/55, length(which(programadores$M2 == 75))*100/55, length(which(programadores$M2 > 75))*100/55,
 length(which(programadores$M3 <100))*100/55, length(which(programadores$M3 == 100))*100/55, length(which(programadores$M3 > 100))*100/55,
 length(which(programadores$FINAL <83.75))*100/55, length(which(programadores$FINAL == 83.75))*100/55, length(which(programadores$FINAL > 83.75))*100/55)

datos <- data.frame(Modulos, Porcentaje, Valores, Valores.prueba)

ggplot(datos, aes(Modulos, Valores.prueba)) +   
 geom_bar(aes(fill = Porcentaje), position = "dodge", stat="identity", color = "black") + 
 geom_text(aes(x = Modulos, y = Valores.prueba, label = round(Valores.prueba,0), group = Porcentaje),position = position_dodge(width = 0.9),
 vjust = -0.5, size = 4) + 
 geom_bar(aes(Modulos, Valores, group = Porcentaje, color = "Nivel requerido"), position = "dodge", stat="identity", fill = "grey", alpha = 0.5) +
 scale_colour_manual("", values="grey") +
 labs(title="Módulos y puntuaciones finales respecto al nivel requerido modificado", x = "Puntuaciones totales", y = "Porcentaje del total") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.box = "vertical",
 legend.text = element_text(size=13), axis.text = element_text(size=13), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"))




