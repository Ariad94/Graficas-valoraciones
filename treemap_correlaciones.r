### Gráficos de proporciones y correlaciones

library(ggplot2)
library(dplyr)
library(ggthemes)
library(treemapify)
library(ggcorrplot)
library(corrplot)

### ### Diagrama de arbol de competencias, conocimiento y funciones

Nombres <- c("Calidad y mejora continua","Pensamiento analítico","Capacidad de aprendizaje",
 "Solución de problemas","Competencia digital","Lenguajes de programación","Herramientas de desarrollo (id)",
 "Programar aplicaciones","Manejar bases de datos","Asesorar a los usuarios",
 "Documentar los módulos","Aplicar las normas")

#Nombres <- c("Calidad y mejora continua","Pensamiento analítico","Capacidad de aprendizaje",
 #"Solución de problemas","Competencia digital","Lenguajes de programación","Herramientas de desarrollo (id)",
 #"Aplicaciones informáticas","Pruebas del código","Documentar actividad",
 #"Elaborar manual técnico","Interpretar aplicaciones")

Medias <- c(mean(programadores$C1),mean(programadores$C2),mean(programadores$C3),
 mean(programadores$C4),mean(programadores$C5),mean(programadores$CN1),mean(programadores$CN2),
 mean(programadores$F1),mean(programadores$F2),mean(programadores$F3), 
 mean(programadores$F4),mean(programadores$F5))
Grupos <- c(rep("COMPETENCIAS", 5), rep("CONOCIMIENTOS",2), rep("DESEMPEÑO",5))

datos <- data.frame(Grupos, Nombres, Medias)

datos %>% ggplot(aes(area= Medias, fill = Medias, label = Nombres, subgroup = Grupos)) + geom_treemap() +
 geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
 scale_fill_gradient(low = "palegreen3", high = "royalblue2") +
 facet_grid(Grupos~.) + labs(title="Puntuaciones medias de las competencias, conocimientos y funciones", fill = "Medias\n") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=20, hjust=0, face="bold"),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13))



### ### Diagrama de arbol de competencias, conocimiento y funciones según ponderaciones teoricas

Nombres <- c("Calidad y mejora continua","Pensamiento analítico","Capacidad de aprendizaje",
 "Solución de problemas","Competencia digital","Lenguajes de programación","Herramientas de desarrollo (id)",
 "Programar aplicaciones","Manejar bases de datos","Asesorar a los usuarios",
 "Documentar los módulos","Aplicar las normas")

#Nombres <- c("Calidad y mejora continua","Pensamiento analítico","Capacidad de aprendizaje",
 #"Solución de problemas","Competencia digital","Lenguajes de programación","Herramientas de desarrollo (id)",
 #"Aplicaciones informáticas","Pruebas del código","Documentar actividad",
 #"Elaborar manual técnico","Interpretar aplicaciones")

Ponderaciones <- c(0.15, 0.2, 0.15, 0.25, 0.25, 0.6, 0.4, 0.3, 0.25, 0.15, 0.2, 0.1)

#Ponderaciones <- c(0.15, 0.2, 0.15, 0.2, 0.3, 0.6, 0.4, 0.35, 0.2, 0.2, 0.1, 0.15)

Grupos <- c(rep("COMPETENCIAS", 5), rep("CONOCIMIENTOS",2), rep("DESEMPEÑO",5))

datos <- data.frame(Grupos, Nombres, Ponderaciones)

datos %>% ggplot(aes(area= Ponderaciones, fill = Ponderaciones, label = Nombres, subgroup = Grupos)) + geom_treemap() +
 geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
 scale_fill_gradient(low = "palegreen3", high = "royalblue2") +
 facet_grid(Grupos~.) + labs(title="Importancia de las competencias, conocimientos y funciones en el perfil", fill = "Ponderaciones \n") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0, face="bold"),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13))


### ### Gráfica de correlaciones 
# Competencias, conocimientos, funciones

datos  <- data.frame(programadores$C1, programadores$C2, programadores$C3, programadores$C4, programadores$C5,
 programadores$CN1, programadores$CN2, programadores$F1, programadores$F2, programadores$F3, programadores$F4, programadores$F5)

colnames(datos) <- c("COMPETENCIA \nCalidad y mejora continua","COMPETENCIA \nPensamiento analítico","COMPETENCIA \nCapacidad de aprendizaje",
 "COMPETENCIA \nSolución de problemas","COMPETENCIA \nCompetencia digital","CONOCIMIENTO \nLenguajes de programación","CONOCIMIENTO \nHerramientas de desarrollo (id)",
 "FUNCIÓN \nProgramar aplicaciones","FUNCIÓN \nManejar bases de datos","FUNCIÓN \nAsesorar a los usuarios",
 "FUNCIÓN \nDocumentar los módulos","FUNCIÓN \nAplicar las normas")

#colnames(datos) <- c("COMPETENCIA \nCalidad y mejora continua","COMPETENCIA \nPensamiento analítico","COMPETENCIA \nCapacidad de aprendizaje",
 #"COMPETENCIA \nSolución de problemas","COMPETENCIA \nCompetencia digital","CONOCIMIENTO \nLenguajes de programación","CONOCIMIENTO \nHerramientas de desarrollo (id)",
 #"FUNCIÓN \nAplicaciones informáticas","FUNCIÓN \nPruebas del código","FUNCIÓN \nDocumentar actividad",
 #"FUNCIÓN \nElaborar manual técnico","FUNCIÓN \nInterpretar aplicaciones")

coeficientes <- round(cor(datos, method = "spearman"), 1)

ggcorrplot(coeficientes, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", 
 colors = c("darkgreen", "white", "dodgerblue4"), legend.title = "Coeficientes \nde correlación\n") +
 labs(title="Correlación entre competencias, conocimientos y funciones", subtitle = "Coeficientes de correlación de Spearman", x = "", y = "", legend = "Coeficientes \nde correlación\n")+ 
 theme_minimal() +
 theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), 
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=10))

# Módulos

datos  <- data.frame(programadores$M1, programadores$M2, programadores$M3)

colnames(datos) <- c("Módulo de competencias","Módulo de conocimientos","Módulo de desempeño")

coeficientes <- round(cor(datos, method = "spearman"), 1)

ggcorrplot(coeficientes, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", 
 colors = c("darkgreen", "white", "dodgerblue4"), legend.title = "Coeficientes \nde correlación\n") +
 labs(title="Correlación entre módulos", subtitle = "Coeficientes de correlación de Spearman", x = "", y = "", legend = "Coeficientes \nde correlación\n")+ 
 theme_minimal() +
 theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), 
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=10))

