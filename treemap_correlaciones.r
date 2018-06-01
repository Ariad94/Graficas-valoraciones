### Gr�ficos de proporciones y correlaciones

library(ggplot2)
library(dplyr)
library(ggthemes)
library(treemapify)
library(ggcorrplot)

### ### Diagrama de arbol de competencias, conocimiento y funciones

Nombres <- c("Calidad y mejora continua","Pensamiento anal�tico","Capacidad de aprendizaje",
 "Soluci�n de problemas","Competencia digital","Lenguajes de programaci�n","Herramientas de desarrollo (id)",
 "Programar aplicaciones","Manejar bases de datos","Asesorar a los usuarios",
 "Documentar los m�dulos","Aplicar las normas")
Medias <- c(mean(programadores$C1),mean(programadores$C2),mean(programadores$C3),
 mean(programadores$C4),mean(programadores$C5),mean(programadores$CN1),mean(programadores$CN2),
 mean(programadores$F1),mean(programadores$F2),mean(programadores$F3), 
 mean(programadores$F4),mean(programadores$F5))
Grupos <- c(rep("COMPETENCIAS", 5), rep("CONOCIMIENTOS",2), rep("DESEMPE�O",5))

datos <- data.frame(Grupos, Nombres, Medias)

datos %>% ggplot(aes(area= Medias, fill = Medias, label = Nombres, subgroup = Grupos)) + geom_treemap() +
 geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
 facet_grid(Grupos~.) + labs(title="Puntuaciones medias de las competencias, conocimientos y funciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0, face="bold"),
 strip.text.y = element_text(color = "dodgerblue4", size=15))


### ### Diagrama de arbol de competencias, conocimiento y funciones seg�n ponderaciones teoricas

Nombres <- c("Calidad y mejora continua","Pensamiento anal�tico","Capacidad de aprendizaje",
 "Soluci�n de problemas","Competencia digital","Lenguajes de programaci�n","Herramientas de desarrollo (id)",
 "Programar aplicaciones","Manejar bases de datos","Asesorar a los usuarios",
 "Documentar los m�dulos","Aplicar las normas")
Ponderaciones <- c(0.15, 0.2, 0.15, 0.25, 0.25, 0.6, 0.4, 0.3, 0.25, 0.15, 0.2, 0.1)
Grupos <- c(rep("COMPETENCIAS", 5), rep("CONOCIMIENTOS",2), rep("DESEMPE�O",5))

datos <- data.frame(Grupos, Nombres, Ponderaciones)

datos %>% ggplot(aes(area= Ponderaciones, fill = Ponderaciones, label = Nombres, subgroup = Grupos)) + geom_treemap() +
 geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
 facet_grid(Grupos~.) + labs(title="Importancia de las competencias, conocimientos y funciones en el perfil") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0, face="bold"),
 strip.text.y = element_text(color = "dodgerblue4", size=15))


### ### Gr�fica de correlaciones 
# Competencias, conocimientos, funciones

datos  <- data.frame(programadores$C1, programadores$C2, programadores$C3, programadores$C4, programadores$C5,
 programadores$CN1, programadores$CN2, programadores$F1, programadores$F2, programadores$F3, programadores$F4, programadores$F5)

colnames(datos) <- c("COMPETENCIA \nCalidad y mejora continua","COMPETENCIA \nPensamiento anal�tico","COMPETENCIA \nCapacidad de aprendizaje",
 "COMPETENCIA \nSoluci�n de problemas","COMPETENCIA \nCompetencia digital","CONOCIMIENTO \nLenguajes de programaci�n","CONOCIMIENTO \nHerramientas de desarrollo (id)",
 "FUNCI�N \nProgramar aplicaciones","FUNCI�N \nManejar bases de datos","FUNCI�N \nAsesorar a los usuarios",
 "FUNCI�N \nDocumentar los m�dulos","FUNCI�N \nAplicar las normas")

coeficientes <- round(cor(datos, method = "spearman"), 1)

ggcorrplot(coeficientes, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", 
 colors = c("palegreen3", "white", "dodgerblue4")) +
 labs(title="Correlaciones", subtitle = "Coeficientes de correlaci�n de Spearman", x = "", y = "")+ 
 theme_minimal() +
 theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle= element_text(color = "dodgerblue4", size=20), legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1))




