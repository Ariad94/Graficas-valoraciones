### Gráficos de proporciones y correlaciones

library(ggplot2)
library(dplyr)
library(ggthemes)
library(treemapify)

### ### Diagrama de arbol de competencias, conocimiento y funciones

Nombres <- c("Calidad y mejora continua","Pensamiento analítico","Capacidad de aprendizaje",
 "Solución de problemas","Competencia digital","Lenguajes de programación","Herramientas de desarrollo (id)",
 "Programar aplicaciones","Manejar bases de datos","Asesorar a los usuarios",
 "Documentar los módulos","Aplicar las normas")
Medias <- c(mean(programadores$C1),mean(programadores$C2),mean(programadores$C3),
 mean(programadores$C4),mean(programadores$C5),mean(programadores$CN1),mean(programadores$CN2),
 mean(programadores$F1),mean(programadores$F2),mean(programadores$F3), 
 mean(programadores$F4),mean(programadores$F5))
Grupos <- c(rep("COMPETENCIAS", 5), rep("CONOCIMIENTOS",2), rep("DESEMPEÑO",5))

datos <- data.frame(Grupos, Nombres, Medias)

datos %>% ggplot(aes(area= Medias, fill = Medias, label = Nombres, subgroup = Grupos)) + geom_treemap() +
 geom_treemap_text(colour = "white", place = "centre", grow = FALSE) +
 facet_grid(Grupos~.) + labs(title="Puntuaciones medias de las competencias, conocimientos y funciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0, face="bold"),
 strip.text.y = element_text(color = "dodgerblue4", size=15))



