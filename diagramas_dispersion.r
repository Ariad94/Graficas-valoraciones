### Diagramas de dispersi�n

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de dispersion de variables continuas y colores por factores
# Para M1-M2-M3

Competencias <- c(programadores$M1, programadores$M1)
M�dulo <- c(rep("Conocimientos",55), rep("Funciones",55))
Puntuaciones <- c(programadores$M2, programadores$M3)

datos <- data.frame(Competencias, Puntuaciones, M�dulo)

datos %>% ggplot(aes(Competencias, Puntuaciones)) + geom_point(aes(colour = M�dulo), show.legend = TRUE) + 
 geom_smooth(aes(colour = M�dulo), size = 0.5, method = "lm", se = FALSE, show.legend = TRUE) + 
 labs(title="Competencias vs. Conocimientos y Funciones", x = "Puntuaciones del M�dulo de Competencias", y = "Puntuaciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=12, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=10), legend.position="right")

 
 
 

