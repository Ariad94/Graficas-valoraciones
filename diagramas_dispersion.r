### Diagramas de dispersi�n

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de dispersion de variables continuas y colores por factores
# Para M1 vs M3

Competencias <- c(programadores$M1)
M�dulo <- c( rep("Funciones",55))
Puntuaciones <- c(programadores$M3)

datos <- data.frame(Competencias, Puntuaciones, M�dulo)

datos %>% ggplot(aes(Competencias, Puntuaciones)) + geom_point(aes(colour = M�dulo), show.legend = TRUE) + 
 geom_smooth(aes(colour = M�dulo), size = 0.5, method = "lm", se = FALSE, show.legend = TRUE) + 
 labs(title="Competencias vs. Desempe�o ", x = "Puntuaciones del m�dulo de competencias", y = "Puntuaciones del m�dulo de funciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=20), legend.position="right")

# Para M2 vs M1-M3

Conocimientos <- c(programadores$M2, programadores$M2)
M�dulo <- c(rep("Competencias",55), rep("Funciones",55))
Puntuaciones <- c(programadores$M1, programadores$M3)

datos <- data.frame(Conocimientos, Puntuaciones, M�dulo)

datos %>% ggplot(aes(Conocimientos, Puntuaciones)) + geom_point(aes(colour = M�dulo), show.legend = TRUE) + 
 geom_smooth(aes(colour = M�dulo), size = 0.5, method = "lm", se = FALSE, show.legend = TRUE) + 
 labs(title="Conocimientos vs. Competencias y Desempe�o", x = "Puntuaciones del m�dulo de conocimientos", y = "Puntuaciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=20), legend.position="right")



 
 
 

