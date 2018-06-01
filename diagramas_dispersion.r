### Diagramas de dispersión

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de dispersion de variables continuas y colores por factores
# Para M1 vs M3

Competencias <- c(programadores$M1)
Módulo <- c( rep("Funciones",55))
Puntuaciones <- c(programadores$M3)

datos <- data.frame(Competencias, Puntuaciones, Módulo)

datos %>% ggplot(aes(Competencias, Puntuaciones)) + geom_point(aes(colour = Módulo), show.legend = TRUE) + 
 geom_smooth(aes(colour = Módulo), size = 0.5, method = "lm", se = FALSE, show.legend = TRUE) + 
 labs(title="Competencias vs. Desempeño ", x = "Puntuaciones del módulo de competencias", y = "Puntuaciones del módulo de funciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=20), legend.position="right")

# Para M2 vs M1-M3

Conocimientos <- c(programadores$M2, programadores$M2)
Módulo <- c(rep("Competencias",55), rep("Funciones",55))
Puntuaciones <- c(programadores$M1, programadores$M3)

datos <- data.frame(Conocimientos, Puntuaciones, Módulo)

datos %>% ggplot(aes(Conocimientos, Puntuaciones)) + geom_point(aes(colour = Módulo), show.legend = TRUE) + 
 geom_smooth(aes(colour = Módulo), size = 0.5, method = "lm", se = FALSE, show.legend = TRUE) + 
 labs(title="Conocimientos vs. Competencias y Desempeño", x = "Puntuaciones del módulo de conocimientos", y = "Puntuaciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=20), legend.position="right")



 
 
 

