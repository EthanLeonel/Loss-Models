
'-------------------------- Tarea 4 ----------------------------'

library('XLConnect')
library('skimr')
library('fitdistrplus')
library('ggplot2')
library('actuar')
library('goftest')

# Datos 
setwd('/Users/leogame/Documents/Séptimo Semestre/Modelos de Pérdidas/Tareas/Tarea 4')
Data <- readWorksheetFromFile('Tarea_4.xlsx', sheet = 'IF')
# Estadística Descriptiva 
a <- skim_with(numeric = sfl(min,max))
# Resumen de los datos
a(Data)

x <-Data$Incendio_Forestal_log

# Creamos histograma 
theme_set(theme_minimal())

Histograma <- ggplot(Data,aes(x = Incendio_Forestal_log)) + 
  geom_histogram(binwidth = 1.1, color = 'black', 
                 fill =rgb(1,0,0,0.5)) + xlim(-5.556887,5.991426)+
  labs(title="Histograma de Siniestros Logarítmicos",x="Monto",
       y="Frecuencia" ) + theme(plot.background = element_rect(fill= '#FFF6F4'))

Histograma

Distribucion <- ggplot(Data,aes(x = Incendio_Forestal_log, 
  colour = 'Función de Distribución Empírica')) + 
  stat_ecdf(geom = 'step',size=1.5, col='black' ) + 
  labs(title = 'Distribución de Pérdidas por Incendios forestales', 
       x= 'Montos', y = 'Probabilidad Acumulada' ,colour = '') + 
  xlim(-5.556887,5.991426) +theme(plot.background = element_rect(fill= '#FFF6F4'))

Distribucion

# Hacemos un ajuste con la distribución normal. 
Ajuste_1 <- fitdist(x,'norm',method = 'mle')
Ajuste_1
summary(Ajuste_1)

denscomp(Ajuste_1,plotstyle = 'ggplot') 
cdfcomp(Ajuste_1, plotstyle = 'ggplot') 
qqcomp(Ajuste_1, plotstyle = 'ggplot') 
ppcomp(Ajuste_1, plotstyle = 'ggplot') 

# Realizamos la prueba de hipótesis.
p_norm_ks <-  ks.test(x,"pnorm",Ajuste_1$estimate[1],Ajuste_1$estimate[2])
p_norm_ad <-  ad.test(x,"pnorm",Ajuste_1$estimate[1],Ajuste_1$estimate[2])

# No rechazamos la Hipótesis nula sin embargo el p-value no es lo
# suficientemente grande pero analizando ppplot y qqplot no es raro asumir
# que los datos siguen una distribución normal. 

# Hacemos un ajuste con la distribución t-student. 
Ajuste_2 <- fitdist(x,'t',method = 'mle',start = list(df=100))
Ajuste_2
summary(Ajuste_2)

denscomp(Ajuste_2,plotstyle = 'ggplot') 
cdfcomp(Ajuste_2, plotstyle = 'ggplot') 
qqcomp(Ajuste_2, plotstyle = 'ggplot') 
ppcomp(Ajuste_2, plotstyle = 'ggplot') 

# Realizamos la prueba de hipótesis.
(hip1 <- ks.test(x,"pt",Ajuste_2$estimate[1]))
(hip2 <- ad.test(x,"pt",Ajuste_2$estimate[1]))

# Rechazamos la Hipótesis nula pues el p-value es lo
# suficientemente pequeño para concluir que los montos no se distribuyan
# como una t de student.

# Hacemos un ajuste con la distribución gumbel. 
Ajuste_3 <- fitdist(x,'gumbel',start = list(alpha=10,scale=10))
Ajuste_3
summary(Ajuste_3)

denscomp(Ajuste_3,plotstyle = 'ggplot') 
cdfcomp(Ajuste_3,plotstyle = 'ggplot') 
qqcomp(Ajuste_3,plotstyle = 'ggplot') 
ppcomp(Ajuste_3,plotstyle = 'ggplot') 

# Realizamos la prueba de hipótesis.
(hip1 <- ks.test(x,"pgumbel",Ajuste_3$estimate[1],Ajuste_3$estimate[2]))
(hip2 <- ad.test(x,"pgumbel",Ajuste_3$estimate[1],Ajuste_3$estimate[2]))

# Rechazamos la Hipótesis nula pues el p-value es lo
# suficientemente pequeño para concluir que los montos no se distribuyan
# como una distribución gumbel.


# Hacemos un ajuste con la distribución logística. 
Ajuste_4 <- fitdist(x,'logis',start = list(location=0,scale=1))
Ajuste_4
summary(Ajuste_4)
denscomp(Ajuste_4,plotstyle = 'ggplot') 
cdfcomp(Ajuste_4,plotstyle = 'ggplot') 
qqcomp(Ajuste_4, plotstyle = 'ggplot') 
ppcomp(Ajuste_4, plotstyle = 'ggplot') 

# Realizamos la prueba de hipótesis. 
p_logis_ks <-  ks.test(x,"plogis",Ajuste_4$estimate[1],Ajuste_4$estimate[2])
p_logis_ad <- ad.test(x,"plogis",Ajuste_4$estimate[1],Ajuste_4$estimate[2])

# No rechazamos la Hipótesis nula sin embargo el p-value no es lo
# suficientemente grande, analizando qqplot y ppplot estos tienden a parecer
# una línea recta sin embargo no se comporta o se ajusta tan bien como 
# una distribución normal pero no podemos rechazar que los datos se dsitribuyan
# como una distribución logística. 


# Hacemos un ajuste con la distribución Burr. 
Ajuste_5 <- fitdist(x,'burr',start = list(shape1=0.3,shape2=1,rate = 1),
                    method = 'mse') 
Ajuste_5
summary(Ajuste_5)
denscomp(Ajuste_5, plotstyle = 'ggplot') 
cdfcomp(Ajuste_5, plotstyle = 'ggplot') 
qqcomp(Ajuste_5, plotstyle = 'ggplot') 
ppcomp(Ajuste_5, plotstyle = 'ggplot') 

# Realizamos la prueba de hipótesis. 
(hip1 <- ks.test(x,"pburr",Ajuste_5$estimate[1],Ajuste_5$estimate[2],
                 Ajuste_5$estimate[3]))
(hip2 <- ad.test(x,"pburr",Ajuste_5$estimate[1],Ajuste_5$estimate[2],
                 Ajuste_5$estimate[3]))
# Rechazamos la hipótesis nula pues el p-value es muy pequeño entonces 
# concluimos que los datos no se distribuyen como una distribución Burr. 

# Hacemos las comparaciones entre las distribuciones que tengan el p-value más grande
# y mejor se hayan ajustado. 
denscomp(list(Ajuste_1,Ajuste_4),datacol = rgb(1,.45,.4,0.5), plotstyle = 'ggplot')
cdfcomp(list(Ajuste_1,Ajuste_4),datacol = rgb(1,.45,.4,0.5), plotstyle = 'ggplot')
qqcomp(list(Ajuste_1,Ajuste_4), plotstyle = 'ggplot')
ppcomp(list(Ajuste_1,Ajuste_4), plotstyle = 'ggplot')

# como podemos ver la distribución normal parece ajustarse un poco mejor a los
# datos. 

# Hacemos una tabla de los p-values de las distribuciones que más se ajustan a los
# datos, esto con el fin de compararlos. 

P_Values <-data.frame(c(.1894,.1154),c(.2804,.09037))
colnames(P_Values) <- c('Normal','Logística')
rownames(P_Values) <- c('K-S','AD')

# Como podemos notar ambos p-values son suficientes para no rechazar la hipótesis
# nula sin embargo queremos concluir cual es mejor. 

# Se debe de tomar el mínimo de todos los valores en el Akaike's Information Criterion
# y en el Akaike's Information Criterion 

gofstat(list(Ajuste_1,Ajuste_2,Ajuste_3,Ajuste_4,Ajuste_5),
        fitnames = c("Normal","T-Student",'Gumbel',"Logística",'Burr'))

# Notamos que el mínimo en ambos criterios lo tiene la normal entonces este 
# criterio nos ayuda a decidir cual distribución se ajusta mejor entonces
# concluimos que la distribución normal es la que mejor se ajusta a los datos. 
