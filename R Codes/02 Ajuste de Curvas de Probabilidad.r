
#Ajuste de curvas de probabilidad

library(MASS) #Distribuciones de probabilidad
library(actuar) #Distribuciones adicionales de probabilidad
library(fitdistrplus) #Ajuste de curvas de probabilidad
library(goftest) #Probas de bondad de ajuste
# listado de distribuciones de probabilidad: 
# https://cran.r-project.org/web/views/Distributions.html

# Carga de datos 
#muestra aleatoria de una densidad Weibull con parametros (1,2000)
datos<- rweibull(1000,1,2000)

# 1. Análisis descriptivo de los datos 
summary(datos)
hist(datos,freq = FALSE)
plot(ecdf(datos))

# 2. Ajuste de Curvas de Probabilidad
help("fitdistrplus-package")

mod1<-fitdist(datos,"weibull",method = "mle")
mod1
summary(mod1)

par(mfrow=c(2,2))
denscomp(mod1) #Funci?n de Densidad
cdfcomp(mod1) #Funci?n de Distribuci?n
qqcomp(mod1) #Cuantiles Te?ricos vs Emp?ricos
ppcomp(mod1) #Probabilidades Te?ricas vs Emp?ricas

# 3. Validaci?n con pruebas de bondad de ajuste 
(hip1 <- ks.test(datos,"pweibull",mod1$estimate[1],mod1$estimate[2]))
(hip2 <- ad.test(datos,"pweibull",mod1$estimate[1],mod1$estimate[2]))


# 4. Comparativo de ajustes de curvas de probabilidad

mod2<-fitdist(datos,"weibull",method = "mle")
mod3 <- fitdist(datos,"pareto",method = "mse",
                start = list(shape=1,scale=300))
mod4<-fitdist(datos,"lnorm",method = "mle")

leyenda<-c("Weibull","Pareto","Lognormal")
denscomp(list(mod2,mod3,mod4),legendtext = leyenda)
cdfcomp(list(mod2,mod3,mod4),legendtext = leyenda) 
qqcomp(list(mod2,mod3,mod4),legendtext = leyenda) 
ppcomp(list(mod2,mod3,mod4),legendtext = leyenda) 

gofstat(list(mod2,mod3,mod4),
        fitnames = c("Weibull","Pareto","Lognormal"))


 



