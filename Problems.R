#Ana Laura CHenoweth Galaz
#anios (var independiente) x
anio<-c(1995,1996,1997,1998,1999,2000,2001,2002)
#Consumos anuales productos derivados del pollo (var dependiente) y
cons<-c(25.9,26.8,27.3,27.8,29.6,30.5,30.8,32.6)

#Diagrama de dispersion 
length(anio)
length(cons)
plot(anio,cons,col="blue", pch=16, cex=2)
grid()

df <- data.frame(anio,cons)

library(ggplot2)
ggplot(df,aes(x=anio, y=cons))+
  geom_point(color="blue", size=4)

pearson <- cor(anio,cons)
pearson

modelo <- lm(cons ~ anio)
summary(modelo)

# Obtener los coeficientes
coeficientes <- coef(modelo)

# Imprimir la ecuación de la regresión
cat("La ecuación de la regresión estimada es:\n")
cat(sprintf("y = %.2f + %.2f * x\n", coeficientes[1], coeficientes[2]))

ggplot(df, aes(x = anio, y = cons)) +
  geom_point(color = "blue", size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

#nivel de alcohol en la sangre (%) (var independiente) x
alc<-c(0.08,0.10,0.12,0.14,0.15,0.16,0.18)
#tiempo de reacción (segundos)
seg<-c(0.32,0.38,0.44,0.42,0.47,0.51,0.63)

#Diagrama de dispersion 
length(alc)
length(seg)
plot(alc,seg,col="blue", pch=16, cex=2)
grid()

df <- data.frame(alc,seg)

library(ggplot2)
ggplot(df,aes(x=alc, y=seg))+
  geom_point(color="blue", size=4)

pearson2 <- cor(alc,seg)
pearson2

modelo <- lm(seg ~ alc)
summary(modelo)

# Obtener los coeficientes
coeficientes <- coef(modelo)

# Imprimir la ecuación de la regresión
cat("La ecuación de la regresión estimada es:\n")
cat(sprintf("y = %.2f + %.2f * x\n", coeficientes[1], coeficientes[2]))

ggplot(df, aes(x = alc, y = seg)) +
  geom_point(color = "blue", size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

#peso (var indp.)
peso<-c(3.262,3.022,3.062,2.879,3.626,3.957,4.170,2.224,3.125,3.408,2.690,3.119,3.525,2.245,2.417,2.631,2.647,3.087,2.005,3.694)
#talla (var dep)
talla<-c(52,50,51,51,52,54,54,47,50,52,48,50,51,49,48,50,50,50,46,52)

#Realizar un diagrama de dispersión e indique el tipo de relación que sugiere 
# el patron observado en dicho diagrama. 
length(peso)
length(talla)
plot(peso,talla,col="blue", pch=16, cex=2)
grid()

df <- data.frame(peso,talla)

library(ggplot2)
ggplot(df,aes(x=peso, y=talla))+
  geom_point(color="blue", size=4)

pearson3 <- cor(peso,talla)
pearson3

modelo <- lm(talla ~ peso)
summary(modelo)

# Obtener los coeficientes
coeficientes <- coef(modelo)

# Imprimir la ecuación de la regresión
cat("La ecuación de la regresión estimada es:\n")
cat(sprintf("y = %.2f + %.2f * x\n", coeficientes[1], coeficientes[2]))

ggplot(df, aes(x = peso, y = talla)) +
  geom_point(color = "blue", size = 4) +
  geom_smooth(method = "lm", se = FALSE, color = "red")