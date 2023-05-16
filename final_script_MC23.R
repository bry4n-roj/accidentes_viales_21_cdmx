### analisis 2021 --- cdmx
## AUTOR: Bryan Rojas
##date: 11/05/23

#revisar supuestos y comparar reg 4, reg5, REG 8, 10 --- explicas cosas dictintas--- no interesante
#11 y 12, tienen un mejor ajuste

#importamos librerias a utilizar
library(tidyverse)
library(janitor)
library(car)
library(MASS)
library(lmtest)
library(broom)
library(stargazer)
library(ggfortify)
library(margins)
library(ggpubr)
library(writexl)
options(scipen = 999)

#importamos db a usar
db_21cdmx <- read_csv("atus_21_cdmx.csv")
#limpiamos nombres
db_21cdmx <- clean_names(db_21cdmx)
glimpse(db_21cdmx)

#creamos nuevas variables de interes
db_21cdmx <- db_21cdmx %>%
  mutate(suma_autos = automovil + campasaj + microbus + pascamion + omnibus + tranvia + camioneta + camion + tractor + ferrocarri + motociclet + bicicleta + otrovehic,
         suma_fallecidos = condmuerto + pasamuerto + peatmuerto + ciclmuerto + otromuerto + nemuerto,
         suma_heridos = condherido + pasaherido + peatherido + ciclherido + otroherido + neherido)

db_21cdmx <- db_21cdmx %>% 
  mutate(suma_victimas = suma_fallecidos + suma_heridos)

atus_cdmx <- db_21cdmx %>% 
  filter(id_edad != 0 & id_edad < 99)

atus_cdmx <- atus_cdmx %>% 
  mutate(tasa_mortalidad = (suma_fallecidos / suma_victimas)*100, na.rm = TRUE)

atus_cdmx$tasa_mortalidad <- replace_na(atus_cdmx$tasa_mortalidad, 0)

atus_prueba <- atus_cdmx %>% 
  filter(aliento != "Se ignora", cinturon != "Se ignora")

atus_plot <- atus_cdmx %>% 
  dplyr ::select(id_municipio,id_hora, id_edad, suma_autos, suma_fallecidos, suma_heridos, suma_victimas, porcentaje_mortalidad)

plot(atus_plot)

#modelos lineales simples
#reg lineal 1 - mortalidad - completa
reg1 <- lm(tasa_mortalidad ~ id_edad, data = atus_cdmx)
summary(reg1)

#reg lineal 2 - mortalidad - completa
reg2 <- lm(tasa_mortalidad ~ id_edad, data = atus_prueba)
summary(reg2)

#reg 3 victimas
reg3 <- lm(suma_victimas ~ id_edad, data = atus_cdmx)
summary(reg3)

reg4 <- lm(suma_victimas ~ id_edad, data = atus_prueba) # ojo aqui!!
summary(reg4)

#reg5 heridos
reg5 <- lm(suma_heridos ~ id_edad, data = atus_prueba) # ojo aqui
summary(reg5)

#reg6 fallecidos ---CHAFA
reg6 <- lm(suma_fallecidos ~ id_edad, data = atus_prueba) 
summary(reg6)

#reg con id_hora como explicativa CHAFA
reg7 <- lm(tasa_mortalidad ~ id_hora, data = atus_prueba) 
summary(reg7)

#reg8 - prueba
reg8 <- lm(tasa_mortalidad ~ suma_autos , data = atus_prueba) # ojo aqui
summary(reg8)

#reg id_hora
reg9 <- lm(suma_victimas ~ id_hora, data = atus_prueba) 
summary(reg9)

reg10 <- lm(suma_victimas ~ suma_autos, data = atus_prueba) # ojo aqui!!
summary(reg10)

#modelos simples con correcion de variables log
reg11 <- lm(tasa_mortalidad ~ log(suma_autos), data = atus_prueba) # ojo aqui
summary(reg11)

reg12 <- lm(tasa_mortalidad ~ I(suma_autos ^2), data = atus_prueba) # ojo aqui
summary(reg12)

#reg lineales, con correcion de variables originales de interes CHAFA!! NO PONER
reg13_1 <- lm(tasa_mortalidad ~ I(id_edad ^2), data = atus_prueba)
summary(reg13_1)
reg14 <- lm(tasa_mortalidad ~ log(id_edad), data = atus_prueba)
summary(reg14)
reg13 <- lm(tasa_mortalidad ~ I(id_hora ^2), data = atus_prueba)
summary(reg13)

##probando supuestos visualmente reg 8, reg 11 y reg 12
autoplot(reg8)
autoplot(reg11)
autoplot(reg12)

#Seccion regresion multiple----
#reg multiple todas las variables---
reg15 <- lm(tasa_mortalidad ~ suma_autos + caparod + causaacci + aliento + cinturon + sexo, data = atus_prueba)
summary(reg15)

#reg multiple con solo variables de interes
reg16 <- lm(tasa_mortalidad ~ suma_autos + aliento + cinturon, data = atus_prueba)
summary(reg16)

#reg multiple con solo variables significativas + sexo
reg17 <- lm(tasa_mortalidad ~ suma_autos + aliento + cinturon + sexo, data = atus_prueba)
summary(reg17)

#convertimos edad en categorias OMS
summary(atus_prueba$id_edad)
atus_prueba <- atus_prueba %>% 
  mutate(rango_edad = cut(id_edad, breaks = c(0,26,59,84),
                           labels = c("jovenes","adultos", "adultos mayores")))

summary(atus_prueba$id_hora)
atus_prueba <- atus_prueba %>% 
  mutate(rango_horario = cut(id_edad, breaks = c(0,6,12,18,24),
                          labels = c("madrugada","mañana", "tarde","noche")))

#reg18 con edad como categoria
reg18 <- lm(tasa_mortalidad ~ suma_autos + aliento + cinturon + rango_edad, data = atus_prueba)
summary(reg18)

## elegir modelo--- criterios aic / bic - SIMPLES
AIC(reg8, reg11, reg12)
BIC(reg8, reg11, reg12)

AIC(reg15, reg16, reg17, reg18)
BIC(reg15, reg16, reg17, reg18)
#me quedo con reg 12, tiene un ligero mejor ajuste
#me quedo con reg 18, tiene un mejor ajuste multiple

#probar supuestos
autoplot(reg12)
resettest(reg12) # supuesto linealidad : NO ES LINEAL

shapiro.test(resid(reg12)) #NORMALIDAD no cumple
plot(reg12, 2)

bptest(reg12) #hay homocedasticidad
plot(reg12, 3)

dwtest(reg12) # hay autocorrelacion suma autos y tasa mortalidad, por lo tanto reahacer modelos multiples con id edad o id hora
plot(reg12, 5)

#supuestos en reg multiple----
autoplot(reg18)
resettest(reg18) # supuesto linealidad : Si pasa prueba

shapiro.test(resid(reg18)) #NORMALIDAD si cumple con normalidad
plot(reg18, 2)

bptest(reg18) #HAhay homocedasticidad
plot(reg18, 3)

dwtest(reg18) # hay autocorrelacion suma autos y tasa mortalidad, por lo tanto reahacer modelos multiples con id edad o id hora
plot(reg18, 5)

vif(reg18) #no problema con multicolinealidad

## modelos reg multiple quitando cuma autos por alta autocorrelacion---- EDAD
reg20 <- lm(tasa_mortalidad ~ id_edad + caparod + causaacci + aliento + cinturon + sexo, data = atus_prueba)
summary(reg20)

#reg multiple con solo variables significativas
reg21 <- lm(tasa_mortalidad ~ id_edad + aliento + cinturon, data = atus_prueba)
summary(reg21)

#reg multiple con solo variables significativas + sexo
reg22 <- lm(tasa_mortalidad ~ id_edad + aliento + cinturon + sexo, data = atus_prueba)
summary(reg22)

AIC(reg20, reg21, reg22) # me quedo con reg22

## modelos reg multiple quitando cuma autos por alta autocorrelacion---- HORA
reg23 <- lm(tasa_mortalidad ~ id_hora + caparod + causaacci + aliento + cinturon + sexo, data = atus_prueba)
summary(reg23)

#reg multiple con solo variables significativas
reg24 <- lm(tasa_mortalidad ~ id_hora + aliento + cinturon, data = atus_prueba)
summary(reg24)

#reg multiple con solo variables significativas + sexo
reg25 <- lm(tasa_mortalidad ~ id_hora + aliento + cinturon + sexo, data = atus_prueba)
summary(reg25)

AIC(reg20, reg21, reg22,reg23, reg24, reg25)  # me quedo con reg22 y segundo con reg25

#Correr supuestos sobre reg multiple
autoplot(reg22)
resettest(reg22) # supuesto linealidad : NO CUMPLE

shapiro.test(resid(reg22)) #NORMALIDAD si cumple con normalidad
plot(reg22, 2)

bptest(reg22) #hay homocedasticidad ;)
plot(reg22, 3)

dwtest(reg22) # hay autocorrelacion suma autos y tasa mortalidad, por lo tanto reahacer modelos multiples con id edad o id hora
plot(reg22, 5)

vif(reg22) # no hay alta colinealidad


#tecnica adicional - modelo logit / probit
#transformamos variables
atus_prueba <- atus_prueba %>% 
  mutate(prob_mortal = cut(porcentaje_mortalidad, breaks = c(0, 0.5, 1),
                           labels = c("0","1")))

atus_prueba$prob_mortal <- replace_na(atus_prueba$prob_mortal, "0")

#modelo logit binomial
logit_bin1 <- glm(prob_mortal ~ id_edad + aliento + cinturon, data = atus_prueba , family = binomial(link = "logit"))
summary(logit_bin1)
margins(logit_bin1)

#modelo logit binomial2 + sexo
logit_bin2 <- glm(prob_mortal ~ id_edad + aliento + cinturon + sexo, data = atus_prueba , family = binomial(link = "logit"))
summary(logit_bin2)
margins(logit_bin2)

#LOGIT 1 ES MAS ACURATE

#correr probit sin sexo, probit + sexo

probit_bin1 <- glm(prob_mortal ~ id_edad + aliento + cinturon, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin1)
margins(probit_bin1)

probit_bin2 <- glm(prob_mortal ~ id_edad + aliento + cinturon + sexo, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin2)
margins(probit_bin2)

##Estadistica inferencial de variables explicativas modelos
#diferencia de media entre tasa mortalidad y uso de cinturon

prueba_t1 <- t.test(tasa_mortalidad ~ cinturon, data = atus_prueba) #prueba normal
summary(prueba_t1)
prueba_no_paramer1 <- wilcox.test(tasa_mortalidad ~ cinturon, data = atus_prueba) #prueba no parametrica

ggboxplot(data = atus_prueba, x = "cinturon", y = "tasa_mortalidad")

t.test(tasa_mortalidad ~ aliento, data = atus_prueba)

tible_pruebat <- atus_prueba %>% 
  group_by(cinturon) %>% 
  summarize(total = n(),
            tasa_promedio = mean(tasa_mortalidad))

##graficas cool para presentar
#para verificar distribucion de variables
ggplot(atus_prueba, aes(tasa_mortalidad, fill = cinturon))+
  geom_density(alpha = 0.9)+
  xlim(0, 0.499)+
  ylim(0, 2.5)

qqnorm(atus_prueba$tasa_mortalidad)
qqline(atus_prueba$tasa_mortalidad, col = "red")

ggplot(atus_prueba, aes(cinturon, tasa_mortalidad, fill = cinturon))+
  geom_boxplot(alpha = 0.4)

##estadistica descriptiva
atus_prueba %>% 
  ggplot(aes(x = cinturon, y = tasa_mortalidad, fill = value))+
  geom_tile()+
  geom_text(aes(label = value, family = "sans"))

#matriz correlacion
cor_mat_prueba <- cor(atus_plot, c("porcentaje_mortalidad", "id_hora", "id_edad", "suma_autos", "suma_victimas", "suma_heridos", "suma_fallecidos"))

heatmap(cor_mat_prueba)

atus_plot1 <- atus_plot %>% 
  select(id_hora, id_edad, suma_autos, suma_victimas, suma_heridos, suma_fallecidos, porcentaje_mortalidad)

cor_df <- as.data.frame(cor_mat_prueba)

corr_matrix <- "D:/Laboratorio R/proyecto final accidentes 2021 cdmx/proyecto_metodos_23/matriz_correlacion.xlsx"

write_xlsx(cor_df, path = corr_matrix)

#graficas de reg lineal, reg multiple y logit probit
#reg lineal
atus_prueba %>% 
  ggplot(aes(x = id_hora, y = suma_victimas))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x = "Edad",
       y = "tasa de mortalidad")+
  theme(axis.text = element_text(family = "Times"))

#estadistica descriptiva en funcion de variable dependiente
summarize(sexo_t = count(sexo),
          edad_m = mean(id_edad),
          aliento_t = count(aliento),
          cinturon_t = count(cinturon),
          p_victimas = count(suma_victimas),
          p_heridas = count(suma_heridos),
          p_fallecidas = count(suma_fallecidos),
          tasa_promedio = mean(tasa_mortalidad))

table(atus_prueba$suma_heridos)
table(atus_prueba$suma_fallecidos)
table(atus_prueba$aliento)

mean(atus_prueba$id_edad)

stargazer(logit_bin1, logit_bin2, type = "text")

tabla_3in <- stargazer(reg2, reg7, reg8, type = "text")

stargazer(tabla_3in, title = "resumen modelos lineales", out = "D:/Laboratorio R/proyecto final accidentes 2021 cdmx/proyecto_metodos_23/modelos_lineales1.html", type = "html")

tabla_reg_edad <- stargazer(reg2, reg13_1, reg14, type = "text")

tabla_reg_autos <- stargazer(reg8, reg11, reg12, type = "text")

autoplot(reg2)
autoplot(reg7)
autoplot(reg8)

#pruebas estadisticas supuestos reg2--tm ~ edad
resettest(reg2) #linealidad
shapiro.test(resid(reg2)) #normalidad
bptest(reg2) #homocedasticidad
dwtest(reg2)#independencia

#pruebas estadisticas supuestos reg7--tm ~ hora
resettest(reg7) #linealidad
shapiro.test(resid(reg7)) #normalidad
bptest(reg7) #homocedasticidad
dwtest(reg7)#independencia

#pruebas estadisticas supuestos reg8--tm ~ autos
resettest(reg8) #linealidad
shapiro.test(resid(reg8)) #normalidad
bptest(reg8) #homocedasticidad
dwtest(reg8)#independencia

#modelo reg multiple sin var_sexo y con var_sexo
tabla_reg_multiple <- stargazer(reg16, reg17, type = "text")

autoplot(reg16)
autoplot(reg17)

#probar supuestos regmultiple: tm ~ autos + aliento + cinturon
resettest(reg16) #linealidad
shapiro.test(resid(reg16)) #normalidad
bptest(reg16) #homocedasticidad
dwtest(reg16)#independencia
vif(reg16) #colinealidad

#probar supuestos regmultiple2: tm ~ autos + aliento + cinturon + sexo
resettest(reg17) #linealidad
shapiro.test(resid(reg17)) #normalidad
bptest(reg17) #homocedasticidad
dwtest(reg17)#independencia
vif(reg17) #colinealidad

#Tecnica adicional - comparacion logit binomial convarsexo y sin varsexo
tabla_logit <- stargazer(logit_bin1, logit_bin2, type = "text")

#Modelo PROBIT sin sexo, probit + sexo
probit_bin1 <- glm(prob_mortal ~ id_edad + aliento + cinturon, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin1)
margins(probit_bin1)

probit_bin2 <- glm(prob_mortal ~ id_edad + aliento + cinturon + sexo, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin2)
margins(probit_bin2)

#Tecnica adicional - comparacion probit binomial convarsexo y sin varsexo
tabla_probit <- stargazer(probit_bin1, probit_bin2, type = "text")

#modelo logit binomial
logit_bin01 <- glm(prob_mortal ~ suma_autos + aliento + cinturon, data = atus_prueba , family = binomial(link = "logit"))
summary(logit_bin1)
margins(logit_bin1)

#modelo logit binomia2 + sexo
logit_bin02 <- glm(prob_mortal ~ suma_autos  + aliento + cinturon + sexo, data = atus_prueba , family = binomial(link = "logit"))
summary(logit_bin2)
margins(logit_bin2)

#resume
tabla_logit01 <- stargazer(logit_bin01, logit_bin02, type = "text")

#Modelo PROBIT sin sexo, probit + sexo
#probit01
probit_bin01 <- glm(prob_mortal ~ suma_autos + aliento + cinturon, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin01)
margins(probit_bin01)

probit_bin02 <- glm(prob_mortal ~ id_edad + aliento + cinturon + sexo, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin02)
margins(probit_bin02)

tabla_logit_probit <- stargazer(logit_bin01, probit_bin01, type = "text")

#logit model 01
logit_bin01 <- glm(prob_mortal ~ suma_autos + aliento + cinturon, data = atus_prueba , family = binomial(link = "logit"))
summary(logit_bin1)
margins(logit_bin1)

#modelo logit binomia2 + sexo
logit_bin02 <- glm(prob_mortal ~ suma_autos  + aliento + cinturon + sexo, data = atus_prueba , family = binomial(link = "logit"))
summary(logit_bin2)
margins(logit_bin2)

#resume LOGIT 01 Y 03
tabla_logit01 <- stargazer(logit_bin01, logit_bin02, type = "text")

#Modelo PROBIT01 Y 02
probit_bin01 <- glm(prob_mortal ~ suma_autos + aliento + cinturon, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin1)
margins(probit_bin1)

probit_bin02 <- glm(prob_mortal ~ suma_autos + aliento + cinturon + sexo, data = atus_prueba , family = binomial(link = "probit"))
summary(probit_bin2)
margins(probit_bin2)

tabla_probit02 <- stargazer(probit_bin01, probit_bin02, type = "text")

tabla_log_prob <- stargazer(logit_bin01, probit_bin01, type = "text")