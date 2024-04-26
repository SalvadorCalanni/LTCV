#Original data is in spanish. For any questions contact salvadorcalanni@gmail.com


library(ggplot2)
library(boot)
library(glmtoolbox)
library(emmeans)
library(DHARMa)

#Data preparation

data <- read.csv("mice_data.csv", dec = ",")
names(data)
str(data)
data$Prop<-data$Respuestas/data$TotalEstimulos

data


data$Edad <- factor(data$Edad)
data$Sexo <- factor(data$Sexo)
theme_set(theme_classic(base_size = 20))



PuestaAPunto <- subset(data,Exp == "PuestaAPunto")

ModelMice1<- glm(Prop ~ IM,
                family = binomial,
                weights = TotalEstimulos,
                data = PuestaAPunto)
summary(ModelMice1)

ModelMice2<- glm(Prop ~ IM+Sexo,
                family = binomial,
                weights = TotalEstimulos,
                data = PuestaAPunto)
summary(ModelMice2)

ModelMice3<- glm(Prop ~ IM+Sexo+Edad,
                family = binomial,
                weights = TotalEstimulos,
                data = PuestaAPunto)
summary(ModelMice3)

simModelMice3 <- simulateResiduals(ModelMice3, n = 1000) #esto sirve para ver supuestos
plotQQunif(simModelMice3) #
plotResiduals(simModelMice3) #

#anova to compare between models
anova(ModelMice1, ModelMice2, ModelMice3, test = "Chisq")


#Analizing best model (ModelMice3)

car::Anova(ModelMice3)


(contrastes <- emmeans(ModelMice3, pairwise ~ Sexo, type = "response"))
(contrastes <- emmeans(ModelMice3, pairwise ~ Edad, type = "response"))



#SCGx###############################


SCGx <- subset(data,Exp == "ExpSCGx")

ModeloSCGx1<- glm(Prop ~ IM,
                  family = binomial,
                  weights = TotalEstimulos,
                  data = SCGx)
summary(ModeloSCGx1)

ModeloSCGx2<- glm(Prop ~ IM+Tratamiento,
                 family = binomial,
                 weights = TotalEstimulos,
                 data = SCGx)
summary(ModeloSCGx2)



#anova to compare between models
anova(ModeloSCGx1, ModeloSCGx2, test = "Chisq")


#Analizing best model (ModeloSCGx)

car::Anova(ModeloSCGx2)


(contrastes <- emmeans(ModeloSCGx2, pairwise ~ Tratamiento, type = "response"))


####Graphs###########

MyData1 <- data.frame(IM = seq(1, 18, length=50),
                      Tratamiento = "scgx")

MyData2 <- data.frame(IM = seq(1, 18, length=50),
                      Tratamiento = "sham")



P1 <- predict(ModeloSCGx2, newdata=MyData1, type="response")
P2 <- predict(ModeloSCGx2, newdata=MyData2, type="response")


par(mar = c(5,5,2,2), cex.lab = 1)
g <- ggplot(SCGx, aes(IM, Prop))+
  geom_jitter(aes(color = factor(Tratamiento)), width = 0.2, height = 0.02, size = 4, alpha = 0.8, show.legend = FALSE) +
  scale_color_manual(values = c("#4c1d4b", "#FFA500")) +
  geom_line(aes(IM, P1), color="#4c1d4b", linetype ="solid", data = MyData1, size = 1) +
  geom_line(aes(IM, P2), color="#FFA500",linetype ="solid", data = MyData2, size = 1) + 
  labs (title="SCGx", y = "Response frequency", x= "IM")
g  

