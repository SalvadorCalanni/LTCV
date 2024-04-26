#Original data is in spanish. For any questions contact salvadorcalanni@gmail.com


library(ggplot2)
library(boot)
library(glmtoolbox)
library(emmeans)
library(DHARMa)

#Data preparation

data <- read.csv("Rats_data.csv", dec = ",")
names(data)
str(data)
data$Prop<-data$Respuestas/data$TotalEstimulos
data$PropNoRespuesta<-(1-data$Prop)
data$PropFreezing<-data$Freezing/data$TotalEstimulos
data$PropHB<-data$HeadBobbing/data$TotalEstimulos
data$PropRearing<-data$Rearing/data$TotalEstimulos
data


data$Tratamiento <- factor(data$Tratamiento)
data$Edad <- factor(data$Edad)
data$Sexo <- factor(data$Sexo)
data$Hora <- factor(data$Hora)

theme_set(theme_classic(base_size = 20))


#MODELING

PuestaAPunto<- subset(data, Exp == "PuestaAPunto")



Model1<- glm(Prop ~ IM + (1|Animal),
                      family = binomial,
                      weights = TotalEstimulos,
                      data = PuestaAPunto)
summary(Model1)

# Simulations Dharma
sim1 <- simulateResiduals(Model1, n = 1000) #esto sirve para ver supuestos
plotQQunif(sim1) #
plotResiduals(sim1) #

Model2<- glm(Prop ~ IM+Sexo+ (1|Animal),
                      family = binomial,
                      weights = TotalEstimulos,
                      data = PuestaAPunto)
summary(Model2)



Model3<- glm(Prop ~ IM+Sexo+Edad + (1|Animal),
                      family = binomial,
                      weights = TotalEstimulos,
                      data = PuestaAPunto)
summary(Model3)


Model4<- glm(Prop ~ IM+Sexo+Edad+Hora,
                      family = binomial,
                      weights = TotalEstimulos,
                      data = PuestaAPunto)
summary(Model4)

sim4 <- simulateResiduals(Model3, n = 1000) #esto sirve para ver supuestos
plotQQunif(sim4) #
plotResiduals(sim4) #


#anova to compare between models
anova(Model1, Model2, Model3, Model4, test = "Chisq")

#Analizing best model (Model4)

car::Anova(Model4)


(contrastes <- emmeans(Model4, pairwise ~ Sexo, type = "response"))
(contrastes <- emmeans(Model4, pairwise ~ Edad, type = "response"))
(contrastes <- emmeans(Model4, pairwise ~ Hora, type = "response"))



####ISCHEMIA##############

Isquemia <- subset(data,Exp == "Isquemia")

ModelIschemia1<- glm(Prop ~ IM + (1|Animal),
                     family = binomial,
                     weights = TotalEstimulos,
                     data = Isquemia)
summary(ModelIschemia1)

ModelIschemia2<- glm(Prop ~ IM + Tratamiento,
                     family = binomial,
                     weights = TotalEstimulos,
                     data = Isquemia)
summary(ModelIschemia2)

simIsq <- simulateResiduals(ModelIschemia2, n = 1000) #esto sirve para ver supuestos
plotQQunif(simIsq) #
plotResiduals(simIsq) #


#anova to compare between models
anova(ModelIschemia1, ModelIschemia2, test = "Chisq")

#Analizing best model (ModelIschemia2)

car::Anova(ModelIschemia2)


(contrastes <- emmeans(ModelIschemia2, pairwise ~ Tratamiento, type = "response"))



MyData1 <- data.frame(IM = seq(5, 25, length=50),
                      Tratamiento="Control")

MyData2 <- data.frame(IM = seq(5, 25, length=50),
                      Tratamiento="IsqUnilat")

MyData3 <- data.frame(IM = seq(5, 25, length=50),
                      Tratamiento="IsqBilat")


P1 <- predict(ModelIschemia2, newdata=MyData1, type="response")
P2 <- predict(ModelIschemia2, newdata=MyData2, type="response")
P3 <- predict(ModelIschemia2, newdata=MyData3, type="response")


par(mar = c(5,5,2,2), cex.lab = 1)
g <- ggplot(Isquemia, aes(IM, Prop))+
  geom_jitter(aes (color =factor(Tratamiento)),width = 0.3, height = 0.02, size=5, alpha = 0.8, show.legend = FALSE)+
  scale_color_manual(values = c( "#f7c456", "#4c1d4b","#e83f3f"))+
  geom_line(aes(IM, P1), color="#f7c456", data = MyData1, size = 1) +
  geom_line(aes(IM, P2), color="#e83f3f", data = MyData2, size = 1) + 
  geom_line(aes(IM, P3), color="#6b296a", data = MyData3, size = 1) + 
  labs (title="Ischemia", y = "Response frequency", x= "Contraste (IM)")
g  
