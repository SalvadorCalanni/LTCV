#Original data is in spanish. For any questions contact salvadorcalanni@gmail.com


library(ggplot2)
library(multcomp)
library(boot)
library(glmtoolbox)


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


########Example using young males (MACHOS JOVENES)

YoungMales <- subset(data, Sexo == "M" & Edad == "Joven" 
                     & Tratamiento == "Naive" & Hora == "2pm")

#GLM model

#Prop = proportion of positive responses, IM = Michelson index (Contrast intensity)

ModelYoungMales<- glm(Prop ~ IM,  
                    family = binomial,
                    weights = TotalEstimulos,
                    data = Machos)
summary(ModelYoungMales)

#Plotting the predicted data

MyData1 <- data.frame(IM = seq(5, 25, length=50))

P1 <- predict(ModelYoungMales, newdata=MyData1, type="response")



g <- ggplot(YoungMales, aes(IM, Prop))+
  geom_jitter(aes (), pch= 16, width = 0.2, height = 0.02, size=5, alpha = 0.5, show.legend = FALSE)+
  geom_line(aes(IM, P1), data = MyData1, size = 1) + 
  labs (title="GLM model predictrions for young rat males", y = "Response frequency", x= "Contrast (IM)")
g  






#The rest of the models and plots used in the paper:

Machos <- subset(data, Sexo == "M" & Edad == "Joven" 
                 & Tratamiento == "Naive" & Hora == "2pm")


ModelosMachos<- glm(Prop ~ IM,
                    family = binomial,
                    weights = TotalEstimulos,
                    data = Machos)
summary(ModelosMachos)



MyData1 <- data.frame(IM = seq(5, 25, length=50))

P1 <- predict(ModelosMachos, newdata=MyData1, type="response")



g <- ggplot(Machos, aes(IM, Prop))+
  geom_jitter(aes (), pch= 16, width = 0.2, height = 0.02, size=5, alpha = 0.5, show.legend = FALSE)+
  geom_line(aes(IM, P1), data = MyData1, size = 1) + 
  labs (title="GLM model predictrions", y = "Response frequency", x= "Contrast (IM)")
g  


###################MODELOS######################################

MRatasBinomial2<- glm(Prop ~ IM+Sexo,
                      family = binomial,
                      weights = TotalEstimulos,
                      data = PuestaAPunto)
summary(MRatasBinomial2)



MRatasBinomial3<- glm(Prop ~ IM+Sexo+Edad,
                      family = binomial,
                      weights = TotalEstimulos,
                      data = PuestaAPunto)
summary(MRatasBinomial3)

MRatasBinomial4<- glm(Prop ~ IM+Sexo+Edad+Hora,
                      family = binomial,
                      weights = TotalEstimulos,
                      data = PuestaAPunto)
summary(MRatasBinomial4)





##Modelos puesta a punto

PuestaAPunto <- subset(data,Exp == "PuestaAPunto")

MRatasBinomial<- glm(Prop ~ IM+Edad+Sexo+Hora,
                     family = binomial,
                     weights = TotalEstimulos,
                     data = PuestaAPunto)
summary(MRatasBinomial)
summary(glht(MRatasBinomial, mcp(Edad="Tukey")))
summary(glht(MRatasBinomial, mcp(Sexo="Tukey")))
summary(glht(MRatasBinomial, mcp(Hora="Tukey")))


MRatasPoisson<- glm(Respuestas ~ IM+Edad+Sexo+Hora,
                    family = poisson,
                    weights = TotalEstimulos,
                    data = PuestaAPunto)
summary(MRatasPoisson)


#Gr?ficos
#Machos vs. hemras
theme_set(theme_classic(base_size = 20))

SexoDia <- subset(PuestaAPunto,Hora == "2pm" & Edad == "Joven")

MyData1 <- data.frame(IM = seq(5, 25, length=50),
                      Edad = "Joven", Sexo="M", Hora = "2pm")

MyData2 <- data.frame(IM = seq(5, 25, length=50),
                      Edad = "Joven", Sexo="H", Hora = "2pm")




P1 <- predict(MRatasBinomial, newdata=MyData1, type="response")
P2 <- predict(MRatasBinomial, newdata=MyData2, type="response")



par(mar = c(5,5,2,2), cex.lab = 1)
g <- ggplot(SexoDia, aes(IM, Prop))+
  geom_jitter(aes (color =factor(Sexo), pch = factor(Edad)),width = 0.3, height = 0.02, size=5, alpha = 0.8, show.legend = FALSE)+
  scale_color_manual(values = c( "#e83f3f", "#4c1d4b")) +
  geom_line(aes(IM, P1), color="#4c1d4b", linetype ="solid", data = MyData1, size = 1) +
  geom_line(aes(IM, P2), color="#e83f3f",linetype ="solid", data = MyData2, size = 1) + 
  
  labs (title="Sex", y = "Response frequency", x= "Contrast (IM)")
g  



IC50sexo<- data.frame(Animal = unique(SexoDia$Animal), Sexo = NA, IC50 = NA)

for (i in 1:nrow(IC50sexo)) {
  animal <- IC50sexo$Animal[i]
  animal_data <- data[data$Animal == animal, ]
  
  animal_model <- glm(Prop ~ IM,
                      family = binomial,
                      weights = TotalEstimulos,
                      data = animal_data)
  
  new_data <- data.frame(IM = seq(0, 25, length = 100))
  predicted_probs <- predict(animal_model, newdata = new_data, type = "response")
  ic50sexo <- new_data$IM[which.min(abs(predicted_probs - 0.5))]
  IC50sexo[i, "IC50"] <- ic50sexo
  
  Sexo <- unique(animal_data$Sexo)
  IC50sexo[i, "Sexo"] <- Sexo
}


print(IC50sexo)



# Set the levels of the factor variable in the data
IC50sexo$Sexo <- factor(IC50sexo$Sexo)

# Create the plot with the modified order
ggplot(IC50sexo, aes(x = factor(Sexo), y = IC50)) +
  geom_boxplot() +
  geom_jitter(aes(color = factor(Sexo)), width = 0.05, height = 0.01, size = 5, alpha = 0.8) +
  scale_color_manual(values = c("#e83f3f", "#4c1d4b")) +
  xlab("Sexo") +
  ylab("IC50") +
  ggtitle("IC50 by Tratamiento")



res <- aov(IC50 ~ Sexo, data=IC50sexo)
summary(res)
model.tables(res, type="means", se = TRUE)
TukeyHSD(res)




#ViejasvsJovenes


MachosEdad <- subset(PuestaAPunto,Sexo == "M" & Hora == "2pm")

MyData1 <- data.frame(IM = seq(5, 25, length=50),
                      Edad = "Joven", Sexo="M", Hora = "2pm")

MyData2 <- data.frame(IM = seq(5, 25, length=50),
                      Edad = "Vieja", Sexo="M", Hora = "2pm")


P1 <- predict(MRatasBinomial, newdata=MyData1, type="response")
P2 <- predict(MRatasBinomial, newdata=MyData2, type="response")


par(mar = c(5,5,2,2), cex.lab = 1)
g <- ggplot(MachosEdad, aes(IM, Prop))+
  scale_color_manual(values = c( "#ea5300", "#9e6100")) +
  geom_jitter(aes (color =factor(Edad)),width = 0.3, height = 0.02, size=5, alpha = 0.8, show.legend = FALSE)+
  geom_line(aes(IM, P1), color="#ea5300", linetype ="solid", data = MyData1, size = 1) +
  geom_line(aes(IM, P2), color="#9e6100",linetype ="solid", data = MyData2, size = 1) + 
  labs (title="Viejas vs j?venes", y = "%Respuesta", x= "IM")
g  



IC50edad<- data.frame(Animal = unique(MachosEdad$Animal), Edad= NA, IC50 = NA)

for (i in 1:nrow(IC50edad)) {
  animal <- IC50edad$Animal[i]
  animal_data <- MachosEdad[MachosEdad$Animal == animal, ]
  print(animal_data)
  
  animal_model <- glm(Prop ~ IM,
                      family = binomial,
                      weights = TotalEstimulos,
                      data = animal_data)
  
  new_data <- data.frame(IM = seq(0, 25, length = 100))
  predicted_probs <- predict(animal_model, newdata = new_data, type = "response")
  ic50edad <- new_data$IM[which.min(abs(predicted_probs - 0.5))]
  IC50edad[i, "IC50"] <- ic50edad
  Edad <- unique(animal_data$Edad)
  IC50edad[i, "Edad"] <- Edad
}


print(IC50edad)




# Create the plot with the modified order
ggplot(IC50edad, aes(x = factor(Edad), y = IC50)) +
  geom_boxplot() +
  geom_jitter(aes(color = factor(Edad)), width = 0.05, height = 0.01, size = 5, alpha = 0.8) +
  scale_color_manual(values = c("#ea5300", "#9e6100")) +
  xlab("Edad") +
  ylab("IC50") +
  ggtitle("IC50 by Edad")




res <- aov(IC50 ~ Edad, data=IC50edad)
summary(res)
model.tables(res, type="means", se = TRUE)
TukeyHSD(res)




#Dia vs Noche


MachosHora <- subset(PuestaAPunto,Sexo == "M" & Edad == "Joven")

MyData1 <- data.frame(IM = seq(5, 25, length=50),
                      Edad = "Joven", Sexo="M", Hora = "2pm")

MyData2 <- data.frame(IM = seq(5, 25, length=50),
                      Edad = "Joven", Sexo="M", Hora = "00am")


P1 <- predict(MRatasBinomial, newdata=MyData1, type="response")
P2 <- predict(MRatasBinomial, newdata=MyData2, type="response")


par(mar = c(5,5,2,2), cex.lab = 1)
g <- ggplot(MachosHora, aes(IM, Prop))+
  geom_jitter(aes (color =factor(Hora)), width = 0.3, height = 0.02, size=5, alpha = 0.8,  show.legend = FALSE)+
  scale_color_manual(values = c("#02004e", "#1d87f3")) +
  geom_line(aes(IM, P1), color="#1d87f3",  data = MyData1, size = 1) +
  geom_line(aes(IM, P2), color="#02004e", data = MyData2, size = 1) + 
  labs (title="2pm vs 00am", y = "Response frequency", x= "Contrast (IM)")
g  



IC50hora<- data.frame(Animal = unique(MachosHora$Animal), Hora= NA, IC50 = NA)

for (i in 1:nrow(IC50hora)) {
  animal <- IC50hora$Animal[i]
  animal_data <- MachosHora[MachosHora$Animal == animal, ]
  
  animal_model <- glm(Prop ~ IM,
                      family = binomial,
                      weights = TotalEstimulos,
                      data = animal_data)
  
  new_data <- data.frame(IM = seq(0, 25, length = 100))
  predicted_probs <- predict(animal_model, newdata = new_data, type = "response")
  ic50hora <- new_data$IM[which.min(abs(predicted_probs - 0.5))]
  IC50hora[i, "IC50"] <- ic50hora
  Hora <- unique(animal_data$Hora)
  IC50hora[i, "Hora"] <- Hora
}


print(IC50hora)




# Create the plot with the modified order
ggplot(IC50hora, aes(x = factor(Hora), y = IC50)) +
  geom_boxplot() +
  geom_jitter(aes(color = factor(Hora)), width = 0.05, height = 0.01, size = 5, alpha = 0.8) +
  scale_color_manual(values = c("#02004e", "#1d87f3")) +
  xlab("Hora") +
  ylab("IC50") +
  ggtitle("IC50 by hora")




res <- aov(IC50 ~ Edad, data=IC50hora)
summary(res)
model.tables(res, type="means", se = TRUE)
TukeyHSD(res)

