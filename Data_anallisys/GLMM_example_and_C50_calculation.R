#Original data is in spanish. For any questions contact salvadorcalanni@gmail.com


library(ggplot2)
library(boot)
library(glmtoolbox)
library(emmeans)
library(DHARMa)
library(glmmTMB)
library(dplyr)


# Preparing data
data <- read.csv("mice_data.csv", dec = ",")
data$Prop <- data$Respuestas / data$TotalEstimulos

# Subset data for PuestaAPunto experiment
PuestaAPunto <- subset(data, Exp == "PuestaAPunto")

# Ensure that Sexo and Edad are factors
PuestaAPunto$Sexo <- factor(PuestaAPunto$Sexo)
PuestaAPunto$Edad <- factor(PuestaAPunto$Edad)

# Fit the mixed generalized linear model
GLMM <- glmmTMB(
  Prop ~ IM + Sexo + Edad + (1 | Animal),
  data = PuestaAPunto,
  family = binomial,
  weights = TotalEstimulos
)

# Summarize the model
summary(GLMM)

# C50 calculation

# Extract fixed effects from the model
fixed_effects <- fixef(GLMM)$cond

# Extract random intercepts for each animal
random_effects <- ranef(GLMM)$cond$Animal

# Create a unique data frame for animals with their respective Sexo and Edad
unique_animals <- PuestaAPunto %>%
  dplyr::select(Animal, Sexo, Edad) %>%
  dplyr::distinct()

# Initialize a vector to store C50 values for each animal
C50 <- numeric(nrow(unique_animals))

# Loop over each unique animal to calculate C50
for (i in seq_len(nrow(unique_animals))) {
  # Extract the random intercept for the current animal
  current_animal <- unique_animals$Animal[i]
  intercept_animal <- fixed_effects["(Intercept)"] + random_effects[current_animal, "(Intercept)"]
  
  # Get the values of Sexo and Edad for the current animal
  current_sexo <- unique_animals$Sexo[i]
  current_edad <- unique_animals$Edad[i]
  
  # Adjust the intercept for Sexo
  sexo_effect_name <- paste0("Sexo", current_sexo)
  if (sexo_effect_name %in% names(fixed_effects)) {
    intercept_animal <- intercept_animal + fixed_effects[sexo_effect_name]
  }
  
  # Adjust the intercept for Edad
  edad_effect_name <- paste0("Edad", current_edad)
  if (edad_effect_name %in% names(fixed_effects)) {
    intercept_animal <- intercept_animal + fixed_effects[edad_effect_name]
  }
  
  # Coefficient for IM
  beta_IM <- fixed_effects["IM"]
  
  # Calculate C50 for the current animal
  C50[i] <- -intercept_animal / beta_IM
}

# Add the C50 values back to the unique_animals data frame
unique_animals$C50 <- C50

# View the results
print(unique_animals)



# Create a new grouping variable combining Sexo and Edad
unique_animals$Group <- paste(unique_animals$Sexo, unique_animals$Edad, sep = "_")

# Define the desired order of the categories (adjust as necessary)
custom_levels <- unique(unique_animals$Group)

# Set the levels of the factor variable in the data
unique_animals$Group <- factor(unique_animals$Group, levels = custom_levels)

# Plotting the C50 values
ggplot(unique_animals, aes(x = Group, y = C50)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Sexo, shape = Edad), width = 0.1, height = 0, 
              size = 3, alpha = 0.7) +
  scale_color_manual(values = c("M" = "#1F77B4", "H" = "#FF7F0E")) +
  scale_shape_manual(values = c("Joven" = 16, "Vieja" = 17)) +
  ylab("C50") +
  xlab("Group (Sexo_Edad)") +
  theme_minimal()
