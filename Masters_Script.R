#Open libraries needed for analysis and data transformation. 

library(tidyr)
library(tidyverse)
library(psych)
library(afex)
library(emmeans)
library(broom)
library(purrr)
library(readxl)
library(ggplot2)


View(X_VR_Stab_Data)

#In order to conduct the analysis of the data we must first transform the dataset in order to make it suitable for ANOVA analysis. The current dataset has the each condition together and we need to seperate these in order for the analysis to run on R. 
#Firstly, we will put the dataset into a dataframe.
data <- X_VR_Stab_Data

#Clear missing rows
row.has.na <- apply(data, 1, function(x){any(is.na(x))})
data <- data[!row.has.na,]

#Select relevant columns
data_tidy1 <- data %>% select("ID", "AS1 pse", "AS2 pse", "AS5 pse", "PS1 pse", "PS2 pse", "PS5 pse")

data_tidy2 <- data %>% select("AS1 sigma", "AS2 sigma", "AS5 sigma", "PS1 sigma", "PS2 sigma", "PS5 sigma")

#This gathers the relevant columns and places them under the relevant headings.
data_tidy1 <- data_tidy1 %>%
  gather(key = "Condition", value = "Bias", c("AS1 pse", "AS2 pse", "AS5 pse", "PS1 pse", "PS2 pse", "PS5 pse")) 

data_tidy2 <- data_tidy2 %>%
  gather(key = "Condition2", value = "Sigma", c("AS1 sigma", "AS2 sigma", "AS5 sigma", "PS1 sigma", "PS2 sigma", "PS5 sigma"))

#This combines the two newly created tables. 
data_tidy <- cbind(data_tidy1, data_tidy2)

#The following adds a new column for the different movement types. 
data_tidy$Movement1 <- NA
data_tidy$Movement2 <- NA
data_tidy$Movement5 <- NA

#The following maps where each movement type will be placed using an ifelse statement. This is done for all three movement types in order for them to be placed in the right place. 
data_tidy$Movement1 <- ifelse(data_tidy$Condition == "AS1 pse" | data_tidy$Condition == "PS1 pse", "1", "")
data_tidy$Movement2 <- ifelse(data_tidy$Condition == "AS2 pse" | data_tidy$Condition == "PS2 pse", "2", "")
data_tidy$Movement5 <- ifelse(data_tidy$Condition == "AS5 pse" | data_tidy$Condition == "PS5 pse", "5", "")
data_tidy <- unite(data_tidy, "Movement", c("Movement1", "Movement2", "Movement5"), sep = "")

#The following code adds a new column for the different conditions the participant was placed in. 

data_tidy$Condition1 <- NA

#The following states that if the condition is active it will be placed in the "Active" column. The same is done for the passive condition.
data_tidy$Condition1 <- ifelse(data_tidy$Condition == "AS1 pse" | data_tidy$Condition == "AS2 pse" | data_tidy$Condition == "AS5 pse", "Active", "")
data_tidy$Condition2 <- ifelse(data_tidy$Condition == "PS1 pse" | data_tidy$Condition == "PS2 pse" | data_tidy$Condition == "PS5 pse", "Passive", "")

#The unite function combined the two columns conditions. 
data_tidy <- unite(data_tidy, "Condition", c("Condition1", "Condition2"), sep = "")

#The following orders the data based on the ID.

dataanova <- data_tidy[order(data_tidy$ID),]

#This checks the order of the columns created 
colnames(dataanova)
#This reorders the columns to make the data more readable. 
dataanova2 <- dataanova[, c(1, 4, 5, 2, 3)]


#one samples t test, the condition "5" is removed as it is not relevant to my project.
data1 <- X_VR_Stab_Data

t1 <- t.test(data1$`AS1 pse`, mu = 0)
t2 <- t.test(data1$`AS2 pse`, mu = 0)
t4 <- t.test(data1$`PS1 pse`, mu = 0)
t5 <- t.test(data1$`PS2 pse`, mu = 0)
t7 <- t.test(data1$`AS1 sigma`, mu = 0)
t8 <- t.test(data1$`AS2 sigma`, mu = 0)
t10 <- t.test(data1$`PS1 sigma`, mu = 0)
t11 <- t.test(data1$`PS2 sigma`, mu = 0)

#The following will map the t-test results onto a table. 
ttest_sum <- map_df(list(t1, t2, t4, t5, t7, t8, t10, t11), tidy)

#This removes condition 5 as it is not relevant to my personal project. 
data.anova1 <- dataanova2 %>% filter(Movement !="5")

str(data.anova1)

#This lets R know the relevant factors for the ANOVA.
data.anova1$Movement <- as.factor(data.anova1$Movement)
data.anova1$Condition <- as.factor(data.anova1$Condition)

#To see the descriptives for the ANOVA.
describeBy(data.anova1$Bias, group = list(data.anova1$Condition, data.anova1$Movement))
data_sum1 <- data.anova1 %>% group_by(Condition, Movement) %>% summarise_at("Bias", c(mean, sd), na.rm = T)

#This renames the colums so we can see where the bias and sigma values are placed. 
colnames(data_sum1) <- c("Condition", "Movement", "Bias", "SD")

#A graph added to help visualise the ANOVA results for the bias results.
ggplot(data_sum1,aes(x = Condition, y = Bias, group = Movement, colour = Movement)) + geom_point() + geom_line()

#The following will provide the ANOVA for a within subjects design. 
model1 <- aov_4(Bias ~ Condition * Movement + (1 + Condition * Movement | ID), data = data.anova1, na.rm = TRUE)

#This allows us to view the model itself. 
anova(model1)

#This was used to investigate the pairwise relationships between the variables.
emmeans(model1, pairwise ~ Condition * Movement, adjust = "none")

data_sum.sig1 <- data.anova1 %>% group_by(Condition, Movement) %>% summarise_at("Sigma", c(mean, sd), na.rm = T)
colnames(data_sum.sig1) <- c("Condition", "Movement", "Sigma", "SD")

#A graph used to help visualise the ANOVA results in regard to sigma values. 
ggplot(data_sum.sig1,aes(x = Condition, y = Sigma, group = Movement, colour = Movement)) + geom_point() + geom_line()

#ANOVA is created but for sigma values.
model.sig1 <- aov_4(Sigma ~ Condition * Movement + (1 + Condition * Movement | ID), data = data.anova1, na.rm = TRUE)
anova(model.sig1)

#Pairwise analysis for sigma values.
emmeans(model.sig1, pairwise ~ Condition * Movement, adjust = "none")

#The following will do a correlational analysis using the original untransformed dataset. 

cor.test(X_VR_Stab_Data$`AS1 pse`, X_VR_Stab_Data$`AS2 pse`)

#Now to create the scattergraph for a visual representation of the correlation. Firstly, we must map the X and Y variable to each movement condition we are investigating. 

x <- X_VR_Stab_Data$`AS1 pse`
y <- X_VR_Stab_Data$`AS2 pse`

#The following will create the scatterplot.

plot(x, y, main = "Correlation for active movement",
     xlab = "Rotational Ball Movement", ylab = "Axial Ball Movement",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = X_VR_Stab_Data), col = "blue")

#The same is applied to the passive condition.

x <- X_VR_Stab_Data$`PS1 pse`
y <- X_VR_Stab_Data$`PS2 pse`

#The following will create the scatterplot.

plot(x, y, main = "Correlation for active movement",
     xlab = "Rotational Ball Movement", ylab = "Axial Ball Movement",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = X_VR_Stab_Data), col = "blue")



