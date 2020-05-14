# Set up
library(dplyr)
library(ggplot2)
library(ggpubr)
library(caret)

names(arcep_QoS)

#loading dataset
arcep_QoS_DS <- read.csv("E:\\dataProcess\\FinalProject\\Project_Proposal_GROUP_04\\Datasets\\arcep_QoS.csv")

#removing unused variables 
arcepQoS <- subset( arcep_QoS_DS, select = -c(Bouygues.Telecom.Precision.statistique, Free.Mobile.Precision.statistique, Orange.Precision.statistique, SFR.Precision.statistique))

#removing NA values and checking the structure of the cleaned dataset
networkDS <- na.omit(arcepQoS)
dim(networkDS)
head(networkDS)
levels(networkDS$Type.de.terminal)

#Computing summary statistics by groups - count, mean, sd:
group_by(networkDS, Type.de.terminal) %>%
  summarise(
    count = n(),
    mean = mean(Moyenne, na.rm = TRUE),
    sd = sd(Moyenne, na.rm = TRUE),
    median = median(Moyenne, na.rm = TRUE),
    IQR = IQR(Moyenne, na.rm = TRUE)
  )


#Visualizing our data with Box plots and line plots 
#which allow us to visualize group differences:
#+++++++Plot Qos Moyenne by Type of network using Box plot
ggboxplot(networkDS, x = "Type.de.terminal", y = "Moyenne",
          color = "Type.de.terminal", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("2G/3G", "2G/3G/4G"),
          ylab = "Moyenne", xlab = "Type de terminal")

# Line plot with multiple groups
# +++++++++++++++++++++++
# Plot Moyenne ("Moyenne") by Type de terminal and 
# Add error bars: "Moyenne.Precision.statistique"

ggline(networkDS, x = "Type.de.terminal" , y = "Moyenne", color = "Type.de.terminal",
       order = c("2G/3G", "2G/3G/4G"),
       add = c("Moyenne.Precision.statistique", "dotplot"),
       palette = c("#00AFBB", "#E7B800"),
       ylab = "Moyenne", xlab = "Type.de.terminal
       ")

#Computing one-way ANOVA test of independece (the analysis of variance)
#ANOVA test hypotheses:
#Null hypothesis: the means of the different groups are the same
#Alternative hypothesis: At least one sample mean is not equal to the others.
res.aov <- aov(Moyenne ~ Type.de.terminal, data = networkDS)

# Summary of the analysis
summary(res.aov)

# 1. Homogeneity of variances
plot(res.aov, 1)

# 2. Normality
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov)
shapiro.test(x = aov_residuals )

#machine learning Part
####################################################
####################################################

#checking the structure and randomizing our dataset
sapply(training, class)
set.seed(233)       

#Building the model using linear model function
Linear_model <- lm(Moyenne ~ Type.de.terminal+ Lieu+ Service, data = networkDS)

# starting the prediction using our linear model and saving prediction to a new column
networkDS$predicted_Moyenne <- predict(Linear_model, networkDS)

# calculating the errors
error <- networkDS$predicted_Moyenne - networkDS$Moyenne

#plotting the actual vs predicted values on a scatterplot.
networkDS %>% 
  ggplot(aes(x=Moyenne , y=predicted_Moyenne , color = 'red')) +
  geom_point () +
  geom_abline(slope = 1, intercept= 0,color="green",size=2) +
  ggtitle("MODEL UNDER-PREDICTED SOME\nLARGE Moyenne") +
  annotate(geom = "text", x= 0.6, y=0.2, label = "Model under-predicted\non those Qos")+
  annotate(geom = "text", x= 0.6, y=0.9, label = "Model over-predicted\non those Qos")+
  xlim(0,1)+ylim(0,1)+ theme(legend.position = "none" ) 

#plotting how the model performed the prediction of Moyenne's by Type de terminal.
networkDS %>% 
  mutate(error = predicted_Moyenne - Moyenne) %>% 
  ggplot(aes(x= error)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~ Type.de.terminal, scales = "free_y", ncol = 2) +
  scale_x_continuous( name = "Error") +
  ggtitle("Exploring Errors by Type.de.terminal") +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())
