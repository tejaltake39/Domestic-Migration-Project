library(readxl)
# Import data
data <- read_excel("D://R//data.xlsx")
head(data)
View(data)
data=as.data.frame(data)
head(data)

# Convert categorical variable
data$Region <- as.factor(data$Region)
data$Region
data$Region=as.character(data$Region)
data$Region


# Summary
summary(data)
plot(data)
plot(data$NDIR)
hist(data$NDIR)

str(data)


# Correlation
numeric_data <- data[, sapply(data, is.numeric)]
cor(numeric_data)
print(round(cor(numeric_data),3))

library(corrplot)
# Correlation plot
corrplot(cor(numeric_data), method = "color", type = "lower",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix — Domestic Immigration Data",
         mar = c(0,0,2,0))
pairs(numeric_data)



####

# Full regression model
full_model <- lm(NDIR ~ Unemp+Wage+Crime+Income+Metrop+Poor+Taxes+Educ+BusFail+Temp+Region, data = data)
summary(full_model)
par(mfrow=c(2,2))
plot(full_model)


######
anova(full_model)

#####


library(car)
vif(full_model)


# Load libraries
library(car)
library(ggplot2)

# Fit regression model
model <- lm(NDIR ~ Educ + Income + Temp + Wage +
              Taxes + Unemp + Poor + Metrop +
              Crime + BusFail + Region,
            data = data)

# Calculate VIF
vif_values <- vif(model)

# Convert matrix to dataframe
vif_df <- data.frame(
  Variable = rownames(vif_values),
  GVIF = vif_values[,1],
  Df = vif_values[,2],
  Adjusted_VIF = vif_values[,3]
)

# Sort by Adjusted VIF
vif_df <- vif_df[order(vif_df$Adjusted_VIF), ]

# View table
print(vif_df)

# Plot Adjusted VIF
ggplot(vif_df,
       aes(x = reorder(Variable, Adjusted_VIF),
           y = Adjusted_VIF,
           fill = Adjusted_VIF)) +
  
  geom_bar(stat = "identity") +
  
  coord_flip() +
  
  geom_hline(yintercept = 5,
             color = "orange",
             linetype = "dashed",
             linewidth = 1) +
  
  geom_hline(yintercept = 10,
             color = "red",
             linetype = "dashed",
             linewidth = 1) +
  
  labs(
    title = "Multicollinearity Check Using VIF",
    x = "Variables",
    y = "Adjusted VIF"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

###

res=residuals(full_model)
res

plot(full_model$fitted.values,
     residuals(full_model))

abline(h = 0, col = "red")

####

shapiro.test(res)
hist(res)
qqnorm(res)
qqline(res)

######


install.packages("lmtest")
library(lmtest)

bptest(full_model)


#####

dwtest(full_model)

durbinWatsonTest(full_model)
###########
rstudent(full_model)
which(abs(rstudent(full_model)) > 2)
boxplot(rstudent(full_model),
        main = "Studentized Residuals")


# Cook's distance
cooksd <- cooks.distance(full_model)

# Plot Cook's distance
plot(cooksd,
     type = "h",
     main = "Cook's Distance")

# Influential observations
which(cooksd > (4/nrow(data)))


# Hat values
hatvalues(full_model)

# High leverage points
which(hatvalues(full_model) > (2*length(full_model$coefficients)/nrow(data)))

###
dffits(full_model)

###
dfbetas(full_model)


plot(full_model,which = 4)
plot(full_model,which=5)




####

new_data= data[-c(4,26,29,30,32,45),]
new_data

# Summary
summary(new_data)
plot(new_data)
plot(new_data$NDIR)
hist(new_data$NDIR)

str(new_data)


# Correlation
numeric_data <- new_data[, sapply(new_data, is.numeric)]
cor(numeric_data)
print(round(cor(numeric_data),3))

library(corrplot)
# Correlation plot
corrplot(cor(numeric_data), method = "color", type = "lower",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix — Domestic Immigration Data",
         mar = c(0,0,2,0))
pairs(numeric_data)



####

# Full regression model
full_model <- lm(NDIR ~ Unemp+Wage+Crime+Income+Metrop+Poor+Taxes+Educ+BusFail+Temp+Region, data = new_data)
summary(full_model)
par(mfrow=c(2,2))
plot(full_model)


######
anova(full_model)

#####


library(car)
vif(full_model)




#########
###

res=residuals(full_model)
res

plot(full_model$fitted.values,
     residuals(full_model))

abline(h = 0, col = "red")

####

shapiro.test(res)
hist(res)
qqnorm(res)
qqline(res)

######


install.packages("lmtest")
library(lmtest)

bptest(full_model)


#####

dwtest(full_model)

durbinWatsonTest(full_model)
###########
rstudent(full_model)
which(abs(rstudent(full_model)) > 2)
boxplot(rstudent(full_model),
        main = "Studentized Residuals")


# Cook's distance
cooksd <- cooks.distance(full_model)

# Plot Cook's distance
plot(cooksd,
     type = "h",
     main = "Cook's Distance")

# Influential observations
which(cooksd > (4/nrow(data)))


# Hat values
hatvalues(full_model)

# High leverage points
which(hatvalues(full_model) > (2*length(full_model$coefficients)/nrow(data)))

###
dffits(full_model)

###
dfbetas(full_model)


plot(full_model,which = 4)
plot(full_model,which=5)



























































































# Stepwise regression
step_model <- stepAIC(full_model,
                      direction = "both")

# Summary of best model
summary(step_model)






install.packages("MASS")
library(MASS)

step_model <- stepAIC(full_model,
                      direction = "both")

summary(step_model)

vif(step_model)

par(mfrow=c(2,2))
plot(step_model)

anova(step_model)

anova(step_model,full_model)

