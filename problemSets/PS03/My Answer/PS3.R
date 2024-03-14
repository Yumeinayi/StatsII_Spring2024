#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# Load data
gdp_data <- read.csv("gdpChange.csv", stringsAsFactors = F)

# If necessary, correct any inconsistencies in the levels
gdp_data$GDPWdiff_cat <- cut(gdp_data$GDPWdiff,
                             breaks = c(-Inf, -1, 0, Inf),
                             labels = c("negative", "no change", "positive"))

# Verify the conversion
table(gdp_data$GDPWdiff_cat)

# Construct the unordered multinomial logit model with "no change" as the reference category
model_unordered <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp_data, ref = "no change")

# Display the model summary to interpret coefficients
summary(model_unordered)

gdp_data$GDPWdiff_ord <- factor(gdp_data$GDPWdiff_cat, levels = c("negative", "no change", "positive"), ordered = TRUE)

# Construct the ordered multinomial logit model
model_ordered <- polr(GDPWdiff_ord ~ REG + OIL, data = gdp_data)

# Display the model summary
summary(model_ordered)


#####################
# Problem 2
#####################

# Load data
mexico_elections <- read.csv("MexicoMuniData.csv")

# Run Poisson regression
model_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                     family = poisson(link = "log"), 
                     data = mexico_elections)

# Display the model summary to examine statistical tests and p-values
summary(model_poisson)

# Create a new data frame for given conditions
new_data <- data.frame(competitive.district = 1, marginality.06 = 0, PAN.governor.06 = 1)

# Use the model to predict the average number of visits
predicted_visits <- predict(model_poisson, newdata = new_data, type = "response")

# Print the predicted number of visits
predicted_visits
