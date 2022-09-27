## global.R ##
library(shiny)
library(dplyr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(webr)
library(moonBook)
library(leaflet)
library(ggtips)
library(plotly)

data <- read.csv("ds_salaries.csv")
#data = data[, -1]
data$X <- data$X + 1 
std_data <- data
std_data$salary_in_usd <- (std_data$salary_in_usd -mean(std_data$salary_in_usd))/sd(std_data$salary_in_usd)
std_data$salary_in_usd <- round(std_data$salary_in_usd, 2)
salaries <- paste(data[, "salary"],data[, "salary_currency"])
data <- cbind(data, salaries)
df1 <- data.frame(aggregate(data$salary_in_usd, 
                           list(data$work_year, 
                                data$employment_type), 
                           FUN=mean)
)
df1$Group.1 <- factor(df1$Group.1 )
df1 <- with(df1, df1[order(df1$Group.1, -df1$x), ])

df2 <- data.frame(aggregate(data$salary_in_usd, 
                           list(data$work_year, 
                                data$experience_level), 
                           FUN=mean)
)
##########################################################################
year_count <- data.frame(table(data[, 2]))
colnames(year_count) <- c("class", "Freq")
year_prop <- round(year_count$Freq / sum(year_count$Freq) * 100, 1)
year_data <- cbind(year_count, year_prop)
year_data <- year_data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(year_prop) - 0.5*year_prop)
##########################################################################
type_count <- data.frame(table(data[, 4]))
colnames(type_count) <- c("class", "Freq")
type_prop <- round(type_count$Freq / sum(type_count$Freq) * 100, 1)
type_data <- cbind(type_count, type_prop)
type_data <- type_data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(type_prop) - 0.5*type_prop)

##########################################################################
pd <- data.frame(table(data[, c(3, 4)]))


##########################################################################
level_count <- data.frame(table(data[, 3]))
colnames(level_count) <- c("class", "Freq")
level_prop <- round(level_count$Freq / sum(level_count$Freq) * 100, 1)
level_data <- cbind(level_count, level_prop)
level_data <- level_data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(level_prop) - 0.5*level_prop)

##########################################################################
# job_title = data.frame(table(data$job_title))
# job_title = job_title[order(-job_title$Freq), ]
# job_title$Prop <- prop.table(job_title$Freq)
# company_location = data.frame(table(data$company_location))
# company_location = company_location[order(-company_location$Freq), ]
# company_location$Prop <- prop.table(company_location$Freq)
###############################################################################
set.seed(123)
x <- sort(rnorm(dim(data)[1]))
normaldata = data.frame(x=x, y=dnorm(x))
################################################################################
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
################################################################################
data$code <- data$company_location
con <- read.csv("con_lon_lat.csv")
#colnames(con) <- c("code", "country", "long",  "lat", )
con[is.na(con$code), 1] <- "NA"
data <- data %>% left_join(con)
################################################################################
std_data$greater_than_mean <- ifelse(std_data$salary_in_usd >= 0, "yes", "no")
city <- data.frame(table(txhousing$city))

