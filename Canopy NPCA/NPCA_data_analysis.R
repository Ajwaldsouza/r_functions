## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl, documentation = 0)

## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
install.packages("librarian", repos='http://cran.us.r-project.org')
librarian::shelf(plyr,
                 dplyr,
                 lubridate,
                 tidyr,
                 tibble,
                 rstudioapi,
                 ggplot2,
                 ggpubr,
                 readr,
                 EnvStats,
                 quiet = TRUE,
                 update_all = FALSE)

## ----set wd, message=FALSE, warning=FALSE-------------------------------------
setwd(dirname(getActiveDocumentContext()$path))                                 

## ----label data, message=F, warning=F-----------------------------------------
mydir="raw"
myfiles= list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles

## ----import data--------------------------------------------------------------
data_master <- ldply(myfiles, read.table, sep = ",", fill=T, header = F)%>%     #collectively apply "impost function for all the files in the specified directory
  as_tibble()%>%                                                                #Convert the imported data as a tibble
  na.omit()%>%
  select(V = c(V1,V2,V12, V13, V21))%>%
  #Rename  the columns while importing. 
  rename(time=V1, 
         temp = V2,
         light_int = V3,
         co2_read = V4,
         co2_set = V5)%>%
  slice(c(1:1440))


## ----set numeric, message = F-------------------------------------------------
data_master[, 2:ncol(data_master)] <- 
  lapply(data_master[,2:ncol(data_master)], as.numeric)

## ----set date-time------------------------------------------------------------
data_master$time <- as_datetime(data_master$time)   

## ----threshold----------------------------------------------------------------
threshold <- 10 

## ----remove injection---------------------------------------------------------
cleaned_data <- data_master %>%
  mutate(injection = if_else((co2_read - lag(
    co2_read, default = first(co2_read))) > threshold, TRUE, FALSE)) %>%
  filter(injection == FALSE) %>%
  select(-injection)


## ----minutes------------------------------------------------------------------
cleaned_data$minutes <- seq(0, by = 1, length.out = nrow(cleaned_data))

# data_master$minutes <- data_master$minutes*10
# data_master$hours <- data_master$minutes/60


## ----reduce minutes-----------------------------------------------------------
# # #Reducing the dataset by reducing sampling interval
# groups <- cut(as.POSIXct(data_master$time), breaks="10 min")
# library(plyr)
# data_master <- ddply(data_master, "groups", tail, 1)[, -1]

## ----plot co2-----------------------------------------------------------------
p <-
  ggplot(aes(x=time, y=co2_read), data = data_master) +
  geom_line()+ 
  labs(x="Time (hours)", y=expression(CO[2]~(ppm)), color="10:1")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.8, 0.75),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
p

## ----slope--------------------------------------------------------------------
calc_slope_rsq <- function(x, y) {
  model <- lm(y ~ x)
  slope <- coef(model)[2]
  rsq <- summary(model)$r.squared
  return(list(slope = slope, rsq = rsq))
}

## ----run slope, message=FALSE-------------------------------------------------
cleaned_data <- cleaned_data %>%
  mutate(slope = NA, rsq = NA)

for (i in 1:(nrow(cleaned_data) - 30)) {
  result <- calc_slope_rsq(cleaned_data$minutes[i:(i + 30)], cleaned_data$co2_read[i:(i + 30)])
  cleaned_data$slope[i + 2] <- result$slope  # Center the slope value
  cleaned_data$rsq[i + 2] <- result$rsq      # Center the rsq value
}

## -----------------------------------------------------------------------------
cleaned_data <- cleaned_data %>%
  filter(!is.na(slope) & !is.na(rsq))

## -----------------------------------------------------------------------------
filter_rsq <- 0.5

## -----------------------------------------------------------------------------
cleaned_data <- 
  cleaned_data%>%
  subset(rsq >= filter_rsq)

## -----------------------------------------------------------------------------
calc_flux <- function(slope, T, V, A) {
  p <- 1  # atmospheric pressure in atm
  R <- 0.082  # ideal gas constant in L atm K−1 mol−1
  T_k <- T + 273.15  # convert temperature from Celsius to Kelvin
  
  flux <- -slope * (p / (R * T_k)) * V / A
  return(flux)
}

## -----------------------------------------------------------------------------
V <- 100  #  value in liters
A <- 1  #  value in m^2

## -----------------------------------------------------------------------------
cleaned_data <- cleaned_data %>%
  mutate(flux = calc_flux(slope, temp, V, A))

## -----------------------------------------------------------------------------
ggplot(aes(x=minutes, y=flux), data = cleaned_data) +
  geom_line()+ 
  labs(x="Time (hours)", y=expression(CO[2]~(ppm)))+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.8, 0.75),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

## -----------------------------------------------------------------------------
npca_data <- 
  cleaned_data%>%
  subset(flux > -2)

## -----------------------------------------------------------------------------
ggplot(aes(x=minutes, y=flux), data = npca_data) +
  geom_line()+ 
  labs(x="Time (hours)", y=expression(CO[2]~(ppm)))+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.line = element_line(size = 0.4))+
  theme(legend.position = c(0.8, 0.75),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

## -----------------------------------------------------------------------------
k <- 10  # example value, adjust based on your needs
rosner_test <- EnvStats::rosnerTest(npca_data$flux, k = k)

## -----------------------------------------------------------------------------
print(rosner_test)

## -----------------------------------------------------------------------------
outlier_indices <-
rosner_test$all.stats[rosner_test$all.stats$Outlier, "Index"]

## ----eval=FALSE, include=FALSE------------------------------------------------
#  npca_data_no_outliers <- npca_data[-outlier_indices, ]

