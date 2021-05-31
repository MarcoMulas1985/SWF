# FITTING A SINE FUNCTION FOR MAIN PERIODIC VARIATIONS OF DISPLACEMENTS TIME SERIE.
# THIS SINE WAVE IS ASSESSED WITH A LINEAR REGRESSION BETWEEN DISPLACEMENT VALUES AND A SUM OF TRIGONOMETRIC FUNCTIONS.

#### LOADING LIBRARIES ####

library(tidyverse)
library(lubridate)
library(dplyr)
library(xts) 
library(svDialogs)


#### IMPORT DATA ####

#setting the adress of the directory from which data are available
setwd("~/Pietra_di_Bismantova/crackmeters_fissurometers/data")
getwd()

#selecting csv-files
variables <- dir(pattern = "\\.csv$")     # each crackmeter is spotted with a 4-digit number

hourseq <- seq.POSIXt(as.POSIXct("2015/01/01 00:00:00",units = 'hours'), as.POSIXct("2028/01/01 00:00:00",units = 'hours'), by='1 hour')
dayseq <- seq.Date(as.Date("2015/01/01"),as.Date("2028/01/01"),by = "day")

months <- seq.Date(as.Date("2000/01/01 00:00:00",units = 'hours'),as.Date("2100/01/01 00:00:00",units = 'hours'),by = "1 month")
years <- seq.Date(as.Date("2000/01/01 00:00:00",units = 'hours'),as.Date("2100/01/01 00:00:00",units = 'hours'),by = "1 year")


for (i in 1:length(variables)) {
  
  temp_var <- read.csv(paste0(variables[i]),sep=';',dec=',',check.names = FALSE)
  colnames(temp_var) <- c('time','temp', 'ext_mm')
  temp_var$time <- as.POSIXct(temp_var$time, format = '%d/%m/%Y %H:%M')
  
  temp_hourly <- temp_var %>% group_by(Hourly = cut(temp_var$time, breaks=hourseq)) %>% filter(is.na(time) != TRUE) %>% summarise_all(funs(mean=mean(., na.rm=T))) %>% data.frame()
  temp_hourly$Hourly <- as.POSIXct(temp_hourly$Hourly)
  
  temp_day <- temp_var %>% group_by(Daily = cut(as.Date(temp_var$time), breaks=dayseq)) %>% filter(is.na(time) != TRUE) %>% summarise_all(funs(mean=mean(., na.rm=T))) %>% data.frame()
  temp_day$Daily <- as.POSIXct(temp_day$Daily)
  
  
  assign(paste0('point_',strtrim(variables[i],4)),temp_var)
  assign(paste0('mean_hourly_',strtrim(variables[i],4)),temp_hourly)
  assign(paste0('mean_day_',strtrim(variables[i],4)),temp_day)
  
}


# choosing which kind of data to use to pursue
method <- dlgList(choices = c("mean_day","mean_hourly","point"),preselect = "mean_day",multiple = FALSE,title = "Type of data to use:")$res



#### TRAINING SET ####

# For each crackmeter, time range on which linear regression will be performed is chosen beforehand.
# Ideally we have to spot a time interval during which the regime of displacement is periodic, continuous and where we don't observe any big gap between subsequent observations.

data_crack <- get(str_c(method,'_0100'))
psubset_0100 <- data_crack
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)

data_crack <- get(str_c(method,'_0200'))
psubset_0200 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/05/18")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##0200
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/05/18"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_0500'))
psubset_0500 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/10/15"))) ##0500
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/10/15"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_0600'))
psubset_0600 <- filter(data_crack, (data_crack[,1] <= as.Date("2015/02/12")) & (data_crack[,1] >= as.Date("2015/01/08"))) ##0600
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/01/08"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2015/02/12"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_0601'))
psubset_0601 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##0601
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_0900'))
psubset_0900 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##0900
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_1010'))
psubset_1010 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/07/12")) & (data_crack[,1] >= as.Date("2015/11/15"))) ##1010
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/11/15"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/07/12"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_1200'))
psubset_1200 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/01/01"))) ##1200
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/01/01"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_1600'))
psubset_1600 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/10/01")) & (data_crack[,1] >= as.Date("2015/11/15"))) ##1600
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/11/15"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/10/01"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_1700'))
psubset_1700 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/01/10"))) ##1700
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/01/10"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_1800'))
psubset_1800 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/11/01")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##1800
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/11/01"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_1900'))
psubset_1900 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##1900
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c(method,'_2020'))
psubset_2020 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2015/10/01"))) ##2020
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/10/01"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')



# # selecting a crackmeter
# crackmeter <- dlgInput(message = "Which crackmeter do you want to track?
#                        Give a number:")$res
# 



#### FILTERING THE MAIN HARMONIC ASSOCIATED TO PERIODIC AND SEASONAL CHANGES IN TEMPERATURE

# choosing which kind of data to use to pursue
frq_choice <- dlgList(choices = c("spotting main harmonic","frequency corresponding to a 1-year period"),preselect = "frequency corresponding to a 1-year period",multiple = FALSE,title = "Choose the frequency for the wave:")$res

for (i in (1:length(variables))) {
  
  data_crack <- get(str_c(method,'_',strtrim(variables[i],4)))
  
  if (frq_choice == "frequency corresponding to a 1-year period") {
    frq_main_harmonic <- 1/(24*3600*365)
    
  } else {
    
    data_type_fft <- dlgList(choices = c('mean_day','mean_hourly','point'),preselect = 'mean_hourly',multiple = FALSE, title = 'Select the type of data to compute FFT:')$res
  
    data_fft <- get(str_c(data_type_fft,'_',strtrim(variables[i],4)))
    
    acq_period <- difftime(data_fft[2,1],data_fft[1,1],units = 'secs')
    acq_frq <- 1/(as.numeric(acq_period))     #frequency at which data have been recorded (sampling frequency)
    
    temperature <- data_fft %>% select(contains('temp')) %>% unlist() %>% unname()
    displ <- data_fft %>% select(contains('ext_mm')) %>% unlist() %>% unname()
    
    # COMPUTING FFT
    temp_d_fourier <- fft(temperature)
    
    x_frq <- seq(0,length(temp_d_fourier)-1,by = 1)*(acq_frq/length(temp_d_fourier))
    
    displ_d_fourier <- fft(displ)
    
    temp_weight <- Mod(temp_d_fourier)
    displ_weight <- Mod(displ_d_fourier)
    
    dens_spec_temp <- data.frame(x_frq,temp_weight)
    assign(str_c('dsp_temp_',strtrim(variables[i],4)),dens_spec_temp)
    
    
    #### PICKING OUT THE FREQUENCY OF THE MAIN HARMONIC ####
    
    f_lim_inf <- 1/(24*3600*(365+3*30))         # frequency corresponding approximatively to a 15-month period
    assign(str_c('f_lim_inf_',strtrim(variables[i],4)),f_lim_inf)
    
    f_lim_sup <- 1/(24*3600)                    # frequency corresponding to a one-day period
    assign(str_c('f_lim_sup_',strtrim(variables[i],4)),f_lim_sup)
    
    focus <- dens_spec_temp %>% filter((dens_spec_temp[,1] >= f_lim_inf) & (dens_spec_temp[,1] <= f_lim_sup))
    
    if (nrow(focus) > 0) {
      max_local <- focus %>% filter(focus[,2] == max(focus[,2],na.rm = TRUE))
      frq_main_harmonic <- max_local
    }
  }
    
  assign(str_c('frq_main_harmonic_',strtrim(variables[i],4)),frq_main_harmonic)
    
  temperature <- data_crack %>% select(contains('temp')) %>% unlist() %>% unname()
  displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
  acq_period <- difftime(data_crack[2,1],data_crack[1,1],units = 'secs')
  

  
  
  
  
  #### CARRYING OUT LINEAR REGRESSION TO MODEL THE BEST WAVE FITTING PERIODIC REVERSIBLE VARIATIONS OF DISPLACEMENTS ####
  
  psubset <- get(str_c('psubset_',strtrim(variables[i],4)))
  
  x_time <- psubset[,1]
  pdispl <- psubset %>% select(contains('ext_mm')) %>% unlist() %>% unname()
  
  sub_wave1 <- sin(2*pi*frq_main_harmonic*as.numeric(x_time))
  sub_wave2 <- cos(2*pi*frq_main_harmonic*as.numeric(x_time))

  model <- lm(pdispl ~ (sub_wave1 + sub_wave2))
  assign(str_c('ap_',strtrim(variables[i],4)),model)
  
  wave1 <- sin(2*pi*frq_main_harmonic*as.numeric(data_crack[,1]))
  wave2 <- cos(2*pi*frq_main_harmonic*as.numeric(data_crack[,1]))
  
  wave_simul <- model$coefficients[1] + model$coefficients[2]*wave1 + model$coefficients[3]*wave2
  assign(str_c('wave_simul_',strtrim(variables[i],4)),wave_simul)

  
  
  
  
  #### CARRYING OUT LINEAR REGRESSION TO MODEL THE BEST WAVE FITTING PERIODIC VARIATIONS OF THE TEMPERATURE ####
  
  x_time <- data_crack[,1]

  wave1 <- sin(2*pi*frq_main_harmonic*as.numeric(x_time))
  wave2 <- cos(2*pi*frq_main_harmonic*as.numeric(x_time))
  
  model <- lm(temperature ~ (wave1 + wave2))
  assign(str_c('apt_',strtrim(variables[i],4)),model)
  
  temp_simul <- model$coefficients[1] + model$coefficients[2]*wave1 + model$coefficients[3]*wave2
  assign(str_c('temp_simul_',strtrim(variables[i],4)),temp_simul)
  # assign(str_c('residuals_temp_',strtrim(variables[i],4)),model$residuals)
  assign(str_c('residuals_temp_',strtrim(variables[i],4)),temperature - temp_simul)
  
  
  #### REMOVING VALUES COMPUTED WITH THE FUNCTION PREVIOUSLY FOUND FROM THE ORIGINAL SIGNAL ####
  
  deltap <- displ - wave_simul
  assign(str_c('deltap_',strtrim(variables[i],4)),deltap)
  

  
  #### COMPUTING IRREVERSIBILITY INDEX AS A RUNNING STANDARD DEVIATION CALCULATED WITH DIFFERENCES (DELTAP) OVER A 21-DAY PERIOD ####

  tw <- 21    # time range in which related differential values are selected so as to compute running standard deviation on the whole time serie/length of the time window (number of days)

  irr_index <- data.frame(matrix(NA,nrow(data_crack),1))

  k <- 1

  while (k < nrow(data_crack) + 1) {

    start_date <- data_crack[k,1] - tw*(24*3600)
    end_date <- data_crack[k,1]
    ind <- which((data_crack[,1] >= start_date) & (data_crack[,1] <= end_date))

    if (length(ind) > ((tw*24*3600)/(as.numeric(acq_period)*10))) {
      irr_index[k,1] <- 4*sd(abs(deltap[ind]),na.rm = TRUE)
    }

    k <- k+1
  }

  assign(str_c("p_irr_index_",strtrim(variables[i],4)),irr_index)

}





#### 2ND STEP: ANALYSIS OF RESIDUALS ####
# APPLYING THE METHOD IMPLEMENTED IN THE SCRIPT NAMED "Irr_index" TO RESIDUAL DATA

for (i in (1:length(variables))) {
  initial_data <- get(str_c(method,'_',strtrim(variables[i],4)))
  res_temp <- get(str_c('residuals_temp_',strtrim(variables[i],4)))
  res_displ <- get(str_c('deltap_',strtrim(variables[i],4)))
  
  res <- data.frame(initial_data[,1],res_temp,res_displ)
  colnames(res)[2] <- 'temp'
  colnames(res)[3] <- 'ext_mm'
  
  assign(str_c('residuals_',method,'_',strtrim(variables[i],4)),res)
}



##### TRAINING SET ####

# For each crackmeter, time range on which linear regression will be performed is chosen beforehand.
# So as to outline such a time interval, we have to point out a period during which the regime of displacement is mainly continuous and where we don't notice/repear any big gap between subsequent observations.

data_crack <- get(str_c('residuals_',method,'_0100'))
rsubset_0100 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/09/13"))) ##0100
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/09/13"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_0200'))
rsubset_0200 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/05/14")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##0200
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/05/14"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_0500'))
rsubset_0500 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/04/04")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##0500
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/04/04"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_0600'))
rsubset_0600 <- filter(data_crack, (data_crack[,1] <= as.Date("2015/02/12")) & (data_crack[,1] >= as.Date("2015/01/08"))) ##0600
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/01/08"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2015/02/12"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_0601'))
rsubset_0601 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/07/29"))) ##0601
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/07/29"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_0900'))
rsubset_0900 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/07/29"))) ##0900
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/07/29"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_1010'))
rsubset_1010 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/05/18")) & (data_crack[,1] >= as.Date("2015/09/03"))) ##1010
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/09/03"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/05/18"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_1200'))
rsubset_1200 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/06/12")) & (data_crack[,1] >= as.Date("2016/07/29"))) ##1200
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/07/29"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/06/12"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_1600'))
rsubset_1600 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/09/01")) & (data_crack[,1] >= as.Date("2016/08/01"))) ##1600
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/08/01"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/09/01"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_1700'))
rsubset_1700 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/01/01")) & (data_crack[,1] >= as.Date("2015/01/01"))) ##1700
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/01/01"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/01/01"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_1800'))
rsubset_1800 <- filter(data_crack, (data_crack[,1] <= as.Date("2016/09/01")) & (data_crack[,1] >= as.Date("2015/08/01"))) ##1800
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2015/08/01"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2016/09/01"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_1900'))
rsubset_1900 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/07/29"))) ##1900
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/07/29"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')

data_crack <- get(str_c('residuals_',method,'_2020'))
rsubset_2020 <- filter(data_crack, (data_crack[,1] <= as.Date("2017/07/21")) & (data_crack[,1] >= as.Date("2016/07/29"))) ##2020
displ <- data_crack %>% select(contains('ext_mm')) %>% unlist() %>% unname()
plot(data_crack[,1],displ,lwd = 0.1)
abline(v = as.POSIXct("2016/07/29"),lwd = 0.1,col = 'red')
abline(v = as.POSIXct("2017/07/21"),lwd = 0.1,col = 'red')


#saving subsets
list_var <- c('rsubset_0100')

for (i in (2:length(variables))) {
  list_var <- c(list_var,str_c('rsubset_',strtrim(variables[i],4)))
}

#setting the adress of the directory where variables will be stored
setwd("~/Pietra_di_Bismantova/crackmeters_fissurometers/variables/point-data_sine-fit")
getwd()
save(list = list_var,file = "rsubset_point-data")




#### LINEAR REGRESSION ####

list_var <- c()

for (i in (1:length(variables))) {
  
  y <- get(paste0('rsubset_',strtrim(variables[i],4)))  # y is provided with all recorded values of temperature
  y <- select(y,contains('temp'))
  y <- as.numeric(as.character(unlist(y)))
  
  x <- get(paste0('rsubset_',strtrim(variables[i],4)))   # x is provided with observed values of displacement
  x <- select(x,contains('ext_mm'))
  x <- as.numeric(as.character(unlist(x)))
  # x <- x - x[1]
  
  assign(paste0('ar_',strtrim(variables[i],4)),lm(x ~ y))
  list_var <- c(list_var,str_c('ar_',strtrim(variables[i],4)))
}

#saving linear models
save(list = list_var,file = "ar_point-data")



#### COMPUTING ESTIMATED DISPLACEMENTS WITH THE LINEAR MODEL PREVIOUSLY ASSESSED #### 

list_var <- c()

for (i in (1:length(variables))) {
  
  coeff_lm <- get(str_c("ar_",strtrim(variables[i],4)))
  x <- get(str_c('residuals_',method,'_',strtrim(variables[i],4)))
  temp <- x %>% select(contains('temp')) %>% unlist() %>% unname()
  
  assign(str_c("r_ext_simul_",strtrim(variables[i],4)), coeff_lm$coefficients[1] + coeff_lm$coefficients[2]*temp)
  list_var <- c(list_var,str_c('r_ext_simul_',strtrim(variables[i],4)))
}

#saving estimated residual displacements
save(list = list_var,file = "r-ext-simul_point-data")




#### CALCULATING DIFFERENCES BETWEEN OBSERVED AND ESTIMATED DISPLACEMENTS ####

list_var <- c()

for (i in (1:length(variables))) {
  
  obs <- get(str_c('residuals_',method,'_',strtrim(variables[i],4)))
  est <- get(str_c("r_ext_simul_",strtrim(variables[i],4)))
  
  displ <- obs %>% select(contains('ext_mm')) %>% unlist() %>% unname()
  
  assign(str_c("residual_deltap_",strtrim(variables[i],4)),displ - est)
  list_var <- c(list_var,str_c('residual_deltap_',strtrim(variables[i],4)))
}

#saving residual absolute differences between residual displacements and estimated residual displacements
save(list = list_var,file = "residual-deltap_point-data")




#### IRREVERSIBILITY INDEX ####

list_var <- c()

tw <- 21    # time range in which related differential values are selected so as to compute left-aligned running standard deviation on the whole time serie/length of the time window (number of days)

for (i in (1:length(variables))) {
  
  delta <- get(str_c("residual_deltap_",strtrim(variables[i],4)))
  data_crack <- get(str_c('residuals_',method,'_',strtrim(variables[i],4)))
  acq_period <- difftime(data_crack[2,1],data_crack[1,1],units = 'secs')
  
  irr_index <- data.frame(matrix(NA,nrow(data_crack),1))
  
  k <- 1
  
  while (k < nrow(data_crack) + 1) {
    
    start_date <- data_crack[k,1] - tw*(24*3600)
    end_date <- data_crack[k,1]
    ind <- which((data_crack[,1] >= start_date) & (data_crack[,1] <= end_date))
    
    if (length(ind) > ((tw*24*3600)/(as.numeric(acq_period)*10))) {         # we need a certain amount/quantity of measures consecutively recorded before the date on which the irreversibility index will be calculated
      irr_index[k,1] <- 4*sd(abs(delta[ind]),na.rm = TRUE)
    }
    
    k <- k+1
  }
  
  assign(str_c("r_irr_index_",strtrim(variables[i],4)),irr_index)
  list_var <- c(list_var,str_c('r_irr_index_',strtrim(variables[i],4)))
  print(i)
}

#saving irreversibility indexes calculated with residuals from the first step of the method fitting a sine wave to the time serie showing displacements
save(list = list_var,file = "r-irr-index_point-data")



#### GRAPHS ####

#setting the path toward the directory where reports will be stored
setwd("~/Pietra_di_Bismantova/crackmeters_fissurometers/reports/single wave fitting")
getwd()


for (i in (1:length(variables))) {
  
  obs <- get(str_c(method,'_',strtrim(variables[i],4)))
  wave <- get(str_c("wave_simul_",strtrim(variables[i],4)))
  temp_simul <- get(str_c('temp_simul_',strtrim(variables[i],4)))
  coeff_lm <- get(str_c("ap_",strtrim(variables[i],4)))
  coeff_lm2 <- get(str_c("apt_",strtrim(variables[i],4)))
  psubset <- get(paste0('psubset_',strtrim(variables[i],4)))[1]
  deltap <- get(str_c("deltap_",strtrim(variables[i],4)))
  residuals_temp <- get(str_c('residuals_temp_',strtrim(variables[i],4)))
  irr_index <- get(str_c("p_irr_index_",strtrim(variables[i],4))) %>% unlist() %>% unname()
  
  frq_main_harmonic <- get(str_c('frq_main_harmonic_',strtrim(variables[i],4)))
  
  
  pdf(file=as.character(paste(Sys.Date(),str_c("_rmv-main-wave_",strtrim(variables[i],4),'_',method,"_data_v1.pdf"),sep='')),paper ="a4")
  
  par(mfrow=c(3,1),mar = c(5,5,2,3))
  
  

  # displaying the whole time serie of observed displacements with their associated temperature at which they have been measured
  
  displ <- obs %>% select(contains('ext_mm')) %>% unlist() %>% unname()
  temp <- obs %>% select(contains('temp')) %>% unlist() %>% unname()
  
  plot(obs[,1],temp,lwd = 0.1,col = 'grey',
       xlab ='', ylab = '',yaxt='n',type='l')
  
  axis(4)
  mtext(side=4,text = "Temperature (°C)",col = 'grey',cex = .5,line = 2)
  
  par(new = TRUE)
  plot(obs[,1],displ,lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(displ)),
       xlab = '', ylab = 'Displacement (mm)')
  
  abline(v=as.POSIXct(months),lwd = 0.1,col='grey')
  abline(v=as.POSIXct(years),lwd = 0.1,col='black')
  abline(v = psubset[1,1],lwd = 0.1,col = 'red')
  abline(v = psubset[length(unlist(psubset)),1],lwd = 0.1,col = 'red')
  
  
  
  # displaying the spectral density of temperature variations
  
  if (frq_choice == "spotting main harmonic") {
    
    dsp_temp <- get(str_c('dsp_temp_',strtrim(variables[i],4)))
    f_lim_inf <- get(str_c('f_lim_inf_',strtrim(variables[i],4)))
    f_lim_sup <- get(str_c('f_lim_sup_',strtrim(variables[i],4)))
  
    plot(dsp_temp[(1:floor(nrow(dsp_temp)/2)),1],log(dsp_temp[(1:floor(nrow(dsp_temp)/2)),2]),type = 'l',lwd = 0.1,
         xlab = 'frequency (Hz)',ylab = 'temperature spectral dens. (log scale)',
         main = str_c('main frequency: ',frq_main_harmonic,' (Hz)'))
    abline(v = f_lim_inf,lwd = 0.1,col = 'red')
    abline(v = f_lim_sup,lwd = 0.1,col = 'red')
    abline(v = frq_main_harmonic,lwd = 0.1,col = 'green')
  }
  
  
  
  # displaying the simulated wave describing only periodic variations of displacements

  plot(obs[,1],temp,lwd = 0.1,col = 'grey',
       xlab ='', ylab = '',yaxt='n',type='l')
  
  axis(4)
  mtext(side=4,text = "Temperature (°C)",col = 'grey',cex = .5,line = 2)
  
  par(new = TRUE)
  plot(obs[,1],wave,lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(wave)),
       main = str_c('R_squared =',as.character(round(summary(coeff_lm)$r.squared,3))),
       xlab = '', ylab = 'periodic displacements (mm)')
  
  abline(v=as.POSIXct(months),lwd = 0.1,col='grey')
  abline(v=as.POSIXct(years),lwd = 0.1,col='black')
  abline(v = psubset[1,1],lwd = 0.1,col = 'red')
  abline(v = psubset[length(unlist(psubset)),1],lwd = 0.1,col = 'red')
  
  
  title(main =as.character(paste(Sys.Date(),str_c("_",strtrim(variables[i],4),'    pre-processing data method: ',method),sep='')),outer = TRUE,line = -1)
  
  
  

  # displaying (once again) the whole time serie of the observed displacements with the computed wave superimposed
  
  plot(obs[,1],displ,type='l',xlab = '',ylab = "Displacement (mm)",lwd = 0.1,col='blue')
  points(obs[,1],wave,lwd = 0.1,col = 'red')
  
  abline(v=as.POSIXct(months),lwd = 0.1,col='grey')
  abline(v=as.POSIXct(years),lwd = 0.1,col='black')
  abline(v = psubset[1,1],lwd = 0.1,col = 'red')
  abline(v = psubset[length(unlist(psubset)),1],lwd = 0.1,col = 'red')
  
  
  
  # displaying the whole time serie of differences between values of observed displacements and those from the wave
  
  plot(obs[,1],deltap,xlab = '',ylab = 'Delta (mm)',lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(deltap)))
  
  abline(h = 0,lwd = 0.1)

  abline(v=as.POSIXct(months),lwd = 0.1,col='grey')
  abline(v=as.POSIXct(years),lwd = 0.1,col='black')
  abline(v = psubset[1,1],lwd = 0.1,col = 'red')
  abline(v = psubset[length(unlist(psubset)),1],lwd = 0.1,col = 'red')
  

  
  
  # displaying residual displacements (which are assumed to be irreversible) with respect to temperature
  
  plot(temp,deltap,lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(deltap)),
       xlab = 'Temperature (°C)',ylab = 'Remaining displacements_deltap (mm)')
  
  
    
  
  # displaying the time serie of absolute values of the reduced mean-centered irreversibility index

  plot(obs[,1],temp,lwd = 0.1,col = 'grey',
       xlab ='', ylab = '',yaxt='n',type='l')

  abline(v = as.POSIXct(months),col='grey')
  abline(v = as.POSIXct(years),col='black')
  abline(v = psubset[1,1],col = 'red')
  abline(v = psubset[length(unlist(psubset)),1],col = 'red')

  axis(4)
  mtext(side=4,text = "Temperature (°C)",col = 'grey',cex = .5,line = 2)

  par(new=T)
  plot(obs[,1],abs((irr_index - mean(irr_index,na.rm = TRUE))/sd(irr_index,na.rm = TRUE)),
       ylab= "Z-score irr. index",xlab='',type='l',lwd = 0.8,col = 'blue',
       main= paste0('Moving window = ',tw,' days'),xaxt = 'n')
  
  
  title(main =as.character(paste(Sys.Date(),str_c("_",strtrim(variables[i],4),'    pre-processing data method: ',method),sep='')),outer = TRUE,line = -1)
  
  
  
  # displaying the simulated wave fitting variations of the temperature
  
  plot(obs[,1],temp,lwd = 0.1,col = 'grey',xlab ='', ylab = 'Temperature (°C)',type='l',
       main = str_c('R_squared =',as.character(round(summary(coeff_lm2)$r.squared,3))))
  
  points(obs[,1],temp_simul,lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(temp_simul)))
  
  abline(v=as.POSIXct(months),lwd = 0.1,col='grey')
  abline(v=as.POSIXct(years),lwd = 0.1,col='black')
  abline(v = psubset[1,1],lwd = 0.1,col = 'red')
  abline(v = psubset[length(unlist(psubset)),1],lwd = 0.1,col = 'red')
  
  
  
  # plotting differences between recorded values of temperature and those generated with the fitting model
  # residuals remaining from the model
  
  plot(obs[,1],temp,lwd = 0.1,col = 'grey',xlab ='', ylab = '',yaxt='n',type='l')
  
  axis(4)
  mtext(side=4,text = "Temperature (°C)",col = 'grey',cex = .5,line = 2)
  
  par(new = TRUE)
  plot(obs[,1],residuals_temp,type = 'l',lwd = 0.1,
       col = 'blue',xlab = '', ylab = 'Temperature residuals (°C)')
  
  abline(v=as.POSIXct(months),lwd = 0.1,col='grey')
  abline(v=as.POSIXct(years),lwd = 0.1,col='black')
  abline(v = psubset[1,1],lwd = 0.1,col = 'red')
  abline(v = psubset[length(unlist(psubset)),1],lwd = 0.1,col = 'red')
  
  
  
  # displaying residual displacements (which are assumed to be irreversible) with respect to temperature residuals

  plot(residuals_temp,deltap,lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(deltap)),
       xlab = 'Temperature residuals (°C)',ylab = 'Remaining displacements_deltap (mm)')
  
  
  title(main =as.character(paste(Sys.Date(),str_c("_",strtrim(variables[i],4),'    pre-processing data method: ',method),sep='')),outer = TRUE,line = -1)

  
  
  
  
  obs <- get(str_c('residuals_',method,'_',strtrim(variables[i],4)))
  displ <- obs %>% select(contains('ext_mm')) %>% unlist() %>% unname()
  temp <- obs %>% select(contains('temp')) %>% unlist() %>% unname()
  est <- get(str_c("r_ext_simul_",strtrim(variables[i],4)))
  coeff_lm <- get(str_c("ar_",strtrim(variables[i],4)))
  subset <- get(paste0('rsubset_',strtrim(variables[i],4)))[1]
  delta <- get(str_c("residual_deltap_",strtrim(variables[i],4)))
  irr_index <- get(str_c("r_irr_index_",strtrim(variables[i],4))) %>% unlist() %>% unname()
  
  
  # displaying the whole time serie of residual displacements
  
  plot(obs[,1],displ,lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(displ)),
       xlab = '', ylab = 'Displacement (mm)')
  
  abline(v=as.POSIXct(months),col='grey')
  abline(v=as.POSIXct(years),col='black')
  abline(v = subset[1,1],col = 'red')
  abline(v = subset[length(unlist(subset)),1],col = 'red')
  
  
  
  # displaying residual displacements with respect to residual temperature
  
  plot(temp,displ,lwd = 0.1,
       col = colorRampPalette(c("blue","green",'yellow', "red"))(length(temp)),
       main=str_c('Ext. ',strtrim(variables[i],4),'     R_squared =',as.character(round(summary(coeff_lm)$r.squared,3))), ylab= 'Displacement (mm)', xlab= 'Temperature (°C)')
  
  abline(a = coeff_lm$coefficients[1],b = coeff_lm$coefficients[2])
  
  
  
  
  # displaying the time serie of absolute values of the reduced mean-centered irreversibility index
  
  plot(obs[,1],temp,lwd = 0.1,col = 'grey',
       xlab ='', ylab = '',yaxt='n',type='l')
  
  abline(v=as.POSIXct(months),col='grey')
  abline(v=as.POSIXct(years),col='black')
  abline(v = subset[1,1],col = 'red')
  abline(v = subset[length(unlist(subset)),1],col = 'red')
  
  axis(4)
  mtext(side=4,text = "Temperature (°C)",col = 'grey',cex = .5,line = 2)
  
  
  par(new=T)
  # plot(obs$Hourly[-(1:((tw*24)-1))],abs((irr_index - mean(irr_index,na.rm = TRUE))/sd(irr_index,na.rm = TRUE)),
  #      ylab= "Irreversibility index - blue",xlab='',type='l',lwd = 1,col = 'blue',main= paste0('Moving window = ',tw,' days'),
  #      xaxt = 'n')
  
  plot(obs[,1],abs((irr_index - mean(irr_index,na.rm = TRUE))/sd(irr_index,na.rm = TRUE)),
       ylab= "Z-score res_irr. index",xlab='',type='l',lwd = 0.8,col = 'blue',main= paste0('Moving window = ',tw,' days'),
       xaxt = 'n')
  
  
  title(main =as.character(paste(Sys.Date(),str_c("_",strtrim(variables[i],4),'    pre-processing data method: ',method),sep='')),outer = TRUE,line = -1)
  
  
  
  # displaying (once again) the whole time serie of the remaining displacements
  
  plot(obs[,1],displ,type='l',xlab = '',ylab = "Displacement (mm)",lwd = 0.1,col='red')
  
  abline(v=as.POSIXct(months),col='grey')
  abline(v=as.POSIXct(years),col='black')
  
  
  
  # displaying the whole time serie of estimated residual displacements superimposed with residual variations of the temperature
  
  plot(obs[,1],temp,col = 'grey',
       xlab = '', ylab = '',yaxt='n',type='l',lwd = 0.1)
  
  abline(v=as.POSIXct(months),col='grey')
  abline(v=as.POSIXct(years),col='black')
  abline(v = subset[1,1],col = 'red')
  abline(v = subset[length(unlist(subset)),1],col = 'red')
  
  axis(4)
  mtext(side=4,text = "Temperature (°C)",col = 'grey',cex = .5,line = 2)
  
  par(new=T)
  plot(obs[,1],est,type='l',ylab = "estimated displ. (mm) - green",xlab = '',xaxt = 'n',lwd = 0.1,col='green')
  
  
  
  
  # displaying the whole time serie of differences between observed and estimated residual displacements
  
  plot(obs[,1],delta,type='l',xlab = '',lwd = 0.1,col = 'blue')
  
  abline(v=as.POSIXct(months),col='grey')
  abline(v=as.POSIXct(years),col='black')
  abline(v = subset[1,1],col = 'red')
  abline(v = subset[length(unlist(subset)),1],col = 'red')
  
  
  title(main =as.character(paste(Sys.Date(),str_c("_",strtrim(variables[i],4),'    pre-processing data method: ',method),sep='')),outer = TRUE,line = -1)
  
  
  dev.off()
}

