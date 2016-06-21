# Using R to teach seasonal adjustment

# {Abstract:} This article shows, using R software, how to seasonally adjust a time series using the X-13-ARIMA-SEATS
# program and the seasonal package developed by Christoph Sax. In addition to presenting step-by-step seasonal
# adjustment, the article also explores how to analyze the program output and how to forecast the original and
# seasonally adjusted time series. A case study was proposed using the Brazilian industrial production. 
# It was verified that the effect of Carnival, Easter and working days improved the seasonal adjustment when 
# treated by the model.

# Authors:
# Pedro Costa Ferreira
# Daiane Marcolino Mattos

## ------------------- 3.1	Preparing the working environment ---------------

# 1. Set the working directory

## this is justa an example
setwd("C:/work")
setwd("C:\\Users\\pedro.guilherme\\Dropbox\\01 Articles\\00 working\\02 Using R to teach seasonal adjustment\\usingRtoteachseasonaladjustment")

# 2. Download the X-13ARIMA-SEATS executable file

download.file("https://www.census.gov/ts/x13as/pc/x13as_V1.1_B19.zip",
              destfile = "./x13.zip")
unzip("x13.zip")

# 3. Inform the location of the executable file downloaded in (2)

local <- paste0(getwd(), "/x13as")
Sys.setenv(X13_PATH = local)

# 4. Install and load the seasonal package

install.packages("seasonal")
library(seasonal)


# 5. Verify whether the previous steps were executed correctly

checkX13()

# 6. Read the interest time series

install.packages("RCurl")
library(RCurl)
usingR_url <- getURL("https://raw.githubusercontent.com/pedrocostaferreira/Articles/master/using-R-to-teach-seasonal-adjustment/IPI.csv")
ipi <- read.csv2(text = usingR_url)
ipi <- ts(ipi, start = c(2002,1), freq = 12)
ipi


## ------------------- 3.2	Graphical Analysis ---------------

# Figura 2
plot(ipi)
abline(h = seq(70,110,5), v = seq(2002,2016,1), lty = 3, col = "darkgrey")

# Figura 3
monthplot(ipi, labels = month.abb, lty.base = 2)
legend("topleft", legend = c("ipi/month", "average/month"), 
        cex = 0.8, lty = c(1,2), bty = "n")


## ------------------- 3.3	Implementation of X13-ARIMA-SEATS in automatic mode ---------------

seas_adjust <- seas(x = ipi)
  

## ------------------- 3.4	Verify whether the previous steps were executed correctly ---------------

# 1. Verify the Seasonality QS test

qs(seas_adjust)

# 2. Pre-adjustment diagnosis and SARIMA model

summary(seas_adjust)


# 3. Verify the seasonality evidence or working days effect graphically;

# spectrum series
spec_orig <- series(seas_adjust,"sp0")
spec_sa <- series(seas_adjust,"s1s")
spec_resid <- series(seas_adjust,"spr")
spec_irreg <- series(seas_adjust,"s2s")

# frequency
freq <- c((1:6)/12, 0.3482, 0.4326) 

# spectral plot: original series 
plot(spec_orig, type = "l")
abline(v = freq, col = c(rep("red",6),"blue","blue"),
      lty = c(rep(2,6),3,3))
legend("topleft", legend = c("Orig", "S. Effects", "TD. Effects"),
      cex = 0.6, lty = 1:3, bty = "n", col = c(1,2,4))


# 4. Seasonal adjustment stability

sliding <- series(seas_adjust, "sfs")
sum(sliding[,"Max_._DIFF"] > 3, na.rm = T) /
   length(na.omit(sliding[,"Max_._DIFF"])) * 100


## ------------------- 3.5	Automatic adjustment correction ---------------


# building working days
usingR_url_wd <- getURL("https://raw.githubusercontent.com/pedrocostaferreira/Articles/master/using-R-to-teach-seasonal-adjustment/work_days.csv")
wd <- read.csv2(text = usingR_url_wd)
head(wd)

Workdays_ok <- ts(wd[,5], start = c(1970,1), freq = 12)

# building moving holidays (mh)

usingR_url_mh <- getURL("https://raw.githubusercontent.com/pedrocostaferreira/Articles/master/using-R-to-teach-seasonal-adjustment/moving_holidays.csv")
mh <- read.csv2(text = usingR_url_mh)
head(mh)


# Easter
dates.easter <- as.Date(as.character(mh$Easter),"%d/%m/%Y")
easter <- genhol(dates.easter, start = -8, frequency = 12)

# Carnival
dates.carnival <- as.Date(as.character(mh$Carnival),"%d/%m/%Y")
carnival <- genhol(dates.carnival, start = -3, end = 1, frequency = 12)

regs <- na.omit(cbind(Workdays_ok, easter, carnival))
head(regs)

seas_adjust2 <- seas(x = ipi, xreg = regs, regression.aictest = NULL, regression.usertype = c("td", "easter", "holiday"))

qs(seas_adjust2)
summary(seas_adjust2)


# figure 5
monthplot(seas_adjust2, col.base = 1)
legend("topleft", legend = c("Irregular", "Seasonal", "Seasonal Average"),
      col = c(4,2,1), lwd = c(1,2,2), lty = 1, bty = "n", cex = 0.6)

# figure 6
plot(seas_adjust2)
grid()
legend("topleft", legend = c("Original", "Adjusted"),
      col = c(1,2), lwd = c(1,2), lty = 1, bty = "n", cex = 0.6)

out(seas_adjust2)

## ----------------------- 3.6	Forecast --------------------

forecast <- window(series(seas_adjust2, "fct"), end = c(2016,12), freq = 12)
ts.plot(ipi, forecast, col = c(1,1,4,4), lwd = c(1,2,1,1))
grid()
legend("topleft", legend = c("IPI", "Forecasted IPI", "CI 95%"),
       lty = 1, col = c(1,1,4), lwd = c(1,2,1,1), bty = "n", cex = 0.7)


seas_adjust3 <- seas(ipi, xreg = regs, regression.aictest = NULL, 
                  regression.usertype = c("td","easter", "holiday"),
                  seats.appendfcst = "yes")

# forecast 
fcst <- window(series(seas_adjust3, "s11"), start = c(2015,10), end = c(2016,12), 
                  freq = 12)

# adjusted
fit <- window(series(seas_adjust3, "s11"), end = c(2015,9), freq = 12)

# plot
ts.plot(ipi, fit, fcst, col = c(1,2,4), lwd = c(1,2,2))
grid()
legend("topleft", legend = c("IPI", "Adjusted PIM", "Adjusted PIM forecast"),
                   lty = 1, col = c(1,2,4), lwd = c(1,2,2), cex = 0.7, bty = "n")



