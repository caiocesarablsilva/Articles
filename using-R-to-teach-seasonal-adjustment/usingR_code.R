# Using R to teach seasonal adjustment

# {Abstract:} This article shows, using R software, how to seasonally adjust a time series using the X-13-ARIMA-SEATS
# program and the seasonal package developed by Christoph Sax. In addition to presenting step-by-step seasonal
# adjustment, the article also explores how to analyze the program output and how to forecast the original and
# seasonally adjusted time series. A case study was proposed using the Brazilian industrial production. 
# It was verified that the effect of Carnival, Easter and working days improved the seasonal adjustment when 
# treated by the model.


## ------------------- 3.1	Preparing the working environment ---------------

# 1. Set the working directory

## this is justa an example
setwd("C:/work")

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
usingR_url <- getURL("https://raw.githubusercontent.com/pedrocostaferreira/usingRtoteachseasonaladjustment/master/pimpf.csv")
pim <- read.csv2(text = usingR_url)
pim <- ts(pim, start = c(2002,1), freq = 12)
pim


## ------------------- 3.2	Graphical Analysis ---------------

# Figura 2
plot(pim)
abline(h = seq(70,110,5), v = seq(2002,2016,1), lty = 3, col = "darkgrey")

# Figura 3
monthplot(pim, labels = month.abb, lty.base = 2)
legend("topleft", legend = c("pim/month", "average/month"), 
        cex = 0.8, lty = c(1,2), bty = "n")


## ------------------- 3.3	Implementation of X13-ARIMA-SEATS in automatic mode ---------------

ajuste <- seas(x = pim)
  

## ------------------- 3.4	Verify whether the previous steps were executed correctly ---------------





