# Open libraries
library(tidyverse)
library(scales)
library(viridis)
library(RColorBrewer)
library(data.table)

# Data source:
# https://www.mortality.org/
# Choose countries, then go to Cohort data > Death Rates > 1x10
# Download and replace this with path to folder
data_folder <- "/Users/saloni/Documents/Github/misc/"
# set filename to cMx_1x10_(name of country).txt

countries <- c("Italy")
sexes <- c("Females", "Males")
lifetable_list <- list()  # Renaming to lifetable_list for clarity

# Import data
for (sex in sexes) {
  for (country in countries) {
    
    # Create a unique key for country and sex
    key <- paste(country, sex, sep = "_")
    
    # Import and rename cols
    data <- read_table(paste0(data_folder, "lifetable_1x1_", country, "_", sex, ".txt"), skip=2, na = ".")
    colnames(data) <- c("Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
    
    data <- data %>%
      mutate(Country = country, Sex = sex, Type = "Period")
    
    # Assign to the list using the unique key
    lifetable_list[[key]] <- data
  }
}

# Combine all data frames in the list into one data frame
lifetable <- do.call(rbind, lifetable_list)
lifetable$Sex <- as.factor(lifetable$Sex)
summary(lifetable)

# Functions needed for 'Life expectancy and equality: a long run relationship'

# Some useful fucntions: for ax and life table
AKm02a0 <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

LifeExpectancy <- compiler::cmpfun(function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[1]
})

LifeTable      <- function(mx,sex = "f"){
  mx <- as.matrix(mx)
  i.openage <- nrow(mx)
  ax        <- mx * 0 + .5
  ax[1, ]   <- AKm02a0(m0 = mx[1, ], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)        
  qx[i.openage, ]       <- ifelse(is.na(qx[i.openage, ]), NA, 1)
  ax[i.openage, ]       <- 1 / mx[i.openage, ]                   
  px 				      <- 1 - qx 																				
  px[is.nan(px)]  <- 0 
  lx 			        <- apply(px, 2, function(px., RADIX, OPENAGE){ 		
    if (all(is.na(px.))) {
      px.
    } else {
      c(RADIX, RADIX * cumprod(px.[1:OPENAGE]))
    }
  }, RADIX = 1, OPENAGE = i.openage - 1
  )
  rownames(lx)    <- 0:(i.openage - 1) 
  dx 				      <- lx * qx 																				
  Lx 				      <- lx - (1 - ax) * dx 														
  Lx[i.openage, ]	<- lx[i.openage, ] * ax[i.openage, ]
  Tx 				      <- apply(Lx, 2, function(Lx., i.openage, OPENAGE){
    c(rev(cumsum(rev(Lx.[1:OPENAGE]))),0) + Lx.[i.openage]	
  }, OPENAGE = i.openage - 1, i.openage = i.openage
  )
  rownames(Tx)    <- rownames(lx)
  ex 				      <- Tx / lx 	                              
  list(e0=ex[1,],ex=ex,lx=lx,mx=mx)
}



h.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  l <- length(ex)
  v <- (sum(dx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  k <- v/ex[1]
  eq <- -log(k)
  return(eq)
}

my.cv.frommx <- function(mx,sex){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  age           <- 0:(i.openage-1)
  vx <- sum(dx*(age+ax-ex[1L])^2)
  cv <- sqrt(vx)/ex[1L]
  cv.inv <- -log(cv)
  cv.inv
}

log.G.frommx           <- function(mx,sex="f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  age           <- 0:(i.openage-1) + ax
  e             <- rep(1, length(age))
  D             <- outer(dx, dx)
  X_            <- abs(e%*%t(age) - age%*%t(e))
  G             <- sum(D*X_)/(2*ex[1L])
  G
}


Results <- lifetable %>%
  group_by(Country, Sex, Year) %>%
  summarise(
    h = h.frommx(mx = mx, sex = Sex),
    v = my.cv.frommx(mx = mx, sex = Sex),
    G = log.G.frommx(mx = mx, sex = Sex),
    eo = first(ex)
  ) %>%
  ungroup() %>%
  distinct()


