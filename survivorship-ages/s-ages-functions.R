#############################################################################
# 
# Code to calculate survivorship ages using data from the Human Mortality Database
#
# Reference: Mortality as a Function of Survival (Alvarez and Vaupel, 2023)
# https://doi.org/10.1215/00703370-10429097 
#
# Last updated: 05-08-2023
# Author: Jesus-Adrian Alvarez
#
# ############################################################################

path <- "" # update with path to files


# Function to interpolate death counts and exposures using splines
ageInterpolationSpline <- function(Dx, Nx, Age, startAge = 0, endAge = 110){
  
  dat <- data.frame(Dx, Nx, Age)
  
  splineDx <- interpSpline(Dx~Age, dat)
  splineNx <- interpSpline(Nx~Age, dat)
  
  Age  <- predict(splineDx, seq( startAge, endAge, 0.01 ))$x
  sDx  <- abs(predict(splineDx, seq( startAge, endAge, 0.01 ))$y)
  sNx  <- abs(predict(splineNx, seq( startAge, endAge, 0.01 ))$y)
  sMx <- abs(sDx / sNx)
  
  out  <- data.frame(Age, Dx = sDx, Nx = sNx, Mx = sMx)
  return(out)}

# Function to calculate survival, density, hazard and cumulative hazards in a quasi-continuous age dimension
calculateSurvival <- function(Age, hx){
  
  Hx <- cumtrapz(Age,hx)
  Sx <- exp(-Hx)
  Fx <- 1-Sx
  fx <- hx * Sx
  
  dat <- data.frame(Age,hx,Hx,Sx,Fx,fx)
  
  return(dat)}


# Function to calculate survivorship ages
calculateSurvivalAges <- function(Age,fx,Sx,hx){
  dat <- data.frame(Age,fx,Sx,hx)
  dat$Sx <- dat$Sx*100
  
  s0    <- max(dat$Age)
  s1    <- dat$Age[ceiling(round(dat$Sx,3)) == 01][1]
  s2    <- dat$Age[ceiling(round(dat$Sx,3)) == 02][1]
  s3    <- dat$Age[ceiling(round(dat$Sx,3)) == 03][1]
  s4    <- dat$Age[ceiling(round(dat$Sx,3)) == 04][1]
  s5    <- dat$Age[ceiling(round(dat$Sx,3)) == 05][1] 
  s6    <- dat$Age[ceiling(round(dat$Sx,3)) == 06][1]
  s7    <- dat$Age[ceiling(round(dat$Sx,3)) == 07][1]
  s8    <- dat$Age[ceiling(round(dat$Sx,3)) == 08][1]
  s9    <- dat$Age[ceiling(round(dat$Sx,3)) == 09][1]
  s10   <- dat$Age[ceiling(round(dat$Sx,3)) == 10][1]
  s11   <- dat$Age[ceiling(round(dat$Sx,3)) == 11][1] 
  s12   <- dat$Age[ceiling(round(dat$Sx,3)) == 12][1]
  s13   <- dat$Age[ceiling(round(dat$Sx,3)) == 13][1]
  s14   <- dat$Age[ceiling(round(dat$Sx,3)) == 14][1]
  s15   <- dat$Age[ceiling(round(dat$Sx,3)) == 15][1] 
  s16   <- dat$Age[ceiling(round(dat$Sx,3)) == 16][1]
  s17   <- dat$Age[ceiling(round(dat$Sx,3)) == 17][1]
  s18   <- dat$Age[ceiling(round(dat$Sx,3)) == 18][1]
  s19   <- dat$Age[ceiling(round(dat$Sx,3)) == 19][1]
  s20   <- dat$Age[ceiling(round(dat$Sx,3)) == 20][1]
  s21   <- dat$Age[ceiling(round(dat$Sx,3)) == 21][1]
  s22   <- dat$Age[ceiling(round(dat$Sx,3)) == 22][1]
  s23   <- dat$Age[ceiling(round(dat$Sx,3)) == 23][1]
  s24   <- dat$Age[ceiling(round(dat$Sx,3)) == 24][1]
  s25   <- dat$Age[ceiling(round(dat$Sx,3)) == 25][1]
  s26   <- dat$Age[ceiling(round(dat$Sx,3)) == 26][1]
  s27   <- dat$Age[ceiling(round(dat$Sx,3)) == 27][1]
  s28   <- dat$Age[ceiling(round(dat$Sx,3)) == 28][1]
  s29   <- dat$Age[ceiling(round(dat$Sx,3)) == 29][1]
  s30   <- dat$Age[ceiling(round(dat$Sx,3)) == 30][1]
  s31   <- dat$Age[ceiling(round(dat$Sx,3)) == 31][1]
  s32   <- dat$Age[ceiling(round(dat$Sx,3)) == 32][1]
  s33   <- dat$Age[ceiling(round(dat$Sx,3)) == 33][1]
  s34   <- dat$Age[ceiling(round(dat$Sx,3)) == 34][1]
  s35   <- dat$Age[ceiling(round(dat$Sx,3)) == 35][1]
  s36   <- dat$Age[ceiling(round(dat$Sx,3)) == 36][1]
  s37   <- dat$Age[ceiling(round(dat$Sx,3)) == 37][1]
  s38   <- dat$Age[ceiling(round(dat$Sx,3)) == 38][1]
  s39   <- dat$Age[ceiling(round(dat$Sx,3)) == 39][1]
  s40   <- dat$Age[ceiling(round(dat$Sx,3)) == 40][1]
  s41   <- dat$Age[ceiling(round(dat$Sx,3)) == 41][1]
  s42   <- dat$Age[ceiling(round(dat$Sx,3)) == 42][1]
  s43   <- dat$Age[ceiling(round(dat$Sx,3)) == 43][1]
  s44   <- dat$Age[ceiling(round(dat$Sx,3)) == 44][1]
  s45   <- dat$Age[ceiling(round(dat$Sx,3)) == 45][1]
  s46   <- dat$Age[ceiling(round(dat$Sx,3)) == 46][1]
  s47   <- dat$Age[ceiling(round(dat$Sx,3)) == 47][1]
  s48   <- dat$Age[ceiling(round(dat$Sx,3)) == 48][1]
  s49   <- dat$Age[ceiling(round(dat$Sx,3)) == 49][1]
  s50   <- dat$Age[ceiling(round(dat$Sx,3)) == 50][1]
  s51   <- dat$Age[ceiling(round(dat$Sx,3)) == 51][1]
  s52   <- dat$Age[ceiling(round(dat$Sx,3)) == 52][1]
  s53   <- dat$Age[ceiling(round(dat$Sx,3)) == 53][1]
  s54   <- dat$Age[ceiling(round(dat$Sx,3)) == 54][1]
  s55   <- dat$Age[ceiling(round(dat$Sx,3)) == 55][1]
  s56   <- dat$Age[ceiling(round(dat$Sx,3)) == 56][1]
  s57   <- dat$Age[ceiling(round(dat$Sx,3)) == 57][1]
  s58   <- dat$Age[ceiling(round(dat$Sx,3)) == 58][1]
  s59   <- dat$Age[ceiling(round(dat$Sx,3)) == 59][1]
  s60   <- dat$Age[ceiling(round(dat$Sx,3)) == 60][1]
  s61   <- dat$Age[ceiling(round(dat$Sx,3)) == 61][1]
  s62   <- dat$Age[ceiling(round(dat$Sx,3)) == 62][1]
  s63   <- dat$Age[ceiling(round(dat$Sx,3)) == 63][1]
  s64   <- dat$Age[ceiling(round(dat$Sx,3)) == 64][1]
  s65   <- dat$Age[ceiling(round(dat$Sx,3)) == 65][1]
  s66   <- dat$Age[ceiling(round(dat$Sx,3)) == 66][1]
  s67   <- dat$Age[ceiling(round(dat$Sx,3)) == 67][1]
  s68   <- dat$Age[ceiling(round(dat$Sx,3)) == 68][1]
  s69   <- dat$Age[ceiling(round(dat$Sx,3)) == 69][1]
  s70   <- dat$Age[ceiling(round(dat$Sx,3)) == 70][1]
  s71   <- dat$Age[ceiling(round(dat$Sx,3)) == 71][1]
  s72   <- dat$Age[ceiling(round(dat$Sx,3)) == 72][1]
  s73   <- dat$Age[ceiling(round(dat$Sx,3)) == 73][1]
  s74   <- dat$Age[ceiling(round(dat$Sx,3)) == 74][1]
  s75   <- dat$Age[ceiling(round(dat$Sx,3)) == 75][1]
  s76   <- dat$Age[ceiling(round(dat$Sx,3)) == 76][1]
  s77   <- dat$Age[ceiling(round(dat$Sx,3)) == 77][1]
  s78   <- dat$Age[ceiling(round(dat$Sx,3)) == 78][1]
  s79   <- dat$Age[ceiling(round(dat$Sx,3)) == 79][1]
  s80   <- dat$Age[ceiling(round(dat$Sx,3)) == 80][1]
  s81   <- dat$Age[ceiling(round(dat$Sx,3)) == 81][1]
  s82   <- dat$Age[ceiling(round(dat$Sx,3)) == 82][1]
  s83   <- dat$Age[ceiling(round(dat$Sx,3)) == 83][1]
  s84   <- dat$Age[ceiling(round(dat$Sx,3)) == 84][1]
  s85   <- dat$Age[ceiling(round(dat$Sx,3)) == 85][1]
  s86   <- dat$Age[ceiling(round(dat$Sx,3)) == 86][1]
  s87   <- dat$Age[ceiling(round(dat$Sx,3)) == 87][1]
  s88   <- dat$Age[ceiling(round(dat$Sx,3)) == 88][1]
  s89   <- dat$Age[ceiling(round(dat$Sx,3)) == 89][1]
  s90   <- dat$Age[ceiling(round(dat$Sx,3)) == 90][1]
  s91   <- dat$Age[ceiling(round(dat$Sx,3)) == 91][1]
  s92   <- dat$Age[ceiling(round(dat$Sx,3)) == 92][1]
  s93   <- dat$Age[ceiling(round(dat$Sx,3)) == 93][1]
  s94   <- dat$Age[ceiling(round(dat$Sx,3)) == 94][1]
  s95   <- dat$Age[ceiling(round(dat$Sx,3)) == 95][1]
  s96   <- dat$Age[ceiling(round(dat$Sx,3)) == 96][1]
  s97   <- dat$Age[ceiling(round(dat$Sx,3)) == 97][1]
  s98   <- dat$Age[ceiling(round(dat$Sx,3)) == 98][1]
  s99   <- dat$Age[ceiling(round(dat$Sx,3)) == 99][1]
  s100  <- dat$Age[ceiling(round(dat$Sx,3)) == 100][1]
  
  out <- data.frame(s0 ,
                    s1 ,
                    s2 ,
                    s3 ,
                    s4 ,
                    s5 ,
                    s6 ,
                    s7 ,
                    s8 ,
                    s9 ,
                    s10,
                    s11,
                    s12,
                    s13,
                    s14,
                    s15,
                    s16,
                    s17,
                    s18,
                    s19,
                    s20,
                    s21,
                    s22,
                    s23,
                    s24,
                    s25,
                    s26,
                    s27,
                    s28,
                    s29,
                    s30,
                    s31,
                    s32,
                    s33,
                    s34,
                    s35,
                    s36,
                    s37,
                    s38,
                    s39,
                    s40,
                    s41,
                    s42,
                    s43,
                    s44,
                    s45,
                    s46,
                    s47,
                    s48,
                    s49,
                    s50,
                    s51,
                    s52,
                    s53,
                    s54,
                    s55,
                    s56,
                    s57,
                    s58,
                    s59,
                    s60,
                    s61,
                    s62,
                    s63,
                    s64,
                    s65,
                    s66,
                    s67,
                    s68,
                    s69,
                    s70,
                    s71,
                    s72,
                    s73,
                    s74,
                    s75,
                    s76,
                    s77,
                    s78,
                    s79,
                    s80,
                    s81,
                    s82,
                    s83,
                    s84,
                    s85,
                    s86,
                    s87,
                    s88,
                    s89,
                    s90,
                    s91,
                    s92,
                    s93,
                    s94,
                    s95,
                    s96,
                    s97,
                    s98,
                    s99,
                    s100)
  
  out <- gather(out,s, Age)
  out$s <- as.integer(substr(out$s,2,4))
  
  return(out)}



# Function to read all txt files in the path
getHMDdata <- function(name, path){
  p       <- paste0(path,"",name)
  dat_raw     <- read.table(p, header = TRUE, skip = 2, na.strings = ".", as.is = TRUE)
  dat <- HMDparse(dat_raw, p)
  Country <- strsplit(name, split=".", fixed = T)[[1]][1]
  Type_x  <- strsplit(name, split=".", fixed = T)[[1]][2]
  Type    <- strsplit(Type_x, split="_")[[1]][1]
  out     <- data.frame(Country, Type, dat)
  return(out)}


# Parse data
HMDparse<- function(DF, filepath){
  age2int<- function(Age){
    as.integer(gsub("[-+]", "", unlist(lapply(strsplit(as.character(Age), split = "-"), "[[", 1))))
  }
  
  if (any(grepl("age", tolower(colnames(DF))))) {
    DF$Age <- age2int(DF$Age)
  }
  if (grepl("pop", tolower(filepath))) {
    all.years <- sort(unique(age2int(DF$Year)))
    Pluses <- grepl(pattern = "\\+", DF$Year)
    Minuses <- grepl(pattern = "\\-", DF$Year)
    Jan1i <- DF$Year %in% as.character(all.years[-length(all.years)]) | 
      Pluses
    Dec31i <- DF$Year %in% as.character(all.years[-1]) | 
      Minuses
    Jan1 <- DF[Jan1i, ]
    Dec31 <- DF[Dec31i, ]
    Jan1$Year <- age2int(Jan1$Year)
    Dec31$Year <- age2int(Dec31$Year)
    cols1 <- match(c("female", "male", "total"), tolower(colnames(Jan1)))
    cols2 <- match(c("female", "male", "total"), tolower(colnames(Dec31)))
    colnames(Jan1)[cols1] <- paste0(colnames(Jan1)[cols1], 
                                    1)
    colnames(Dec31)[cols2] <- paste0(colnames(Dec31)[cols2], 
                                     2)
    DF <- cbind(Jan1, Dec31[, cols2])
    orgi <- grepl("male", tolower(colnames(DF))) | grepl("total", 
                                                         tolower(colnames(DF)))
    DF <- cbind(DF[, !orgi], DF[, orgi])
  }
  if (any(grepl("year", tolower(colnames(DF))))) {
    DF$Year <- age2int(DF$Year)
  }
  if (any(grepl("cohort", tolower(colnames(DF))))) {
    DF$Cohort <- age2int(DF$Cohort)
  }
  invisible(DF)
}


library(tidyverse)
library(splines)
library(data.table)
library(pracma)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#source("SurvivorshipAges_Functions.R")


# Read and clean HMD data -------------------------------------------------


# Read all the names of th e
names    <- list.files(path = path, pattern="*.txt")
# Put everything in a list
HMDlist <- lapply(names,getHMDdata, path=path)
HMDdata   <-do.call(rbind.data.frame, HMDlist)
Dx <- subset(HMDdata, Type =="Deaths")
Nx <- subset(HMDdata, Type == "Exposures")
Dx <- Dx[,c("Country","Year","Age","Female","Male","Total")]
Nx <- Nx[,c("Country","Year","Age","Female","Male","Total")]
names(Dx) <- c("Country","Year","Age","Dx.f", "Dx.m","Dx.t" )
names(Nx) <- c("Country","Year","Age","Nx.f", "Nx.m","Nx.t" )

Mx <- merge(Dx,Nx, by =c("Country","Year", "Age"))
Mx <- data.frame(lapply(Mx, as.character), stringsAsFactors=FALSE)
Mx$Age[Mx$Age == "110+"] <-110
Mx[, c(2:9)] <- sapply(Mx[, c(2:9)], as.numeric)
Mx           <- arrange(Mx, Year, Age)
Mx$Mx.f <- Mx$Dx.f / Mx$Nx.f
Mx$Mx.m <- Mx$Dx.m / Mx$Nx.m
Mx$Mx.t <- Mx$Dx.t / Mx$Nx.t
Mx[is.na(Mx)]<-0

# Long format
Mxf <- Mx[,c("Country","Year", "Age","Dx.f","Nx.f","Mx.f")]
Mxm <- Mx[,c("Country","Year", "Age","Dx.m","Nx.m","Mx.m")]
Mxt <- Mx[,c("Country","Year", "Age","Dx.t","Nx.t","Mx.t")]

names(Mxf) <- c("Country","Year","Age","Dx","Nx","Mx")
names(Mxm) <- c("Country","Year","Age","Dx","Nx","Mx")
names(Mxt) <- c("Country","Year","Age","Dx","Nx","Mx")

Mxf$Sex <- "Females"
Mxm$Sex <- "Males"
Mxt$Sex <- "Total"

data <- data.table(rbind(Mxf,Mxm,Mxt))

# Smooth data to the finest age interval ----------------------------------

smoothData<- data[,ageInterpolationSpline(Dx = Dx, Nx = Nx, Age= Age), by =list(Country,  Sex, Year)] 

# Calculate survivorship ages (s-ages) ------------------------------------

survival <- smoothData[, calculateSurvival(Age=Age,hx=Mx), by = list(Country,  Sex, Year)]
survivalAges <- survival[, calculateSurvivalAges(Age,fx,Sx,hx), by = list(Country,  Sex, Year)]
survivalAges$Year <- as.integer(survivalAges$Year)

# Charts ------------------------------------------------------------------

brks <- seq(1900,2020,by = 20)
labs <- c("1900","'20","'40","'60","'80","2000", "'20")

# Survivorship ages. Figure 2 of Alvarez and Vaupel (2023)
ggplot(subset(survivalAges,
              s>=1 ))+
  geom_line(aes(Year, Age, group = s), colour = "grey50",alpha=0.7, size = 0.3)+
  
  geom_line(data=subset(survivalAges,s  %in% 100 ),
            aes(Year,  Age, group = s)
            ,colour= "black", size =0.3)+
  
  
  geom_line(data=subset(survivalAges, s  %in% seq(10,90, by =10)),
            aes(Year,  Age, group =s)
            ,colour= "firebrick3", size =0.3)+
  scale_color_viridis_c(direction = -1,option = "D",
                        begin = 0.5,end = 1,limits = c(0,110))+
  facet_grid(Country~Sex)+
  scale_y_continuous(breaks = seq(0,120,by = 10), expand = c(0,0))+
  scale_x_continuous(breaks = brks,labels = labs, expand = c(0,0))+
  theme_classic()+
  coord_cartesian(ylim = c(0,110), xlim=c(1900,2022))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background =element_blank(),
        strip.text = element_text(size=12, face = "bold"),
        text = element_text(size = 12,  colour = "black"),
        aspect.ratio = 1.5,
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(vjust=1),
        axis.title.y = element_text(vjust=2),
        axis.title = element_text(face = "bold"),
        panel.spacing = unit(2,"lines"))




