setwd("~/BPS-PortlandWageByIndustry")
library(readxl)
library(ggplot2)
library(tidyr) # For gather() function.

# Import the data with Portland MSA's 2016 wage distribution for each occupation. The data file used was the one provided by Nick in his email dated July 19, 2017. 

wages_by_occ <- read.csv("~/BPS-PortlandWageByIndustry/PDXwages_by_occ.csv")
wages_by_occ$median <- as.numeric(gsub(",", "", wages_by_occ$A_MEDIAN))

# Set the cut-off points for the income groups. These are based on 25th percentile, 50th percentile and 75th percentile for all wages in Portland MSA.

CutoffLow <- 27720
CutoffLowerMiddle <- 41370
CutoffUpperMiddle <- 65800

# For each industry NAICS code "naics" get the occupation distribution as a data frame with occupation code "occ" and percentage employed "percent".

OccupationList <- read.csv("OccupationCodes - Sheet1.csv") # This file is based on the table here: https://www.bls.gov/oes/current/oes_38900.htm
names(OccupationList)[1] <- "occ"
#OccupationList <- OccupationList[OccupationList$Level == "detail", ]

# For the year 2016, what is the median income for each occupation?
for(i in 1:length(OccupationList$Annual.mean.wage)){
  OccupationList$median[i] <- wages_by_occ$median[as.character(wages_by_occ$OCC_CODE)==OccupationList$occ[i] & wages_by_occ$YEAR==2016]  
}


### Check the distribution of median annual wages for different occupations relative to the three cut-off points chosen. 
### The graph shows that very few occupations have median income below the 25th wage percentile.  
ggplot(OccupationList, aes(OccupationList$median)) + geom_histogram(binwidth = 5000, color="black") + geom_vline(xintercept = CutoffLow, color = "red") + geom_vline(xintercept = CutoffLowerMiddle, color = "red") + geom_vline(xintercept = CutoffUpperMiddle, color = "red")

## Create a new variable "inclevel" for each occupation that translates the median income of that occupation into one of four categories: Low, Lower Middle, Upper Middle and High.
OccupationList$inclevel <- NA
OccupationList$inclevel[!is.na(OccupationList$median) & OccupationList$median < CutoffLow] <- "Low"
OccupationList$inclevel[!is.na(OccupationList$median) & OccupationList$median >= CutoffLow & OccupationList$median < CutoffLowerMiddle] <- "Lower Middle"
OccupationList$inclevel[!is.na(OccupationList$median) & OccupationList$median >= CutoffLowerMiddle  & OccupationList$median < CutoffUpperMiddle] <- "Upper Middle"
OccupationList$inclevel[!is.na(OccupationList$median) & OccupationList$median >= CutoffUpperMiddle] <- "High"

OccupationList$inclevel <- ordered(OccupationList$inclevel, levels=c("High", "Upper Middle", "Lower Middle", "Low"))
summary(OccupationList$inclevel)
OccupationList$Occupation.title[is.na(OccupationList$median)] ### This gives us the list of occupations for which we do not have median annual wage to determine their wage category.

### Below we manually assign "inclevel" values for those occupations that we do not have annual median income. (Based on info that Nick sent by email on July 21, 2017)
OccupationList$inclevel[OccupationList$Occupation.title == "Directors, Religious Activities and Education"] <- "Lower Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Riggers"] <- "Upper Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Set and Exhibit Designers"] <- "Upper Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Actors"] <- "Lower Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Athletes and Sports Competitors"] <- "Upper Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Musicians and Singers"] <- "Upper Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Entertainers and Performers, Sports and Related Workers, All Other"] <- "Low"
OccupationList$inclevel[OccupationList$Occupation.title == "Anesthesiologists"] <- "High"
OccupationList$inclevel[OccupationList$Occupation.title == "Obstetricians and Gynecologists"] <- "High"
OccupationList$inclevel[OccupationList$Occupation.title == "Surgeons"] <- "High"
OccupationList$inclevel[OccupationList$Occupation.title == "Hearing Aid Specialists"] <- "Upper Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Proofreaders and Copy Markers"] <- "Lower Middle"
OccupationList$inclevel[OccupationList$Occupation.title == "Airline Pilots, Copilots, and Flight Engineers"] <- "High"

##### Get the list of NAICS codes and exclude the industry summary codes from the table here: https://www.bls.gov/emp/ep_table_109.htm
##### It is important to keep in mind that this is national level data for each industry. May or may not reflect the industry-occupation matrix for Portland. 
##### But we are making an assumption here.

IndustryList <- read.csv("IndustryList.csv")
list1 <- (IndustryList$NAICS)

##### The function below is to download all the industry-occupation matrices for a given list of industry NAICS codes from the BLS website and save them in a "Temp" folder.

downloadIndOccMatrix <- function(l) {
  for(naics in l) {
    dest <- paste0("Temp/ind_", as.character(naics), ".xlsx", sep = "")
    url <- paste("https://www.bls.gov/emp/ind-occ-matrix/ind_xlsx/ind_", as.character(naics), ".xlsx", sep = "")
    download.file(url, destfile = dest, mode="wb")
  }
}
#downloadIndOccMatrix(list1)   ## Currently, all Excel files are downloaded and part of this git repository. 
                              ## Therefore this line is commented out. To update these excel files from the 
                              ## BLS site, un-comment this line.
                              ## It takes about five minutes to download all the Excel files. 


## The "getIndustryOccMatrix()" function below reads the Excel files downloaded above and returns a dataframe with three variables: 
## - the occupation code, the percentage of employees in the industry who are engaged in that occupation, and the wage level of that occupation.

getIndustryOccMatrix <- function(naics) {
  dest <- paste0("Temp/ind_", as.character(naics), ".xlsx", sep = "")
  temp <- read_excel(dest, col_names = FALSE, na = "NA", skip = 5) 
  temp <- temp[!is.na(temp[[4]]), ]      
  occ= c()
  percent = c()
  inclevel = c()
  for(i in 1:length(OccupationList$occ)) {
    if(length(temp[[2]][temp[[2]]==OccupationList$occ[i]])>0 ) {
      occ <- c(occ, temp[[2]][temp[[2]]==OccupationList$occ[i]])
      percent <- c(percent, (temp[[4]][temp[[2]]==OccupationList$occ[i]]*100/temp[[4]][1]))
      inclevel <- c(inclevel, OccupationList$inclevel[OccupationList$occ==OccupationList$occ[i]])
    }
  }
  ind_matrix <- data.frame(occ=occ, percent=percent, inclevel = inclevel)
  ind_matrix <- ind_matrix[ind_matrix$occ %in% grep("0$", ind_matrix$occ, perl = TRUE, value = TRUE), ]
  ind_matrix <- ind_matrix[-1, ]
  return(ind_matrix)
}


# Calculate for each industry the proportion of those in high, upper_middle, lower_middle and low.

industry_prop_incomegroup <- function(naics) {
  temp <- getIndustryOccMatrix(naics)
  high <- sum(temp$percent[temp$inclevel==1])/100
  uppermiddle <- sum(temp$percent[temp$inclevel==2])/100
  lowermiddle <- sum(temp$percent[temp$inclevel==3])/100
  low <- sum(temp$percent[temp$inclevel==4])/100
  return(c(high, uppermiddle, lowermiddle, low))
}



#### Create the "output" data frame with the proportions of the 4 income groups for each industry.

output <- data.frame(naics = list1,
                     high = NA,
                     uppermiddle = NA,
                     lowermiddle = NA,
                     low = NA)

for(i in 1:length(list1)) {
  proportions <- industry_prop_incomegroup(list1[i])
  output$naics[i] <- list1[i] 
  output$high[i] <- round(proportions[1], digits = 3)
  output$uppermiddle[i] <- round(proportions[2], digits = 3)
  output$lowermiddle[i] <- round(proportions[3], digits = 3)
  output$low[i] <- round(proportions[4], digits = 3)
}

#### Save the "output" data frame as a file.
write.csv(output, file = "WageGroupProportions_byIndustry.csv")

############# DIAGNOSTICS TO IDENTIFY INDUSTRIES FOR WHICH PROPORTIONS DON'T ADD UP CORRECTLY #####################################

output1 <- output
output1$tot <- output1$high + output1$uppermiddle + output1$lowermiddle + output1$low
ggplot(output1, aes(output1$tot)) + geom_histogram() # Check the distribution of the total proportions of the 4 income groups for each industry. We see that most of the totals add up to 1.0 with some rounding errors. 
output1$naics[output1$tot<0.95]    # List of 10 NAICS codes for which the total of the proportions of the 4 income groups add up to less than 0.75. I will examine manually for each of 


# 525900: The total adds up to less than 0.25 because this industry has only 2.5 total employees. So the percentages of employees in different occupations round up to 0.0 for most occupations.
# 525100: The total adds up to less than 0.5 because this industry has 1.3 total employees. Same reason as above.
# 523200: Same reason as above. Only 7 employees.
# The remaining are also the same reasons.



################ Other analysis ##################################3

# For each occupation we can see how the 10th, 25th, 50th, 75th and 90th wage percentile has changed over time.
for(i in 8:27) {
  wages_by_occ[[i]] <- as.numeric(gsub(",", "", wages_by_occ[[i]]))
}

x=c(0.10, 0.25, 0.5, 0.75, 0.90)
temp <- wages_by_occ[wages_by_occ$OCC_CODE=="00-0000", ]
temp_long <- gather(temp, percentile, wage, A_PCT10:A_PCT90, factor_key=TRUE)
temp_long$percentile <- factor(temp_long$percentile, labels = c("0.1", "0.25", "0.50", "0.75", "0.9"), levels = c("A_PCT10", "A_PCT25", "A_MEDIAN", "A_PCT75", "A_PCT90"))
temp_long$percentile <- as.numeric(as.character(temp_long$percentile))
ggplot(temp_long, aes(percentile, wage)) + geom_point(aes(color = YEAR)) #+ geom_smooth(method = "loess", span=0.98)

x=c(0.10, 0.25, 0.5, 0.75, 0.90)
temp <- wages_by_occ[wages_by_occ$OCC_CODE=="11-0000", ]
temp_long <- gather(temp, percentile, wage, A_PCT10:A_PCT90, factor_key=TRUE)
temp_long$percentile <- factor(temp_long$percentile, labels = c("0.1", "0.25", "0.50", "0.75", "0.9"), levels = c("A_PCT10", "A_PCT25", "A_MEDIAN", "A_PCT75", "A_PCT90"))
temp_long$percentile <- as.numeric(as.character(temp_long$percentile))
ggplot(temp_long, aes(percentile, wage)) + geom_point(aes(color = YEAR)) #+ geom_smooth(method = "loess", span=0.98)

x=c(0.10, 0.25, 0.5, 0.75, 0.90)
y=c(wages_by_occ$A_PCT10[wages_by_occ$OCC_CODE=="00-0000" & wages_by_occ$YEAR == 2016], 
    wages_by_occ$A_PCT25[wages_by_occ$OCC_CODE=="00-0000" & wages_by_occ$YEAR == 2016], 
    wages_by_occ$A_MEDIAN[wages_by_occ$OCC_CODE=="00-0000" & wages_by_occ$YEAR == 2016], 
    wages_by_occ$A_PCT75[wages_by_occ$OCC_CODE=="00-0000" & wages_by_occ$YEAR == 2016], 
    wages_by_occ$A_PCT90[wages_by_occ$OCC_CODE=="00-0000" & wages_by_occ$YEAR == 2016])
d <- data.frame(x, y)
ggplot(d, aes(x, y)) + geom_point() + geom_smooth(method = "loess", span=0.98)