setwd("E:\\LoDo Thesis\\GIS\\ArcGIS Stuff")
res <- read.csv("E:\\LoDo Thesis\\GIS\\ArcGIS Stuff\\new_builds_res_csv.csv")
com <- read.csv("E:\\LoDo Thesis\\GIS\\ArcGIS Stuff\\new_builds_com_csv.csv")
attach(res)
attach(com)
options(scipen = 999) #getting rid of scientific notation

# histograms
hist(res$RES_ORIG_YEAR_BUILT)
hist(res$RES_ABOVE_GRADE_AREA)
hist(com$COM_ORIG_YEAR_BUILT)
hist(com$COM_GROSS_AREA)

# Residential year vs sqft
plot(res$RES_ABOVE_GRADE_AREA ~ res$RES_ORIG_YEAR_BUILT, cex=.7, pch=19, xlab='Year Built',
     ylab='Square Footage',main="Residential New-Builds in LoDo: Year Built and Size of Property")
res_yr_size <- lm(res$RES_ABOVE_GRADE_AREA~res$RES_ORIG_YEAR_BUILT)
summary(res_yr_size)
abline(res_yr_size, col='red')

# Commercial year vs sqft
plot(com$COM_GROSS_AREA ~ com$COM_ORIG_YEAR_BUILT, cex=.7, pch=19, xlab='Year Built',
     ylab='Square Footage',main="Commercial New-Builds in LoDo: Year Built and Size of Property")
com_yr_size <- lm(com$COM_GROSS_AREA ~ com$COM_ORIG_YEAR_BUILT)
summary(com_yr_size)
abline(com_yr_size, col='red')

# Commercial year vs appraised value
plot(com$APPRAISED_TOTAL_VALUE ~ com$COM_ORIG_YEAR_BUILT, cex=.7, pch=19, xlab='Year Built',
     ylab='Appraised Value ($)',main="Commercial New-Builds in LoDo: Year Built and Property Value")
com_yr_value <- lm(com$APPRAISED_TOTAL_VALUE ~ com$COM_ORIG_YEAR_BUILT)
summary(com_yr_value)
abline(com_yr_value, col='red')

# Residential year vs appraised value
plot(res$APPRAISED_TOTAL_VALUE ~ res$RES_ORIG_YEAR_BUILT, cex=.7, pch=19, xlab='Year Built',
     ylab='Appraised Value ($)',main="Residential New-Builds in LoDo: Year Built and Property Value")
res_yr_value <- lm(res$APPRAISED_TOTAL_VALUE ~ res$RES_ORIG_YEAR_BUILT)
summary(res_yr_value)
abline(res_yr_value, col='red')
