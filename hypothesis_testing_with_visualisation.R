#############################################################
############# HYPOTHESIS TESTING ############################
#############################################################

#The avarage IQ of the adult population is 100 with a standart deviation of 15. A researcher beleives this value has changed. The researcher decided to test
#the IQ of 75 random adults. The average IQ of the sample is 105. Is there enough evidence to suggest the average IQ has changed?


####################### First step: state the null and alernative hypothesis############################################

#H0(null hypothesis): iqmean= 100
#Ha(alternative hypothesis): iqmean != 100
#because Ha stataes "not equal" we need to performa a two tailed test (check both is it has increased or decreased)
n <- 75
sPopulation <- 15
meanPopulation <- 100
meanSample <- 105


####################### Second step: choose the level of significance (a) #############################################

#a is the aeria under the curve in each tail where if ou result lies the H0 will be rejected (rejectipon region), in this case this is not given to use.
#We will use a=0.05, so 0.025 for each tail
a <- 0.025


####################### Third step: Find critical value ###############################################################

#critical value is the point (z value) that separates the tails as defined from a to the main curve
#the standart deviation of the population is given so we will use a  z test

criticalRight <- qnorm(a,lower.tail=FALSE)
criticalleft <-qnorm(a)
criticalleft

####################### Four step: Find test statistic ###############################################################

#a z test statistic in our case
zTestStat <- (meanSample-meanPopulation )/(sPopulation/sqrt(n))
zTestStat
####################### Five step: Find test statistic ###############################################################

# zTest < criticalRight and as we can also see from our graph the test statistic lies on the rejection aeria
#and so we will reject the H0 and accept the Ha

x <- seq(-5,5,length=100)*sPopulation + meanPopulation
hx <- dnorm(x,meanPopulation,sPopulation)

plot(x, hx, type="n", xlab="", ylab="",
     axes=FALSE)

i <- x >= 80 & x <= 120
lines(x, hx)

abline(v=((criticalRight * sPopulation)+meanPopulation), col="red")
abline(v=(meanPopulation-(criticalRight * sPopulation)), col="red")
abline(v=((zTestStat*sPopulation)+meanPopulation), col="blue")









