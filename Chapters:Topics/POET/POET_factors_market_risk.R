library(POET)
library(reshape2)

#Data - p = 50 stocks total. 10 from 5 sectors each over period Jan 1st - Dec 31 (2010). n = 252
#Cannot obtain data from CRSP without subscription
#Bias - selected companies above market cap, favored larger co.s
#TEMPORARY DATA USED FROM CAPIQ 
setwd("/Users/BlackHawk/Desktop/TheBigD/POET_Data/")
p1 = read.csv("POET_prices1.csv", 1)
p1 = p1[1:50,] #Irregularity in data
p2 = read.csv("POET_prices2.csv", 1)
prices = cbind(p1, p2)

#Need to turn price matrix into returns
prices_t_plus_1 = prices[,2:ncol(prices)]
prices = prices[,1:ncol(prices) - 1]
daily_returns = prices_t_plus_1/prices
daily_returns = log(daily_returns)
 
daily_returns = daily_returns[-c(seq(1,365,7), seq(2,365,7))] #Remove weekends from return data

daily_returns = 100*daily_returns

rowmeans = rowMeans(final_mat)  #Values may seem too large, but are correct. Look at columns 1700-1762
de_meaned_final_mat = final_mat - rowmeans   #de-mean final matrix to make POET more efficient
de_meaned_final_mat = as.matrix(de_meaned_final_mat)

Ks = POETKhat(as.matrix(de_meaned_final_mat))    #May take 20 minutes with large dataset...
K = Ks$K1HL

set_threshold = 'hard'

C = POETCmin(as.matrix(daily_returns), K, thres = set_threshold, 'vad')   #Also takes forever cause of CV
POET_MATRIX = POET(as.matrix(daily_returns), K, C, thres = set_threshold, 'vad')

POET_Cov_Mat = POET_MATRIX$SigmaY

# Get risk market risk (returns - risk free rate)
FF = read.csv("F-F_Research_Data_Factors_daily.csv")

market_risk = FF[23638:23889,2]  #date and market risk column
# data spans January 1, 2016 to December 31, 2016

#put in eigenvalues in ppt -> why we need to do the thresholding
