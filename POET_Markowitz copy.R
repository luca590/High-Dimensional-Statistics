library(POET)
library(reshape2)
library(CVTuningCov)

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

#Take out any columns with all 0s
daily_returns = daily_returns[,colSums(daily_returns) != 0]

dim(daily_returns)      #daily_returns is now 50 x 252 (taking out weekends). Stocks x time

daily_returns = 100*daily_returns
#Maybe because of package? Need to transpose daily_returns so factor is 1 x 50
#daily_returns = t(daily_returns)

rowmeans = rowMeans(daily_returns)  #Values may seem too large, but are correct. Look at columns 1700-1762
de_meaned_final_mat = daily_returns - rowmeans   #de-mean final matrix to make POET more efficient
de_meaned_final_mat = as.matrix(de_meaned_final_mat)

Ks = POETKhat(as.matrix(de_meaned_final_mat))    #May take 20 minutes with large dataset...
K = Ks$K1HL


set_threshold = 'hard'

C = POETCmin(as.matrix(daily_returns), K, thres = set_threshold, 'vad')   #Also takes forever cause of CV
POET_MATRIX = POET(as.matrix(daily_returns), K, C, thres = set_threshold, 'vad')

#--------------------------- Relation to Eigenvectors???? ---------------------#
#Check if eigenvectors and factors are the same
print('Checking...' )
cov_daily = cov(t(daily_returns))  #covariance between stocks instead of time
e_daily = eigen(cov_daily)

#Consider setting K = 3, look at eigenvalues
plot(e_daily$values)
#------------------------------------------------------------------------------#

#----------- Estimated cov of raw data, estimated by POET: NOT sample cov -----------#
calculate_weights <- function(covariance_matrix) {
#calculate weights
one_vector = rep(1,ncol(covariance_matrix))
inverse_cov = solve(covariance_matrix)

alpha = daily_returns - POET_MATRIX$loadings%*%POET_MATRIX$factors
alpha = rowMeans(alpha)

mu = alpha
#mu_target is scalar, NOT vector

mu_target = 0.04  

A = as.numeric(t(mu)%*%inverse_cov%*%one_vector)   #will result in scalar
B = as.numeric(t(mu)%*%inverse_cov%*%mu)           #will result in scalar
C = as.numeric(t(one_vector)%*%inverse_cov%*%one_vector)       #will result in scalar
D = B*C-(A*A)

w_eff = (B*inverse_cov%*%one_vector-
             A*inverse_cov%*%mu+
             mu_target*(C*inverse_cov%*%mu-
             A*inverse_cov%*%one_vector))/D    #Should transpose mu_traget... correct?

w_eff
}
#---------------------------------------------------------------------------------------#

calculate_risk <- function(w_eff) {
    as.numeric((t(w_eff)/ncol(daily_returns))%*%cov_daily%*%w_eff)
}

w = calculate_weights(POET_MATRIX$SigmaY)
w2 = calculate_weights(Portfolio_wieghts_diag)

risk_POET = calculate_risk(w)
risk_DIAG = calculate_risk(w2)


