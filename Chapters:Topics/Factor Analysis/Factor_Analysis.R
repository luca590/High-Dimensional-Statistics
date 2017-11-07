library(readxl)
library(reshape2)

setwd("/Users/BlackHawk/Desktop/TheBigD/ERIM/")
BrandxWeek = read_excel("BrandxWeekxAdvertise.xlsx")
OtherxHH = read_excel("Common_to_week_vary_HH.xlsx")
EXPxHH = read_excel("TimexHHIDxExp.xlsx")
EXPxHH = EXPxHH[2:115]

#---------------------- Because BrandxWeek only has 47 unique dates -----------------#
#Get the factors
EXP_with_removed_dates = EXPxHH[, unique(unlist(BrandxWeek[,1]))]
cov_matrix = cov(EXP_with_removed_dates)
e = eigen(cov_matrix)

sprintf("precentage captured: %f", sum(e$values[1:17])/sum(e$values))

#-------------------------- Get factors and advertisement vector ---------------------#
factor_matrix = e$vectors[,1:17]
brands_without_dates = as.data.frame(BrandxWeek[,2:ncol(BrandxWeek)])
temp = apply(brands_without_dates, 2, unlist)
temp2 = apply(temp, 2, as.numeric)  #corece the data.frame into a numeric matrix, otherwise rowSums not possible
advertising_sum_vector = rowSums(temp2) #vector of advertising minutes summed


#-------- Get rid of the leading X's; might not need anymore ---------#
expend = data.frame(EXPxHH)
tempnames = colnames(expend)
tempnames2 = substr(tempnames, 2, 7)    #Take out the X from each column name (will also take out first H of HHID_2)
colnames(expend) = tempnames2
#-----------------------------------------------------------#

#-------------------- perform OLS --------------------------#
#sort matricies based on times before regression
linear_reg <- lm(advertising_sum_vector ~ factor_matrix, data = data.frame(advertising_sum_vector, factor_matrix))
summary(linear_reg)


#-------------------------- With university data -------------------------------#
setwd("/Users/BlackHawk/Desktop/TheBigD/World_uni_rankings/")

ranking_saudi_flie = read.csv("cwurData.csv")
year_file = read.csv("education_expenditure_supplementary_data.csv")
Barro_Lee_file = read.csv("educational_attainment_supplementary_data.csv")
uni_name_and_place_file = read.csv("school_and_country_table.csv")
ranking_shanghai_file = read.csv("shanghaiData.csv")
ranking_times_file = read.csv("timesData.csv")

# Do factor analysis on rankings of universities and see if the factors correlate to other variables
rank_mat = ranking_times_file[,1:2]
rank_mat = cbind(rank_mat, ranking_times_file[,14])
colnames(rank_mat) = c("Rank", "Name", "Year")
rank_matrix_with_names = dcast(rank_mat, Name ~ Year, value.var = "Rank")
pure_rank = rank_matrix_with_names[,2:7]

my_factors = unique(unlist(pure_rank))

# create uni x uni matrix with (year x )





