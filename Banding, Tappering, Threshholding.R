library(CVTuningCov)
library(reshape2)
library(lubridate)
library(MASS)
library(mice)
library(maps)
library(mapdata)

setwd("/Users/BlackHawk/Desktop/TheBigD/Global Climate Data/")
Climate_data_all = read.csv("GlobalLandTemperaturesByMajorCity.csv")
#------------------------ Bickel and Levina Banding Cov Matrix ----------------------#
Longitude_latitude_date = Climate_data_all[,c("dt", "AverageTemperature", "Latitude", "Longitude")]
#Only keep at month of January 
January_temp = subset(Longitude_latitude_date, 
                      month(as.POSIXlt(as.character(Longitude_latitude_date[,"dt"]))) == 01)

length_of_coordinate_number = 3

January_temp <- within(January_temp, {
    Latitude <- ifelse(grepl("N", as.character(Latitude)),
                       as.numeric(substr(as.character(Latitude), 1, length_of_coordinate_number + 1)),
                        -1*as.numeric(substr(as.character(Latitude), 1, length_of_coordinate_number + 1))
    )
    Longitude <- ifelse(grepl("E", as.character(Longitude)),
                       as.numeric(substr(as.character(Longitude), 1, length_of_coordinate_number + 1)),
                       -1*as.numeric(substr(as.character(Longitude), 1, length_of_coordinate_number + 1))
    )
})


combine_coordinates = interaction(unlist(January_temp["Longitude"]), unlist(January_temp["Latitude"]), sep = " ")
January_temp2 = January_temp[,c("dt", "AverageTemperature")]
January_temp3 = cbind(January_temp2, combine_coordinates)
#longitude first then latitude for mapping function

final_mat = dcast(January_temp3, dt ~ combine_coordinates, value.var = "AverageTemperature", fun.aggregate = mean)
final_mat = final_mat[117:270,]
rownames(final_mat) = final_mat[,1:1]  #make the row names the dates of each row
final_mat = final_mat[,2:ncol(final_mat)]

final_mat[final_mat == "NaN"] <- NA    #replace all NaNs with NAs so easier to impute
a = mice(final_mat, m = 1, maxit = 5, meth = "pmm", seed = 500)
complete_matrix = complete(a, 1)     #Input the imputed values from mice and set to final_mat
#----------------------------- Please explain mice ---------------------#
final_mat = complete_matrix

cov_matrix = cov(final_mat)
corr_matrix = cor(final_mat)

#final_mat = matrix(rnorm(144, 0, 1), 12, 12)
#banding
banding_k = random.CV(final_mat, method = "Banding", k.grid = seq(0,100,1))
#banding produces a diagonal identity matrix with width "banding_k$CV.k[1]" so need to multiply by cov_matrix
cov_banded = cov_matrix * banding(nrow(cov_matrix), banding_k$CV.k[1])

#thresholding
hard_thresh = random.CV(final_mat, method = "HardThresholding", k.grid = seq(0,2,0.05))
#cov_hard = hard.thresholding(cov_matrix, hard_thresh$CV.k[1])
cov_hard = hard.thresholding(cov_matrix, 0.15)

soft_thresh = random.CV(final_mat, method = "SoftThresholding", k.grid = seq(0,2,0.05))
#cov_soft = soft.thresholding(cov_matrix, soft_thresh$CV.k[1])
#hard set to 0.15 because CV unsuccessful
cov_soft = soft.thresholding(cov_matrix, 0.15)

#tappering
taper = random.CV(final_mat, method = "Tapering", k.grid = seq(0,2,0.05))
taper_matrix = tapering(dim(cov_matrix), k = taper$CV.k[1])
#now multiply

#--------------- Calculate Principal Components of cov matrix -----------#
map('worldHires')
e = eigen(cov_soft)
e_vector = e$vectors[,1:4]
rownames(e_vector) = rownames(cov_soft)

#Need to turn PC1 back into matrix with latitude and longitude?
PC1 = as.matrix(e_vector[,1:1])
m = 10*1:ncol(PC1)
n = 10*1:nrow(PC1)

PC2 = as.matrix(e_vector[,2:2])
PC3 = as.matrix(e_vector[,3:3])
PC4 = as.matrix(e_vector[,4:4])


#----------------  Creat Conture plot by passing vector as argument -------------------#
create_conture <- function(PC1) {
coordinate_column = unlist(rownames(PC1))
#will use this library to split column names into 2 columns (long and lat)
library(stringr)
#split coordinate column into longitude and latitude
temp = str_split_fixed(coordinate_column, " ", 2)
temp = as.data.frame(apply(temp, 2, as.numeric))
PC1_2 = cbind(temp, PC1)

n_row = 200
n_col = 200
normalizer = 100/(max(PC1_2)+abs(min(PC1_2)))

map_mat = matrix(rep(0,n_row*n_col), n_row, n_col) #create matrix of zeros, size of map
#Then inject PC values into newly created matrix of 0's
map_mat[round(PC1_2[,1]*normalizer+abs(min((PC1_2)))),round(PC1_2[,2]*normalizer+abs(min((PC1_2))))] = PC1_2[,3]

filled.contour(seq(n_row), seq(n_col), map_mat)
}
#-----------------------------------------------------------------------------#

#Output conture plots
create_conture(PC1)
create_conture(PC2)
create_conture(PC3)
create_conture(PC4)


