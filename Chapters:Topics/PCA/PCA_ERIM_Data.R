library(stringr, stats)

data_file = "Raw/"
file_count = 8    #change this varibale to change the data file type # (ex. file8)
data_item_type = "brownie"  #change this varibale to change the data content (ex. brownie or sugar)

setwd(str_c("/Users/BlackHawk/Desktop/TheBigD/ERIM/", data_file, data_item_type, "/"))
BigData = str_c(data_item_type, "_f", toString(file_count), ".dat") #create data path

#--------------------- Read the Data and Initialize Vectors -------------------#
rows_to_read = 50 #change this # to increase number of cases to feed algo
sequence_length = 74 #length of byte sequence, will vary depending on file being read
selected_rows = readLines(BigData, n = rows_to_read) #how many rows to consider
one_row = selected_rows[1]
digits = strsplit(one_row, split = "", fixed = FALSE, perl = FALSE, useBytes = TRUE)

num_sequence = digits[[1]][1:sequence_length]

#put number characters together with paste, then convert to integers
#HHID = num_sequence[1:8] - don't include for now, b/c needs more than 1 byte
NumCats = strtoi(paste(num_sequence[13:14], collapse = ""), base = 10)
NumDogs = strtoi(paste(num_sequence[15:16], collapse = ""), base = 10)
NumTVs = strtoi(paste(num_sequence[17:18], collapse = ""), base = 10)
Income = strtoi(paste(num_sequence[22:23], collapse = ""), base = 10)
NHHMem = strtoi(paste(num_sequence[24:25], collapse = ""), base = 10) #number of members in household
Dish_Washer = strtoi(num_sequence[50], base = 10)
Freezer = strtoi(num_sequence[51], base = 10)
Toaster = strtoi(num_sequence[52], base = 10)

#---------------------- Create Data Frame -------------------------------------#

# Can iterate through data and individially get each integer in byte sequence and create vectors of integers from data
for (i in 2:length(selected_rows)) {
  one_row = selected_rows[i:i]
  digits = strsplit(one_row, split = "", fixed = FALSE, perl = FALSE, useBytes = TRUE)
  num_sequence = digits[[1]][1:sequence_length] #update num_sequence

  NumCats = c(NumCats, strtoi(paste(num_sequence[13:14], collapse = ""), base = 10))
  NumDogs = c(NumDogs, strtoi(paste(num_sequence[15:16], collapse = ""), base = 10))
  NumTVs = c(NumTVs, strtoi(paste(num_sequence[17:18], collapse = ""), base = 10))
  Income = c(Income, strtoi(paste(num_sequence[22:23], collapse = ""), base = 10))
  NHHMem = c(NHHMem, strtoi(paste(num_sequence[24:25], collapse = ""), base = 10))
  Dish_Washer = c(Dish_Washer, strtoi(num_sequence[50], base = 10))
  Freezer = c(Freezer, strtoi(num_sequence[51], base = 10))
  Toaster = c(Toaster, strtoi(num_sequence[52], base = 10))
}

brownie.data <- data.frame(NumCats, NumDogs, NumTVs, Income, 
                          NHHMem, Dish_Washer, Freezer, Toaster) #put vectors into data frame

brownie.data = brownie.data * 1.0 #convert int to neumeric

#--------------------------------Do PCA (2 methods)---------------------------#


#----------------------- With Evectors and Eigenvalues -----------------------#
cov_matrix = cov(brownie.data)
#finding eigendvectors and eigendvalues; eigendvalues are always returned in decreasing order
#values are associated (location) with vectors
eigendStuff = eigen(cov_matrix)
eVectors = eigendStuff$vectors
eValues = eigendStuff$values 

eigendStuff #print eigendvalues and vectors

#------------------------- PCA with Package ----------------------------------#
br.pca <- prcomp(brownie.data)
br.pca    #print pca











       
