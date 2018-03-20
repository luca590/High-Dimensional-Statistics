library("softImpute")
library("reshape2")
library("Metrics")

setwd("/Users/BlackHawk/Desktop/TheBigD/Netflix_Movie_Data/Small data") # Change this to the directory where your data is
movie_titles = read.csv("movies.csv") #movieId,title,genres
ratings = read.csv("ratings.csv") #userId,movieId,rating,timestamp

#---------- may (un)comment to reduce datasize and improve speed --------#
#ratings = ratings[1:10000,] 
#----------------------------------------------------------------------#

tags = read.csv("tags.csv") #userId,movieId,tag,timestamp
links = read.csv("links.csv") #movieId,imdbId,tmdbId - used in linking different sources of movie data

drop_rate = 0.01
drop_rows = sample(1:nrow(ratings),floor(nrow(ratings)*drop_rate))
rating_test = ratings[drop_rows,] #create our testing data
rating_training = as.matrix(ratings[,1:3]) #remove timestamp
rating_training[drop_rows,3] <- NA #We will use these rows for testing, so need to impute

rating_training = as.data.frame(rating_training) #cast to dataframe to use in reshape
rating_training = dcast(rating_training, userId ~ movieId, value.var = "rating")
rating_training = rating_training[,-1] #can drop the userid column, the names of rows are now useids

test_userids = as.character(rating_test[,1]) #We want to later get the columns with the same name as 
# our userids, not the column #. Because our data isn't perfect not every column # is the same as its corresponding userid
test_movieids = as.character(rating_test[,2])

userid_names = rownames(rating_training)
movieid_names = colnames(rating_training)

test_coordinates = cbind(test_userids, test_movieids)

#---------- A function for getting the ratings given lists of user and movie ids --------#
get_coordinates <- function(x, i, j) {  
    v = {}
    for(m in 1:length(i)) {
            v = cbind(v, x[i[m],j[m]])
    }
    return(as.vector(v))
}
#-----------------------------------------------------------------------------------------#

max_lambda = lambda0(rating_training) #This is the lambda that will return a 0 matrix, our lambda must be smaller
matrix_rank = floor(min(dim(rating_training))/2)
#result_parts = 0

#-------- Calculate vectors of actual (testing) data and predicted (training) data ------#
        # lam = lambda; method = "svd" or "als"
get_actual_and_predicted <- function(lam, method, training, rn, cn, testing, mr) {
        
        result_parts <<- softImpute(as.matrix(training), rank.max = mr, lambda = lam, type = method) #returns UDV
        imputed <<- as.matrix(result_parts$u) %*% diag(result_parts$d) %*% t(as.matrix(result_parts$v))
        # <<- in R means write as global variable (can be accessed outside function)
        # even though the last value in result_parts$d is always a 0, it's better to keep it than remove it
        
        rownames(imputed) = rn
        colnames(imputed) = cn

        actual = testing[,3]  # Taken from our testing data
        predicted = get_coordinates(imputed, test_userids, test_movieids) # Get the values we predicted with our imputation
    
        return(cbind(actual, predicted))
        }
#-----------------------------------------------------------------------------------------#

#---------- We will use the below function for testing different values of lambda -------#
testing_lambda = function(max_lam, training, rn, cn, testing, mr, type) {
# ------ Tunning Parameters --------#
    max_lam = max_lam / 1000
    step = 4  #this is the amount we will increase lambda with each iteration - large means faster convergence
    iterations = 30 #number of for loop iterations
#-----------------------------------#
#------------- set up --------------#
    lambda = {}
    soft_result = {}
    type.method = "svd" # our default method
    type.lambda = 0  # our default lambda
#-----------------------------------#
    
    for(i in 1:iterations) {
        switch(type,  # This switch statement will make the function slower, but the code simpler
               soft = {
                   type.lambda = max_lam*i*step
                 },
               hard = 1,
               als = {
                   type.lambda = max_lam*i*step
                   type.method = "als"
                })
        
        ap = get_actual_and_predicted(type.lambda, type.method, training, rn, cn, testing, mr)
        RMSE = rmse(ap[,1], ap[,2])  #Calculate the difference (error) btwn the values we predicted and actual values
        
        print(c("Soft RMSE for lambda = ", type.lambda, " is : ", RMSE, " i is: ", i))
        
        lambda[i] = type.lambda
        soft_result[i] = RMSE
    }
    return(cbind(lambda, soft_result))  #return the lambdas and their corresponding error rates that were tested
}
#----------------------------------------------------------------------------------#

get_best_lambda <- function(data) {
    min_error_index = which.min(data[,2])
    best_lambda = data[min_error_index, 1]
    return(c(best_lambda, data[min_error_index, 2]))
}


#--------------------------- Soft SVD -----------------------------#
lambdas_and_errors = testing_lambda(max_lambda, rating_training, userid_names, 
               movieid_names, rating_test, matrix_rank, type = "soft")
#get_actual_and_predicted: (lam, method, training, rn, cn, testing, mr)
soft_lambda = get_best_lambda(lambdas_and_errors)[1]
ap = get_actual_and_predicted(soft_lambda, method = "svd", rating_training, userid_names, 
               movieid_names, rating_test, matrix_rank)
soft_RMSE = rmse(ap[,1], ap[,2])  #Calculate the difference (error) btwn the values we predicted and actual values

#--------------------------- Hard SVD -----------------------------#
ap = get_actual_and_predicted(0, method = "svd", rating_training, userid_names, 
               movieid_names, rating_test, matrix_rank)
hard_RMSE = rmse(ap[,1], ap[,2])  #Calculate the difference (error) btwn the values we predicted and actual values

#----------------- Alternating Least Squares (ALS) ----------------#
lambdas_and_errors = testing_lambda(max_lambda, rating_training, userid_names, 
               movieid_names, rating_test, matrix_rank, type = "als")
als_lambda = get_best_lambda(lambdas_and_errors)[1]
ap = get_actual_and_predicted(als_lambda, method = "als", rating_training, userid_names, 
               movieid_names, rating_test, matrix_rank)
ALS_RMSE = rmse(ap[,1], ap[,2])

#problem is a lot of ratings with 0s