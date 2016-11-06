pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        directory <- paste(directory, "/", sep = "")
        all_files <- as.character( list.files(directory) )
        file_paths <- paste(directory, all_files, sep="")
        pollute <- vector("numeric")
        for (each_id in id) {
                temp <- read.csv(file_paths[each_id])
                pollute <- c(pollute, temp[[pollutant]])
        }
        bad <- is.na(pollute)
        cleaned_pollutant <- pollute[!bad]
        mean(cleaned_pollutant)
}

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        directory <- paste(directory, "/", sep = "")
        all_files <- as.character( list.files(directory) )
        file_paths <- paste(directory, all_files, sep="")
        j <- 1
        nobs = vector("integer", length = length(id))
        for (each_id in id) {
                temp <- read.csv(file_paths[each_id])
                nobs[j] <- sum(complete.cases(temp))
                j <- j + 1
        }
        
        data.frame(id, nobs)
}

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        directory <- paste(directory, "/", sep = "")
        all_files <- as.character( list.files(directory) )
        file_paths <- paste(directory, all_files, sep="")
        id = 1:332
        corrs <- vector("numeric", length = 332)
        j <- 1
        for (each_id in id) {
                temp <- read.csv(file_paths[each_id])
                if(sum(complete.cases(temp)) > threshold) {
                        sulfate <- temp$sulfate
                        nitrate <- temp$nitrate
                        corrs[j] <- cor(x = sulfate, y = nitrate, use="complete.obs")
                        j <- j + 1
                }
        }
        corrs
}
