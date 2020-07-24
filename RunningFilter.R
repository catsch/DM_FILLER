RunningFilter <- function(k, y, na.fill=T, ends.fill=T, Method="Average") {

# RunningFilter is a function of running(moving/streaming) filter, there are four types of method, Average and Median methods are the filters to remove the high-frequency noise through regulating the window range; Maximum and Minimum methods are the functions to derive the nevelop lines

# k represents the half window, the window is set as (2k+1).

# y is the list of points to be processed.

# na.fill is the parameter to indicate whether to process the "NA" points, "T" means that all the "NA" points are included in the function as well as valid points, and filled as valid values unless there is not any valid values in the window (all methods are applied through the parameter "na.rm=T"); "N" means that all the "NA" points are excluded from the function and remained as "NA" (all methods are processed only for valid points).

# ends.fill is the parameter to indicate whether to process the "marginal" points (the beginning and end points are not able to be processed in a full window). "T" means these points are also processed, but in a shorter window; "F" means these points are re-assigned as "NA".

# Method is the parameter to indicate which method to be applied in the list of points, "Average" means choosing the average value in the window; "Median" means choosing the median value in the window; "Maximum" and "Minimum" means choosing the maximal or minimal value in the window. 

if (na.fill) f <- y
if(!na.fill) f <- y[!is.na(y)]

	n <- length(f)

	i1 <- k + 1
	i2 <- n - k

	for (i in 1:n) {

	if(i>=i1 & i <=i2) w <- y[(i - k):(i + k)]
	if(i<i1) w <- y[1:(i + k)]
	if(i>i2) w <- y[(i - k):n]

		if(all(is.na(w))){

			f[i] <- NA

		} else {

			if(Method==c("Median")) f[i] <- median(w,na.rm=T)
	
			if(Method==c("Average")) f[i] <- mean(w,na.rm=T)

			if(Method==c("Maximum")) f[i] <- max(w,na.rm=T)

			if(Method==c("Minimum")) f[i] <- min(w,na.rm=T)
			
		}
	}

	if(!ends.fill) {
		f[1:k] <- NA
		f[(i2+1):n] <- NA

	}

if (na.fill) y <- f
if(!na.fill) y[!is.na(y)] <- f

	return(y)
}
