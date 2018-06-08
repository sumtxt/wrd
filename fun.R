as_df <- function(x) as.data.frame(x)


# Interpolates a sorted time series 
# or assigns zero if no value is observed. 
# Interpolation of less than zero are reset to 0.
linpolation <- function(x){
	n_obs <- sum(!is.na(x))
	n <- length(x)

	mu <- mean(x,na.rm=TRUE)

	if (n_obs==0) {
		res <- rep(0,n)
	} else if (n_obs==1) { 
		res <- rep(mu,n) 
	} else {
		res <- na.interpolation(x) 
	}
	res <- ifelse(res<0,0,res)
	return(res)
	}



# Takes a dyad and adds the predictors 
# which either matches the dyad's host
# or destination country 
add_X <- function(dat, pred, drop=TRUE){
	a <- unique(dat %>% pull(asylum_ccode))
	o <- unique(dat %>% pull(origin_ccode))

	pred <- anti_join(pred,dat,
		by=c("asylum_ccode","origin_ccode","year"))

	pred <- pred %>% ungroup %>% 
		filter((asylum_ccode==a | 
						origin_ccode==o) & 
						vary!=0  ) %>% 
		select(year, dyad, x) %>% 
		mutate(x=log(x+1)) %>% 
		spread(dyad, x, fill=0,sep="") 

	dat <- dat %>% full_join(pred, by="year")
	dat <- dat %>% arrange(year) %>%
		select(y,year, matches("dyad[0-9]"))

	if( drop==TRUE ) { dat <- dat %>% filter(!is.na(y)) } 

	dat %>% mutate_at(vars(matches("^dyad")), standardize2 )
	
	return(dat)
	}



# Indicator that equals 
# TRUE if a values in a 
# sequence of values is 
# unobserved and is located
# at the endpoints of the 
# sequence
endmiss <- function(x){
	miss <- is.na(x)
	seq_id <- make_series_id(miss)
	seq_id_max <- max(seq_id)
	seq_id_min <- min(seq_id)
	ends <- (seq_id==seq_id_max|
		seq_id==seq_id_min)   
	return(ends & miss)
}

# endmiss for resample() object
is_endpoint <- function(testdata){
	dat <- testdata$data 
	idx <- testdata$idx
	dat <- dat[order(dat$year),]
	y_to_int <- y <- dat$y
	y_to_int[idx] <- NA
	return(endmiss(y_to_int)[idx])
	}
