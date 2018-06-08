rm(list=ls())

library(countrycode)
library(datatools)
library(dplyr)
library(glmnet)
library(glmnetUtils)
library(imputeTS)
library(modelr)
library(purrr)
library(stringr)
library(tidyr)


source("fun.R")

#################
# Load the data #
#################

source("make_data.R")

# Exclude the residual dyad

refu9999 <- refu %>% filter(origin_ccode==9999 | asylum_ccode==9999)

alldy <- refu %>% filter( !(origin_ccode==9999 | asylum_ccode==9999) ) %>% 
	mutate(dyad=make_id(asylum_ccode,origin_ccode)) 




###########################
# Compute dyad attributes #
###########################

	
alldy <- alldy %>% group_by(dyad) %>% 
	mutate(y=tot, vary=var(y, na.rm=TRUE),
			   n_obs = sum(!is.na(y)), 
			   n_dist=n_distinct(y), n=n()) 

alldy <- ungroup(alldy) %>% 
	select(asylum_ccode, origin_ccode, year, tot, 
		   dyad, y, n_obs, vary, n_dist, n)





##################
# Construct data #
##################


# Construct predictors 

predictors <- alldy %>% group_by(dyad) %>% 
	arrange(year) %>% mutate(x=linpolation(y)) %>% 
	ungroup 


toimput <- alldy %>% filter(n_obs!=n & n_obs>2 & n_dist>3)

forlasso <- toimput %>% group_by(dyad) %>% 
	do(data=add_X(., predictors, drop=FALSE))




################
# Train models #
################


set.seed(42)

f <- y ~ . - year
res <- ungroup(forlasso) %>% 
  mutate(model=map(data, ~ 
  		safely(cv.glmnet)(f,
  		 data=as.data.frame(.),
  		 family=c("gaussian"), 
			 standardize=TRUE, 
			 type.measure='mae', 
			 grouped=FALSE,
			 na.action='na.omit')$result
  		))

res <- res %>% mutate(yhat=map2(model,data, 
		~safely(predict)(.x,newdata=.y)$result))

res_ <- res %>% select(dyad, data, yhat) %>% rowwise %>% 
	filter(length(yhat)!=0) %>% unnest %>% 
	select(dyad, year, yhat) %>% 
	rename(ylasso=yhat)


imputed <- predictors %>% left_join(res_, 
		by=c("dyad","year")) %>% 
	select(asylum_ccode,origin_ccode,year,dyad,
		y,x,ylasso) %>% rename(ylinpol=x)



# Adjustments: 
# - set negative values to zero
# - extrapolations: use last 
# 	observed value if LASSO 
# 	prediction exceeds
# 	this value.

imputed <- imputed  %>% group_by(dyad) %>% 
	arrange(year) %>% 
		mutate(end=endmiss(y), 
			ylasso=ifelse(ylasso<0,0,ylasso),
			ylassox=ifelse(is.na(y), ylasso, y),
		  ylassox=ifelse(is.na(ylassox), ylinpol, ylassox),
		  ylassox=ifelse(end & ylinpol<ylasso & !is.na(ylasso),
		  	ylinpol, ylassox))




##########
# Output #
##########


imputed <- ungroup(imputed) %>% select(-dyad, -end,-ylasso) 

write.csv(imputed, file="./usedata/wrd_1.1.0.csv",row.names=FALSE,na="")



