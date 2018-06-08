#######################
#######################
#######################
#  SYSTEM MEMBERSHIP  #
#######################
#######################
#######################

system <- read.csv("./rawdata/states2016.csv") 
system <- system %>%  select(ccode, styear, endyear) 
system <- rbind(system,c(667,1989,2016))  # Include Palestine State 
system <- rbind(system,c(9999,1989,2016))  # Include "Various/Unknown"

system <- with(system, expand.grid(ccode,ccode,seq(1816,2016), stringsAsFactors=FALSE )) %>%
			   	  distinct(Var1, Var2, Var3) %>%
				  rename(statea=Var1, stateb=Var2, year=Var3) %>%
				  left_join(., system, by=c("statea"="ccode")) %>%
				  filter(year >= styear & year <= endyear) %>%
				  select(-styear,-endyear) %>% 
				  left_join(., system, by=c("stateb"="ccode")) %>%
				  filter(year >= styear & year <= endyear) %>%
				  select(-styear,-endyear)
system <- filter(system, year >= 1989, statea!=stateb )
system <- rename(system, asylum_ccode=statea, origin_ccode=stateb)



# Filter all microstates using the Gleditsch/Ward threshold of 250000. 

population <- read.csv("./rawdata/NMC_5_0.csv")
population <- select(population, year, ccode, tpop)
population <- mutate(population, tpop = tpop * 1000, tpop = ifelse(tpop <0, NA, tpop) )

system <- merge(system, population, 
		by.x=c("year", "origin_ccode"), 
		by.y=c("year", "ccode"), all.x=TRUE, all.y=FALSE )

microstates <- unique(select( filter(system, tpop <= 250000), origin_ccode))$origin_ccode

system <- filter(system, !(origin_ccode %in% microstates) & 
				!(asylum_ccode %in% microstates) ) %>% select(-tpop)




###################
###################
###################
# LOAD UNHCR DATA #
###################
###################
###################


refu <- read.csv("./rawdata/unhcr_popstats_export_persons_of_concern_all_data.csv", 
	header=TRUE, na.strings=c("*"), row.names = NULL, stringsAsFactors = FALSE, 
	colClasses=c("numeric", rep("character",2), rep("numeric", 8)))

refu <- filter(refu, year <= 2016 & year>=1989)

refu$asylum_ccode <- with(refu, countrycode(asylum, "country.name", "cown"))
refu$origin_ccode <- with(refu, countrycode(origin, "country.name", "cown"))


# Fix: Today's Vietnam is 816

refu <- mutate(refu, asylum_ccode=ifelse(asylum_ccode==817, 816, asylum_ccode),
	origin_ccode=ifelse(origin_ccode==817, 816, origin_ccode))


# Assign territories to states

refu <- mutate(refu, 
asylum_ccode = ifelse(asylum=="French Guiana", 220, asylum_ccode), 
asylum_ccode = ifelse(asylum=="China, Hong Kong SAR", 710, asylum_ccode), 
asylum_ccode = ifelse(asylum=="China, Macao SAR", 710, asylum_ccode), 
asylum_ccode = ifelse(asylum=="State of Palestine", 667 , asylum_ccode),
asylum_ccode = ifelse(asylum=="British Virgin Islands", 200, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Montserrat", 200, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Aruba", 210, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Curaçao", 210, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Bonaire", 210, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Sint Maarten (Dutch part)", 210, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Turks and Caicos Islands", 200, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Cayman Islands", 200, asylum_ccode),
asylum_ccode = ifelse(asylum=="Various/Unknown", 9999, asylum_ccode),
asylum_ccode = ifelse(asylum=="Serbia and Kosovo (S/RES/1244 (1999))", 345, asylum_ccode), 
asylum_ccode = ifelse(asylum=="The former Yugoslav Republic of Macedonia" & year < 1993, 345, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Czech Rep." & year < 1993, 315, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Lithuania" & year < 1990, 365, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Armenia" & year < 1991, 365, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Namibia" & year < 1990, 560, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Eritrea" & year < 1993, 530, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Timor-Leste" & year < 2002, 850, asylum_ccode),
asylum_ccode = ifelse(asylum=="Anguilla", 200, asylum_ccode), 
asylum_ccode = ifelse(asylum=="Central African Rep.", 482, asylum_ccode) )

refu <- mutate(refu, 
origin_ccode = ifelse(origin=="Serbia and Kosovo (S/RES/1244 (1999))", 345, origin_ccode), 
origin_ccode = ifelse(origin=="The former Yugoslav Republic of Macedonia" & year < 1993, 345, origin_ccode),
origin_ccode = ifelse(origin=="Czech Rep." & year < 1993, 315, origin_ccode), 
origin_ccode = ifelse(origin=="Lithuania" & year < 1990, 365, origin_ccode), 
origin_ccode = ifelse(origin=="Armenia" & year < 1991, 365, origin_ccode), 
origin_ccode = ifelse(origin=="Namibia" & year < 1990, 560, origin_ccode), 
origin_ccode = ifelse(origin=="Eritrea" & year < 1993, 530, origin_ccode), 
origin_ccode = ifelse(origin=="Timor-Leste" & year < 2002, 850, origin_ccode), 
origin_ccode = ifelse(origin=="Tibetan", 710, origin_ccode), 
origin_ccode = ifelse(origin=="Western Sahara", 600, origin_ccode), 
origin_ccode = ifelse(origin=="Palestinian", 667 , origin_ccode),
origin_ccode = ifelse(origin=="French Guiana", 220, origin_ccode), 
origin_ccode = ifelse(origin=="China, Hong Kong SAR", 710, origin_ccode), 
origin_ccode = ifelse(origin=="China, Macao SAR", 710, origin_ccode), 
origin_ccode = ifelse(origin=="Puerto Rico", 2, origin_ccode), 
origin_ccode = ifelse(origin=="American Samoa", 2, origin_ccode), 
origin_ccode = ifelse(origin=="Martinique", 220, origin_ccode), 
origin_ccode = ifelse(origin=="Bermuda", 200, origin_ccode), 
origin_ccode = ifelse(origin=="French Polynesia", 220, origin_ccode), 
origin_ccode = ifelse(origin=="Turks and Caicos Islands", 200, origin_ccode), 
origin_ccode = ifelse(origin=="Gibraltar", 200, origin_ccode), 
origin_ccode = ifelse(origin=="New Caledonia", 220, origin_ccode), 
origin_ccode = ifelse(origin=="Cook Islands", 920 , origin_ccode),
origin_ccode = ifelse(origin=="Aruba", 210, origin_ccode), 
origin_ccode = ifelse(origin=="US Virgin Islands", 2, origin_ccode), 
origin_ccode = ifelse(origin=="Cayman Islands", 200, origin_ccode),
origin_ccode = ifelse(origin=="Niue", 920, origin_ccode),
origin_ccode = ifelse(origin=="Sint Maarten (Dutch part)", 210, origin_ccode), 
origin_ccode = ifelse(origin=="Anguilla", 200, origin_ccode), 
origin_ccode = ifelse(origin=="British Virgin Islands", 200, origin_ccode), 
origin_ccode = ifelse(origin=="Curaçao", 210, origin_ccode), 
origin_ccode = ifelse(origin=="Guadeloupe", 220, origin_ccode), 
origin_ccode = ifelse(origin=="Réunion", 220, origin_ccode), 
origin_ccode = ifelse(origin=="Saint-Pierre-et-Miquelon", 220, origin_ccode), 
origin_ccode = ifelse(origin=="Mayotte", 220, origin_ccode), 
origin_ccode = ifelse(origin=="Norfolk Island", 900, origin_ccode), 
origin_ccode = ifelse(origin=="Wallis and Futuna Islands ", 220, origin_ccode),
origin_ccode = ifelse(origin=="Svalbard and Jan Mayen", 385, origin_ccode),
origin_ccode = ifelse(origin=="Central African Rep.", 482, origin_ccode),
origin_ccode = ifelse(origin=="Various/Unknown", 9999, origin_ccode),
origin_ccode = ifelse(origin=="", 9999, origin_ccode), 
origin_ccode = ifelse(origin=="Stateless", 9999, origin_ccode))


refu <- refu  %>% mutate(refugees = ifelse(is.na(refugees) & !is.na(total), 0, refugees))

# Retain refugee series (label as 'total')
refu <- select(refu, year, asylum, origin, 
	asylum_ccode, origin_ccode, refugees) %>% 
		rename(total = refugees)

# This drops all refugees which are 'internally displaced' 
refu <- filter(refu, (asylum_ccode != origin_ccode) )







######################
######################
######################
# INCLUDE UNRWA DATA #
######################
######################
######################

unrwa <- read.csv("./rawdata/unrwa.csv", sep=";", header=TRUE, 
	row.names = NULL, stringsAsFactors = FALSE, colClasses="numeric")

unrwa <- gather(unrwa, asylum, total, -year) 
unrwa <- mutate(unrwa, asylum_ccode=countrycode(asylum, "country.name", "cown"))
unrwa <- mutate(unrwa, asylum_ccode=ifelse(is.na(asylum_ccode), 667, asylum_ccode))
unrwa <- mutate(unrwa, origin_ccode=667, origin="Palestine")
unrwa <- unrwa %>% filter( (asylum_ccode != origin_ccode) )
unrwa <- select(unrwa, year, asylum, origin, asylum_ccode, origin_ccode, total)

refu <- rbind(unrwa, refu)

refu <- group_by(refu, asylum_ccode, origin_ccode, year) %>% 
		summarize(tot=sum(total))

refu <- filter(refu, !(origin_ccode %in% microstates) & !(asylum_ccode %in% microstates) ) 



#############
#############
#############
# MERGE ALL #
#############
#############
#############

refu <- merge(system, refu, by=c("asylum_ccode", 
	"origin_ccode", "year"), all.x=TRUE, all.y=FALSE)

refu <- mutate(refu, asylum= countrycode(asylum_ccode, "cown", "country.name") )
refu <- mutate(refu, origin= countrycode(origin_ccode, "cown", "country.name"))

refu <- refu %>% 
	mutate(asylum=ifelse(asylum_ccode==9999, "Various", asylum),
				 origin=ifelse(origin_ccode==9999, "Various", origin),
				 asylum=ifelse(asylum_ccode==816, "Vietnam", asylum),
				 origin=ifelse(origin_ccode==816, "Vietnam", origin),
				 asylum=ifelse(asylum_ccode==667, "State of Palestine", asylum),
				 origin=ifelse(origin_ccode==667, "State of Palestine", origin),
				 asylum=ifelse(asylum_ccode==260, "Germany", asylum),
				 origin=ifelse(origin_ccode==260, "Germany", origin))

refu <- ungroup(refu)

# exclude 2016 since incomplete (for now)
refu <- refu %>% filter(year!=2016)







