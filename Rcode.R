#Q1. Download the FFIEC’s Home Mortgage Disclosure Act (HMDA) Loan/Application Register (LAR) data for all FDIC secured loans 
#at https://ffiec.cfpb.gov/data-publication/snapshot-national-loan-level-dataset/2018. 

#Q2. Load the data. Please note, the datafiles do not have column names, which you must add

pacman :: p_load(pacman, tidyverse, dplyr, ggplot2, cowplot, pastecs)
library(stringr)
library(formattable)

#Imported the IL dataset

load("total_dat.Rdata")

#Q3. Data work:

#a. Reduce both data files to only the State your group is exploring. Reduce both data files to only the State (using State FIPS code) your group is exploring.

#b. Append both files into a single data set.

#parts a and b have already been taken care of in the importing aspect of the assignment.

#c. Format the FIPS county code digits with leading zeros as necessary. Format the FIPS county code as 3 digits with leading zeros as necessary.

#i. This may be easier to do when reading in the file originally depending on the program you employ.

#ii. You are welcome to make the data type text if that is the only way you can figure it out.

#Part c: Here I am adding a leading 0 to the county code to make it 6 digits for further analysis

total_dat$county_code <- paste0("0", total_dat$county_code)

#Part d: The loan amount in the dataset will now be converted into a currency format

library(formattable)
total_dat$loan_amount <- currency(total_dat$loan_amount)

#Q4. Provide the following statistical summaries.
#a. Provide a table of minimum, maximum, and average value of loan by loan type. 
#i.Please make sure that you use the values for loan type and not the numeric codes. (You can use a format or recode the variable , 
#but it should read something like FHA not “2”).

stats <- function(x) {
low <- min(x)
high <- max(x)
avg <- mean(x)
return(c(low, high, avg))
}
stats1 <- total_dat %>%
filter(loan_type == 1)
conventional <- stats(stats1$loan_amount)
conventional
stats2 <- total_dat %>%
filter(loan_type == 2)
FHA <- stats(stats2$loan_amount)
FHA
stats3 <- total_dat %>%
filter(loan_type == 3)
VA <- stats(stats3$loan_amount)
VA
stats4 <- total_dat %>%
filter(loan_type == 4)
RHS_or_FSA <- stats(stats4$loan_amount)
RHS_or_FSA
df <- data.frame("Conventional" = conventional, "FHA" = FHA, "VA" = VA, "RHS or FSA" =
RHS_or_FSA)
rownames(df) <- c("Min Value", "Max Value", "Avg. Value")
df

# b. Provide similar information graphically. Provide a grouped bar chart with the average loan value by loan type grouped by year. 
#Label the averages for each bar on the chart.

total_dat$loan_type <- factor(total_dat$loan_type, levels = c(1, 2, 3, 4), labels =
c("Conventional", "FHA", "VA", "RHS_or_FSA"))
total_dat$activity_year <- as.character(total_dat$activity_year)
df2 <- total_dat %>%
group_by(loan_type, activity_year) %>%
summarise_at(.vars = "loan_amount", .funs = mean)
ggplot(df2, aes(x = loan_type, y = loan_amount, fill = activity_year)) +
geom_col(position = position_dodge()) +
labs(title = "Average loan value by loan type grouped by year",
x = "Loan Type", y = "Loan Amount", fill = "Year") +
geom_text(aes(label = loan_amount), vjust = -1, position = position_dodge(1))

# c. Provide a table of the average value of loans for 2019 and 2018 by property type. i. Make sure loan type values read with appropriate labels inside the 
#table produced by the software. (You can use a format or recode the variable, but for example 2 should be replaced with "Manufactured housing" in the table.

table(total_dat$manufactured_home_secured_property_type)
head(total_dat %>%
filter(manufactured_home_secured_property_type == 1111))
df3 <- total_dat %>%
group_by(activity_year, manufactured_home_secured_property_type) %>%
summarise_at(.vars = "loan_amount", .funs = mean)
df3$manufactured_home_secured_property_type <-
factor(df3$manufactured_home_secured_property_type,
levels = c(1, 2, 3, 1111),
labels = c("Manufactured home and land",
"Manufactured home and not land",
"Not Applicable",
"Exempt"))
df3 <- df3 %>%
rename(Year = activity_year, Property_Type = manufactured_home_secured_property_type,
Avg_Loan_Amount = loan_amount)
df3

# d. Provide a two-way table of the action taken on the loan by preapproval. 
#i. Make sure that only the counts are in the table (no percentages, no row or column totals, ONLY counts)

tab <- table(total_dat$preapproval, total_dat$action_taken)
colnames(tab) = c("Loan Originated", "Application approved but not accepted",
"Application Denied", "Application withdrawn by applicant",
"File closed for incompleteness", "Purchased Loan",
"Preapproval request denied", "Preapproval request approved but not accepted")
rownames(tab) = c("Preapproval requested", "Preapproval not requested")
tab

# ii. Run a Chi-Squared test on the frequency table. Interpret the result.
chisq.test(tab)

#Explanation: We can set the null hypothesis as there being no relationship between the preapproval
#on a loan and the action taken on it. The alternate would be that there is a relationship. If
#we set the alpha value as 0.05 or even 0.01, we can see that the p-value is far less than
#any of these alpha values and so the results are significant. We can therefore reject the
#null and conclude that there is a relationship between preapproval and the action taken
#on that loan. So knowing one will allow us to predict the other.

#iii. Examine the “Not Applicable” category. What percentage of the data falls into this category?
# Examining the "Not Applicable" category
# Taking the "File closed for incompleteness" as the "Not Applicable" category

(37131/sum(tab))*100

#results show that 3.4% of the data fall within this category. 

# iv. Set “Not Applicable” to be missing and rerun the Chi-Squared test. Does the result and interpretation change? Is this test meaningful?

tab[,5] <- 0
chisq.test(tab)

#Explanation: By setting the "Not Applicable" category to missing, the result is not meaningful. Because the test itself cannot be run as the entries must be 
#nonnegative and finite in order to be calculated. Examining the variable “Action Taken” by the following variables. For each of the variables, examine the 
#cross tabulations and provide a meaningful graph that shows the differences. For each entry, do you see any differences for the groups that are either 
#“Application Denied” or “Pre-approval request denied”?

# Q5. Examine the variable “Action Taken” by the following variables. For each of the variables, examine the cross-tabulations and provide a 
#meaningful graph that shows the differences. For each entry, do you see any differences for the groups that are either “Application Denied” 
#or “Preapproval request denied”?

#a. Income

total_dat$action_taken <- revalue(as.character(total_dat$action_taken), c("1" = "Loan
originated" ,
"2" = " Application approved but not accepted",
"3" = "Application denied",
"4" = "Application withdrawn by applicant",
"5" = "File closed for incompleteness",
"6" = "Purchased loan",
"7" = "Preapproval request denied",
"8" = "Preapproval request approved but not accepted"))
View(total_dat)

# cross tabulations

req <- substitute(require(x, character.only = TRUE))
libs<-c("sjPlot")
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})

# action taken v/s income table

tabincome <- tab_xtab(var.row = total_dat$income, var.col = total_dat$action_taken,
title = "Cross Table: Action taken v/s Income", show.row.prc = TRUE)
tabincome

# Answer: The cross-tabulation indicates that 15% of loan applicants fall under the ‘Application denied’ category, out of which most fall under 
#the $0 to $700 income bracket. For ‘Preapproval request denied’ applicants are in the income bracket: $3 to $130.

# action taken v/s income graph

options(scipen=999)
graphincome <- ggplot(total_dat, aes(x = total_dat$action_taken, y = total_dat$income)) +
geom_point(color = "red") +
xlab("Action taken") + ylab("Income ($)") + ylim(0,350000) + ggtitle("Income v/s Action taken") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip()
graphincome

# Answer: The scatterplot indicates that there are very few high-income applicants whose loan applications have been denied; applicants with their 
#request denied are mostly with an income of below $100,000. For ‘Preapproval request denied’ applicants are in the income bracket: $3 to $130.

# b. Race of Applicant (1)

total_dat$applicant_ethnicity_1 <- revalue(as.character(total_dat$applicant_ethnicity_1),
c("1" = "American Indian or Alaska Native",
"2" = "Asian", "21" = "Asian Indian", "22" = "Chinese",
"23" = "Filipino", "24" = "Japanese", "25" = "Korean",
"26" = "Vietnamese", "27" = "Other Asian", "3" = "Black or African
American",
"4" = "Native Hawaiian or Other Pacific Islander", "41" = "Native
Hawaiian",
"42" = "Guamanian", "43" = "Samoan", "44" = "Other Pacific
Islander",
"5" = "White", "6" = "Information not provided by applicant", "7" =
"Not applicable",
"11" = "Mexican", "12" = "Puerto Rican", "13" = "Cuban", "14" =
"Other Hispanic or Latino"
))

# action taken v/s race of applicant table

tabincome1 <- tab_xtab(var.row = total_dat$action_taken, var.col =
total_dat$applicant_ethnicity_1, title = "Cross Table: Action taken v/s Race of Applicant (1)",
show.row.prc = TRUE)
tabincome1

# Answer: From the cross-tabulation we can see that the race with the most number of actions taken on a loan are the Asians. While the lowest are the Cubans.
#With respect to the Application Denied category, the most number of people who are denied are Asians, and this is the same case for the Preapproval request 
#denied category.

# graph for action taken vs race of applicant:

total_dat %>%
ggplot(aes(x = action_taken, fill = applicant_ethnicity_1)) +
geom_bar(position = "dodge") +
coord_flip() +
labs(x = "Action Taken", fill = "Race of Applicant (1)", title = "Barplot of Action Taken grouped
by Race of Applicant (1)")

# c. Sex

# action taken v/s sex table

tabsex <- tab_xtab(var.row = total_dat$action_taken, var.col = total_dat$derived_sex,
title = "Cross Table: Action taken v/s Sex", show.row.prc = TRUE)
tabsex

# Answer: The cross-tabulation indicates that 32.7% of the loan applicants are of "Joint" sex while 31.5% are males, 20.2% are females and the remaining 
#applicants have not disclosed their sex.

# action taken v/s sex graph

options(scipen=999)
graphsex <- ggplot(total_dat, aes(x = action_taken, fill = derived_sex)) + geom_bar(position =
"dodge")+
xlab("Action taken") + ylab("Count") + ylim(0,230000) + ggtitle("Sex v/s Action taken") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
graphsex

# Answer: The grouped bar chart indicates that there are very few observations for each sex for the "Preapproval request denied" action and the 
#"Preapproval request approved but not accepted" action. The "Loan originated" observations are the most, with number of 'Joint' sex being the highest, 
#followed by males. The "Application denied" observations are mostly experienced by males, followed by "Joint" sex and females.

# d. Lien Status

#renaming lien status

total_dat$lien_status <- revalue(as.character(total_dat$lien_status), c("1" = "Secured by a first
lien" , "2" = " Secured by a subordinate lien"))
View(total_dat)

# action taken v/s lien table

tablien <- tab_xtab(var.row = total_dat$action_taken, var.col = total_dat$lien_status,
title = "Cross Table: Action taken v/s Lien status", show.row.prc = TRUE)
tablien

# Answer: The cross-tabulation indicates that 88.5% of the applicants are secured by a first lien while 11.5% are secured by a subordinate lien.

# action taken v/s lien status graph

options(scipen=999)
graphlien <- ggplot(total_dat, aes(x = action_taken, fill = lien_status)) + geom_bar(position =
"dodge")+
xlab("Action taken") + ylab("Count") + ylim(0,160000) + ggtitle("Lien status v/s Action taken") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + coord_flip()
graphlien

# Answer: The grouped bar chart indicates that there are very few observations for each lien status for the "Preapproval request denied" action and the 
#"Preapproval request approved but not accepted" action. The "Application denied" observations are mostly (>100,000) secured by a first lien as compared to a 
#subordinate lien (< 50,000).

# Q6. Now examine the interaction of multiple variables. Please provide a statistical and graphical representation of the impact of the following variable 
#pairs on “Application Denied” or “Preapproval request denied”?

#a. Race and Reason for Denial: 1=”Credit History”

total_dat$applicant_ethnicity_1 <- revalue(as.character(total_dat$applicant_ethnicity_1), 
                                           c("1" = "American Indian or Alaska Native",
                                                                                         
                                             "2" = "Asian", "21" = "Asian Indian", "22" = "Chinese",
                                                                                            
                                             "23" = "Filipino", "24" = "Japanese", "25" = "Korean",
                                                                                            
                                             "26" = "Vietnamese", "27" = "Other Asian", "3" = "Black or African American", 
                                             "4" = "Native Hawaiian or Other Pacific Islander", 
                                                                                            
                                             "41" = "Native Hawaiian",
                                                                                            
                                             "42" = "Guamanian", "43" = "Samoan", "44" = "Other Pacific Islander", "5" = "White", 
                                             "6" = "Information not provided by applicant", "7" = "Not applicable",
                                                                                            
                                             "11" = "Mexican", "12" = "Puerto Rican", "13" = "Cuban", "14" = "Other Hispanic or Latino"))

# filtering with application denied:

action1 <- c("Application denied")
race_denial <- total_dat %>%
select(derived_race, action_taken, denial_reason_1) %>%
filter(denial_reason_1 == 3 & action_taken == "Application denied")

# statistical representation 

table(race_reason_df$derived_race)

# graphical representation:

plot_race_reason <- ggplot(race_reason_df,
aes(x=derived_race)) +
geom_bar(color = "black", fill= "light blue") + xlab("Race") + ylab("Count") + ylim(0,35000) +
ggtitle("Impact of race and credit history on application denied") +
geom_text(stat='count', aes(label=..count..), vjust = -0.5, size = 4) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_race_reason

# credit history for pre-approval request denied:

race_reason_df_1 <- total_dat %>%
filter(action_taken=="Preapproval request denied" &
denial_reason_1=="Credit history") %>%
group_by(derived_race, denial_reason_1) %>%
select(derived_race, denial_reason_1, action_taken)
View(race_reason_df_1)

# statistical representation:

table(race_reason_df_1$derived_race)

# graphical representation:

plot_race_reason_1 <- ggplot(race_reason_df_1,
aes(x=`derived_race`)) +
geom_bar(color = "black", fill= "light blue") + xlab("Race") + ylab("Count") +
ylim(0,200) +
ggtitle("Impact of race and credit history on preapproval request denied") +
geom_text(stat='count', aes(label=..count..), vjust = -0.5, size = 4) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_race_reason_1

#b. Sex and Reason for Denial: 1=” Debt-to-income ratio”

sex_reason_df <- total_dat %>%
filter(action_taken=="Application denied" &
denial_reason_1=="Debt-to-income ratio") %>%
group_by(derived_sex, denial_reason_1) %>%
select(derived_sex, denial_reason_1, action_taken)
View(sex_reason_df)

# statistical representation:

table(sex_reason_df$derived_sex)

# graphical representation:

plot_sex_reason <- ggplot(sex_reason_df,
aes(x=`derived_sex`)) +
geom_bar(color = "black", fill= "light pink") + xlab("Sex") + ylab("Count") +
ylim(0,20000) +
ggtitle("Impact of sex and debt-to-income ratio on application denied") +
geom_text(stat='count', aes(label=..count..), vjust = -0.5, size = 4) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_sex_reason

# debt-to-income ratio for pre-approval request denied:

sex_reason_df_1 <- total_dat %>%
filter(action_taken=="Preapproval request denied" &
denial_reason_1=="Debt-to-income ratio") %>%
group_by(derived_sex, denial_reason_1) %>%
select(derived_sex, denial_reason_1, action_taken)
View(sex_reason_df_1)

# statistical representation:

table(sex_reason_df_1$derived_sex)

# graphical representation:

plot_sex_reason_1 <- ggplot(sex_reason_df_1,
aes(x=`derived_sex`)) +
geom_bar(color = "black", fill= "light pink") + xlab("Sex") + ylab("Count") +
ylim(0,60) +
ggtitle("Impact of sex and debt-to-income on preapproval request denied") +
geom_text(stat='count', aes(label=..count..), vjust = -0.5, size = 4) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_sex_reason_1

# Q7. Create a new dataset that is summary statistics by year and county. Format each variable as a percentage to two decimal places. It should include:
# a. Average loan amount per county.

options(digits = 4)

# renaming loan purpose for part c:

total_dat$loan_purpose <- revalue(as.character(total_dat$loan_purpose), c("1" = "Home
purchase" ,
"2" = "Home improvement",
"31" = "Refinancing",
"32" = "Cash-out refinancing",
"4" = "Other purpose",
"5" = "Not applicable"))
county_data <- total_dat %>%
group_by(activity_year, county_code)
View(county_data)
avgloan_data <- county_data %>%
summarise_at(.vars = "loan_amount", .funs = mean)
avgloan_data <- as.data.frame(avgloan_data)
colnames(avgloan_data) <- c("activity_year", "county_code", "avg_loan_amount")
View(avgloan_data)

#b. Percentage of actions taken for loan origination (Loan originated versus all other values).

install.packages("janitor")
library(janitor)

# to get a data frame with number of actions per county by year

all <- county_data %>%
tabyl(county_code, activity_year, action_taken)
all$county_code <- as.numeric(all$county_code)
bin <- cbind(all$`Application denied`, all$`Application withdrawn by applicant`, all$`File closed
for incompleteness`, all$`Loan originated`, all$`Preapproval request approved but not
accepted`, all$`Preapproval request denied`, all$`Purchased loan`, all$` Application approved
but not accepted`)
bin <- bin %>%
mutate(sum_2018 = rowSums(bin[, c(2,5,8,11,14,17,20,23)]), sum_2019 = rowSums(bin[,
c(3,6,9,12,15,18,21,24)])) %>%
select(county_code, sum_2018, sum_2019)

# to get a data frame with number of 'Loan originated' per county

a1 <- all$`Loan originated`

# to combine the two data frames and delete the unwanted columns

comb <- cbind(bin, a1)
comb <- comb[, c(-4)]

# to find the percentage of actions taken for loan origination

options(scipen=999)
combfinal <- comb %>%
mutate(percent_loan_2018 = percent(`2018`/sum_2018), percent_loan_2019 =
percent(`2019`/sum_2019))
View(combfinal)

#c. Percentage of loans that are for home purchase.

# to get a data frame with number of loan purposes per county by year

all_purpose <- county_data %>%
tabyl(county_code, activity_year, loan_purpose)
all_purpose$county_code <- as.numeric(all_purpose$county_code)
bin_purpose <- cbind(all_purpose$`Home purchase`, all_purpose$`Home improvement`,
all_purpose$Refinancing, all_purpose$`Cash-out refinancing`,
all_purpose$`Other purpose`, all_purpose$`Not applicable`)
bin_purpose <- bin_purpose %>%
mutate(sum_purpose_2018 = rowSums(bin_purpose[, c(2,5,8,11,14,17)]),
sum_purpose_2019 = rowSums(bin_purpose[, c(3,6,9,12,15,18)])) %>%
select(county_code, sum_purpose_2018, sum_purpose_2019)
View(bin_purpose)

# to get a data frame with number of loans for 'Home purchase' per county

a2 <- all_purpose$`Home purchase`

# to combine the two data frames and delete the unwanted columns

comb_purpose <- cbind(bin_purpose, a2)
comb_purpose <- comb_purpose[, c(-4)]

# to find the percentage of loans that are for home purchase

options(scipen=999)
combfinal_purpose <- comb_purpose %>%
mutate(percent_loan_purpose_home_2018 = percent(`2018`/sum_purpose_2018),
percent_loan_purpose_home_2019 = percent(`2019`/sum_purpose_2019))
View(combfinal_purpose)

#d. Percentage of loans that are for refinancing.

# to get a data frame with number of loans for 'Refinancing' per county

a3 <- all_purpose$Refinancing

# to combine the two data frames and delete the unwanted columns

comb_purpose_2 <- cbind(bin_purpose, a3)

comb_purpose_2 <- comb_purpose_2[, c(-4)]

# to find the percentage of loans that are for refinancing

options(scipen=999)
combfinal_purpose_2 <- comb_purpose_2 %>%
mutate(percent_loan_purpose_refinance_2018 = percent(`2018`/sum_purpose_2018),
percent_loan_purpose_refinance_2019 = percent(`2019`/sum_purpose_2019))
View(combfinal_purpose_2)

#e. Number of loan approvals

loan_approval <- county_data %>%
filter(action_taken == "Purchased loan") %>%
summarise_at(.vars = "action_taken", .funs = count)
loan_approval$freq <- loan_approval$action_taken[2]
loan_approval <- loan_approval[, -3]
colnames(loan_approval) <- c("activity_year", "county_code", "loan_freq")
loan_approval$loan_freq
View(loan_approval)

#Q8. Merge the county by year level summary statistics above onto the main dataset. (each individual loan application row should have new columns 
#with overall percentages for the county and year)

# selecting relevant columns from the above summary stats & merging:
# part 7a:

trial <- total_dat
trial <- merge(trial, avgloan_df, by=c("county_code", "activity_year"))

# part 7b:

combfinal <- combfinal %>%
select(county_code, percent_loan_2018, percent_loan_2019)
trial <- merge(trial, combfinal, by = "county_code")

# part 7c:

combfinal_purpose <- combfinal_purpose %>%
select(county_code, percent_loan_purpose_home_2018, percent_loan_purpose_home_2019)
trial <- merge(trial, combfinal_purpose, by = "county_code")

# part 7d:

combfinal_purpose_2 <- combfinal_purpose_2 %>%
select(county_code, percent_loan_purpose_refinance_2018,
percent_loan_purpose_refinance_2019)
trial <- merge(trial, combfinal_purpose_2, by = "county_code")

# part 7e:

trial <- merge(trial, loan_approval, by = c("county_code", "activity_year"))
trial <- filter(trial, county_code != "0NA")
head(trial)

#Q9. Download the Q4 2019 and Q4 2018 data from the FRED ( https://geofred.stlouisfed.org) (use Q4 data if annual is not provided)

setwd("C:\\Users\\tanur\\Downloads")

#a. Equifax Subprime Credit data

ESC19 <- read_excel("Q19Equifax Subprime Credit.xls") #QUARTERLY
ESC18 <- read_excel("Q18Equifax Subprime Credit.xls") #QUARTERLY

#b. Burdened Households

BH <- read_excel("Burdened Households (1).xls") #QUARTERLY

#c. Housing Inventory: Active Listing Count

HI19 <- read_excel("GeoFRED_Housing_Inventory__Active_Listing_Count_by_County_Level-2.xls")

#QUARTERLY

HI18 <- read_excel("GeoFRED_Housing_Inventory__Active_Listing_Count_by_County_Level.xls")

#QUARTERLY

#d. Number of Private Establishments for All Estimates

NPE19 <- read_excel("Q19Number of Private Establishments.xls") #QUARTERLY
NPE18 <- read_excel("Q18Number Private Establishments .xls") #QUARTERLY

#Q10. Limit the FRED data to the state you are assigned making a new variable from the first two characters of the FIPS code.

#Burdened Households

colnames(BH) <- c("Burdened_households_5_year_estimate_by_county_%",
"Region_Name", "county_code", "2018", "2019")
BH <- BH [,-1]
BH <- BH [-1,]
BH18 <- BH %>%
select(Region_Name, county_code, `2018`, `2019`) %>%
mutate(
`2019` = NULL,
activity_year = 2018)
colnames(BH18) <- c("Region_Name", "county_code", "Burdened_Households", "activity_year")
BH19 <- BH %>%
select(Region_Name, county_code, `2018`, `2019`) %>%
mutate(
`2018` = NULL,
activity_year = 2019)
colnames(BH19) <- c("Region_Name", "county_code", "Burdened_Households", "activity_year")
total_BH <- rbind(BH18, BH19)
total_BH <- total_BH[grep("IL$", total_BH$Region_Name),]

# Adding a 0 to county code:

total_BH$county_code <- paste0("0", total_BH$county_code)
View(total_BH)

#Equifax Subprime Credit

ESC19 <- ESC19 [-1,]
ESC19 <- ESC19 [,-1]
ESC19 <- cbind(ESC19,activity_year=c("2019"))
colnames(ESC19) <- c("Region_Name", "county_code",
"Equifax_subprime_credit", "activity_year")
ESC18 <- ESC18 [,-1]
ESC18 <- ESC18 [-1,]
ESC18 <- cbind(ESC18,activity_year=c("2018"))
colnames(ESC18) <- c("Region_Name", "county_code",
"Equifax_subprime_credit", "activity_year")
total_ESC <- rbind(ESC18, ESC19)
total_ESC <- total_ESC[grep("IL$", total_ESC$Region_Name),]

# Adding a 0 to county_code

total_ESC$county_code <- paste0("0", total_ESC$county_code)
View(total_ESC)

#Housing Inventory:

HI19 <- HI19 [,-1]
HI19 <- HI19 [-1,]
HI19 <- cbind(HI19,activity_year=c("2019"))
colnames(HI19) <- c("Region_Name"," county_code", "Housing_Inventory", "activity_year")
HI18 <- HI18 [,-1]
HI18 <- HI18 [-1,]
HI18 <- cbind(HI18,activity_year=c("2018"))
colnames(HI18) <- c("Region_Name"," county_code", "Housing_Inventory", "activity_year")
total_HI <- rbind(HI18, HI19)
total_HI <- total_HI[grep("IL", total_HI$Region_Name),]

# Adding a column of county_code:

total_HI$` county_code` <- paste0("0", total_HI$` county_code`)
View(total_HI)

#Number of Private Establishments for all estimates:

NPE19 <- NPE19 [,-1]
NPE19 <- NPE19 [-1,]
colnames(NPE19) <- c("Region_Name", "county_code", "Number_of_private_establishments")
NPE19 <- cbind(NPE19,activity_year=c("2019"))
NPE18 <- NPE18 [,-1]
NPE18 <- NPE18 [-1,]
colnames(NPE18) <- c("Region_Name", "county_code", "Number_of_private_establishments")
NPE18 <- cbind(NPE18,activity_year=c("2018"))
total_NPE <- rbind(NPE18, NPE19)
total_NPE <- total_NPE[grep("IL$", total_NPE$Region_Name),]

# Adding a 0 to county_code:

total_NPE$county_code <- paste0("0", total_NPE$county_code)
View(total_NPE)

#Q11. Append the 2019 and 2018 FRED data for only your state into a single file.

#a. If your dataset contains statewide estimates then delete the statewide observations.

data_all <- Reduce(function(...) merge(..., all=TRUE), list(total_BH, total_ESC, total_HI,
total_NPE))
view(data_all)
TOTAL <- data_all[, -6]
View(TOTAL)

#12. Download the Q4 2019 and Q4 2018 Census data for your state by county either from the Census website or the FRED. You are welcome to use the 
#CensusAPI package in R to get this data. The variables you need are:
#a. Population
#b. Median income
#c. Percentage below poverty line
#d. High school graduation rates
                   
install.packages("censusapi")
library(censusapi)
setwd("C:\\Users\\tanur\\Downloads")
df <- getCensus(name = "acs/acs5/profile", vintage = 2018,
vars = c("DP02_0086E", "DP03_0092E", "DP03_0119PE",
"DP02_0061PE"),
key = "f7cdd05569fdbe180b165f83f1fe7100c3c7c185",
region = "county:*", regionin = "state:17")
df$state <- paste(0, df$state, sep="")
df$county <- paste(df$state, df$county, sep="")
df[,1] <- "2018"
colnames(df) <- c('activity_year','county_code', 'Total_population',
'Median_earnings_for_workers',
'Percentage_below_poverty_line', 'Highschool_graduation_rates')
View(df)
                   
df1 <- getCensus(name ='acs/acs5/profile', vintage = 2019,
vars = c("DP02_0086E", "DP03_0092E", "DP03_0119PE",
"DP02_0061PE"),
key = "f7cdd05569fdbe180b165f83f1fe7100c3c7c185",
region = "county:*", regionin = "state:17")
df1$state <- paste(0, df1$state, sep="")
df1$county <- paste(df1$state, df1$county, sep="")
df1[,1] <- "2019"
colnames(df1) <- c('activity_year','county_code', 'Total_population',
'Median_earnings_for_workers',
'Percentage_below_poverty_line', 'Highschool_graduation_rates')
View(df1)
                   
dat_14 <- rbind(df, df1)

#Q13. Download from Census the ACS 5 year estimates ending in 2017 and 2018 by county on internet access “Households with a Broadband Internet Subscription”. 
#This variable is not in the FRED but is available numerous ways from the Census bureau. This is the only one I am asking you to find on your own. 
#Merge the data onto the main data set.
                   
f <- file.choose('ACSDT1Y2018.B28011_data_with_overlays_2021-04-03T210121')
df_2018 <- read.csv(f, header = FALSE )
colnames(df_2018) <- df_2018[2,]
df_2018 <- df_2018[-(1:2),]
df_2018$id <- substr(df_2018$id,10,14)
df_2018$id <- paste("0",df_2018$id,sep="")
df_2018[,2] <- df_2018[,1]
df_2018[,1] <- "2018"
df_2018 <- df_2018[,-(3:8)]
df_2018 <- df_2018[,-(4:12)]
colnames(df_2018) <- c('activity_year','county_code',
'households_witha_broadband_internet_subscription')
View(df_2018)

f1 <- file.choose('ACSDT1Y2019.B28011_data_with_overlays_2021-04-04T165804')
df_2019 <- read.csv(f1, header = FALSE, skipNul = TRUE)
colnames(df_2019) <- df_2019[2,]
df_2019 <- df_2019[-(1:2),]
df_2019$id <- substr(df_2019$id,10,14)
df_2019$id <- paste("0",df_2019$id,sep="")
df_2019[,2] <- df_2019[,1]
df_2019[,1] <- "2019"
df_2019 <- df_2019[,-(3:8)]
df_2019 <- df_2019[,-(4:12)]
df_2019 <- df_2019[-24,]
colnames(df_2019) <- c('activity_year','county_code',
'households_witha_broadband_internet_subscription')
View(df_2019)
                   
dat_total <- rbind(df_2018, df_2019)                   
                   
# 14. Limit the Census data to the state you are assigned and append 2019 and 2018 into a single file.
                        
q14 <- merge(dat_total, dat_14, by = c("county_code", "activity_year"))
head(q14)                   
                   
# 15. Merge all three files by county FIPS code and year.
# a. Each file should have two observations/rows for each county, a 2019 and 2018 value.
# b. Please be sure to identify any observations that did not merge correctly. Why did they not merge?
# c. If your dataset contains statewide estimates then delete the statewide observations.
                   
q15 <- merge(q14, trial, by = c("county_code", "activity_year"))
q15 <- merge(q15, TOTAL, by = c("county_code", "activity_year"))                   
                   
# Q16. Create the following new variables:
# a. A new variable “Poverty” with values of “Low” if the poverty rate is below 20%, “Middle” if the value is between 20% and 30%, and “High” above 30%.
                   
load("q15.Rda")
q16 <- q15
q16 <- q16 %>%
mutate(Poverty = ifelse(Percentage_below_poverty_line < 20, "Low",
ifelse((Percentage_below_poverty_line >= 20) & (Percentage_below_poverty_line
<= 30), "Middle",
ifelse(Percentage_below_poverty_line > 30, "High"))))                   
                   
# b. Create a new variable that is the number of loans per capita (total divided by population.
                   
q16 <- merge(q16, loan_approval, by = c("activity_year", "county_code"))
q16 <- q16 %>%
mutate(loan_per_capita = loan_freq/Total_population)
q16 <- q16 %>%
mutate(loan_per_capita = loan_approval_freq)                   
                   
# Q17. Create the following charts: a. Provide a histogram or kernel density plot of the Households with a Broadband Internet Subscription and
#Housing Inventory: Active Listing Count. Describe the relationships between the mean and median estimates.
#i. Add a normal distribution to the plot. Does the distribution appear normally distributed?
#ii. Please make sure that the histogram and density plot are different colors and clearly labeled.
                   
q17 <- q16
q17$unique_HI <- unique(q17$Housing_Inventory)
HI <- unique(q17$Housing_Inventory)
HI <- as.data.frame(HI)
internet <- unique(q17$households_witha_broadband_internet_subscription)
internet <- as.data.frame(internet)
HI_internet <- cbind(HI, internet)
                   
# relationships between mean and median
                   
mean(HI_internet$HI)
median(HI_internet$HI)
                   
# The mean of houses with broadband subscription is 129984 while the median is 48363. This shows that the distribution does not follow normal 
#(as mean is not equal to median). In fact, it is skewed to the right as the median < mean.
                   
mean(HI_internet$internet)
median(HI_internet$internet)
                   
# The mean of housing inventory (active listing count) is 1965 while the median is 876.5, this shows that the distribution does not follow normal
#(as mean is not equal to median). In fact, it is skewed to the right as the median < mean.
                   
options = scipen(9999)
p <- ggplot() +
geom_histogram(data = HI_internet, aes(x = HI, fill = "r"), alpha = 0.3) +
geom_histogram(data = HI_internet, aes(x = internet, fill = "b"), alpha = 0.3) +
scale_fill_manual(name ="Groups", values = c("r" = "red", "b" = "blue"), labels=c("b" =
"Broadband Internet Subscription", "r" = "Household Inventory")) +
labs(x = "Broadband Internet Subscription & Household Inventory", title = "Histograms of
Household Inventory & Households with Broadband Internet Subscription")

# b. Provide a grouped bar chart of percentage loan refinancing by year with the poverty class variable created above as the grouping variable.
# i. Make sure the groups are labeled clearly and colors are used to distinguish between groups.
# ii. Label the top of the bars with the values.

q17 <- merge(q17, combfinal_purpose_2, by = "county_code")
q17b <- q17 %>%
select(county_code, Poverty)
q17b <- unique(q17b)
merge_q17 <- merge(q17b, combfinal_purpose_2, by = "county_code")
merge_q17 <- merge_q17 %>%
group_by(Poverty)
merge_q17_test <- merge_q17 %>%
mutate(refinance_2018 = "2018", refinance_2019 = "2019", poverty_2 = "Low")
s <- merge_q17_test %>%
select(refinance_2018, refinance_2019)
s <- s[, -1]
s <- s %>%
mutate(year = rbind(refinance_2018, refinance_2019))
s <- merge_q17_test %>%
select(refinance_2018, refinance_2019, county_code)
s <- melt(s, id.vars = "county_code")
s <- s[, -2]
t <- merge_q17_test %>%
select(Poverty, poverty_2, county_code)
t <- melt(t, id.vars = "county_code")
t <- t[, -2]
u <- merge_q17_test %>%
select(percent_loan_purpose_refinance_2018, percent_loan_purpose_refinance_2019,
county_code)
u <- u[, -1]
u <- melt(u, id.vars = "county_code")
u <- u[, -2]
v <- cbind(s, t, u)
v <- v[, -c(3, 5)]
colnames(v) <- c("county_code", "year", "poverty", "percentage_refinance")
plot1 <- v %>%
ggplot(aes(x = year, y = percentage_refinance, fill = poverty)) +
geom_bar(stat = "identity")

# Q18. Subset the data to only 2019. Calculate the following correlation matrix:
#a. Take the log of all of the variables you have added (i.e. Burdened Households, Equifax Subprime Credit, housing inventory, population, 
#poverty, average loan amount,…).
#i. If you have negative values in the variable, you need to add a constant to all values to bring the total above zero.
#1. i.e. if you have 5 observations with less than zero [-1, -2, -.5, -3, -1.5] you would add 3 to all values so that the relative scale stays the same.

# question 18 part a

new_2019 <- q15 %>%
filter(activity_year == "2019") %>%
select(county_code, activity_year, Burdened_Households, Equifax_subprime_credit,
Housing_Inventory, Number_of_private_establishments,
households_witha_broadband_internet_subscription) %>%
unique()
new_2019$households_witha_broadband_internet_subscription <-
as.numeric(new_2019$households_witha_broadband_internet_subscription)
new_2019$Burdened_Households <- as.numeric(new_2019$Burdened_Households)
new_2019$Equifax_subprime_credit <- as.numeric(new_2019$Equifax_subprime_credit)
new_2019$Housing_Inventory <- as.numeric(new_2019$Housing_Inventory)
new_2019$Number_of_private_establishments <-
as.numeric(new_2019$Number_of_private_establishments)
new_2019 <- new_2019 %>%
mutate(log_burdened_households = log(Burdened_Households),
log_equifax_subprime = log(Equifax_subprime_credit),
log_housing_inventory = log(Housing_Inventory),
log_pvt_establishments = log(Number_of_private_establishments),
log_broadband = log(households_witha_broadband_internet_subscription))
head(new_2019)
                   
# b. Correlation matrix comparing all of the variables you have added to loan amount.
#i. Please round all the correlations to two decimal places in the software.
# question 18 part b

new_2019$county_code <- as.numeric(new_2019$county_code)
avgloan_data$county_code <- as.numeric(avgloan_data$county_code)
avg_loan <- avgloan_data %>%
filter(activity_year == "2019")
avg_loan$county_code <- as.numeric(avg_loan$county_code)
avg_loan <- avg_loan[c(12,21,27,30,33,56,57,58,60,61,67,68,69,71,83,92,93,95,101,
103,110,111,112),]
avg_loan_corr_df <- merge(new_2019, avg_loan, by = "county_code")
avg_loan_corr_df <- avg_loan_corr_df[,c(-13)]
head(avg_loan_corr_df)                   
                   
q18b <- avg_loan_corr_df %>%
select(Burdened_Households, Equifax_subprime_credit,
households_witha_broadband_internet_subscription, Housing_Inventory,
Number_of_private_establishments, avg_loan_amount)
corr_matrixb <- cor(q18b)
round(corr_matrixb,2)

# c. Correlation matrix comparing all of the logged variables to loan amount.
#i. Please round all the correlations to two decimal places in the software.
# question 18 part c

q18c <- avg_loan_corr_df %>%
select(log_burdened_households, log_equifax_subprime, log_housing_inventory,
log_pvt_establishments, log_broadband, avg_loan_amount)
corr_matrixc <- cor(q18c)
round(corr_matrixc,2)
corr_matrixc
                   
# Q19. Plot three scatterplots of the 3 strongest correlations with loan value you achieve in the previous question placing the average 
#loan value on the y axis and the correlated variable on the X axis.
# a. Does the graph support a linear relationship between the two variables?
# b. Title and label your graph appropriately.
# c. Either do years separately in two side by side graphs or place all observations in a single plot and use either color or shapes to separate the two groups.                   
                   
# Year 2018:

new_2018 <- q15 %>%
filter(activity_year == "2018") %>%
select(county_code, activity_year, Burdened_Households, Equifax_subprime_credit,
Housing_Inventory, Number_of_private_establishments,
households_witha_broadband_internet_subscription) %>%
unique()
new_2018$households_witha_broadband_internet_subscription <-
as.numeric(new_2018$households_witha_broadband_internet_subscription)
new_2018$Burdened_Households <- as.numeric(new_2018$Burdened_Households)
new_2018$Equifax_subprime_credit <- as.numeric(new_2018$Equifax_subprime_credit)
new_2018$Housing_Inventory <- as.numeric(new_2018$Housing_Inventory)
new_2018$Number_of_private_establishments <-
as.numeric(new_2018$Number_of_private_establishments)
new_2018 <- new_2018 %>%
mutate(log_burdened_households = log(Burdened_Households),
log_equifax_subprime = log(Equifax_subprime_credit),
log_housing_inventory = log(Housing_Inventory),
log_pvt_establishments = log(Number_of_private_establishments),
log_broadband = log(households_witha_broadband_internet_subscription))
new_2018$county_code <- as.numeric(new_2018$county_code)
avg_loan_2018 <- avgloan_data %>%
filter(activity_year == "2018")
avg_loan_2018$county_code <- as.numeric(avg_loan_2018$county_code)
avg_loan_2018 <- avg_loan_2018[c(62,71,77,80,83,106,107,108,110,111,117,118,119,
121, 133,142,143,145,151,153,160,161,162),]
avg_loan_corr_df_2018 <- merge(new_2018, avg_loan_2018, by = "county_code")
avg_loan_corr_df_2018 <- avg_loan_corr_df_2018[,c(-13)]
head(avg_loan_corr_df_2018)                   
                   
q18b_2018 <- avg_loan_corr_df_2018 %>%
select(Burdened_Households, Equifax_subprime_credit,
households_witha_broadband_internet_subscription, Housing_Inventory,
Number_of_private_establishments, avg_loan_amount)
corr_matrixb_2018 <- cor(q18b_2018)
round(corr_matrixb_2018,2)

q18c_2018 <- avg_loan_corr_df_2018 %>%
select(log_burdened_households, log_equifax_subprime, log_housing_inventory,
log_pvt_establishments, log_broadband, avg_loan_amount)

corr_matrixc_2018 <- cor(q18c_2018)
round(corr_matrixc_2018,2)

# question 19 part a
# The three strongest correlations in the year 2019 were between loan value and logs of housing inventory (0.76), 
#private establishment (0.84) and broadband internet subscribers (0.83)
#The scatter plots are as below:

plot_loan_HI <- ggplot(avg_loan_corr_df, aes(x = log_housing_inventory,
y = avg_loan_amount)) +
geom_point(size=1, shape=1) + scale_y_continuous(labels = comma)+
geom_smooth(method = 'lm') +
labs(x="Log of Housing Inventory", y="Average loan value",
title="Scatterplot of log of Housing Inventory and Loan Value: 2019" )
plot_loan_HI

# The scatter plot supports a fairly linear relationship between the log of housing inventory (active listing count) and 
#the average loan amount for the year 2019.

plot_loan_pvt <- ggplot(avg_loan_corr_df, aes(x = log_pvt_establishments,
y = avg_loan_amount)) +
geom_point(size=1, shape=1) + scale_y_continuous(labels = comma)+
geom_smooth(method = 'lm') +
labs(x="Log of Number of Private Establishments", y="Average loan value",
title="Scatterplot of log of Private Establishments and Loan Value: 2019" )
plot_loan_pvt

# The scatter plot supports a strong linear relationship between the log of Private Establishments and the average loan amount for the year 2019.

plot_loan_broadband <- ggplot(avg_loan_corr_df, aes(x = log_broadband,
y = avg_loan_amount)) +
geom_point(size=1, shape=1) + scale_y_continuous(labels = comma)+
geom_smooth(method = 'lm') +
labs(x="Log of Number of Broadband Subscribed Houses", y="Average loan value",
title="Scatterplot of log of Broadband Subscribed Houses and Loan Value: 2019" )
plot_loan_broadband

# The scatter plot supports a strong linear relationship between the log of houses with 
#broadband subscription and the average loan amount for the year 2019.
# question 19 part c

plot_loan_HI_2018 <- ggplot(avg_loan_corr_df_2018, aes(x = log_housing_inventory,
y = avg_loan_amount)) +
geom_point(size=1, shape=1) + scale_y_continuous(labels = comma)+
geom_smooth(method = 'lm') +
labs(x="Log of Housing Inventory", y="Average loan value",
title="Scatterplot of log of Housing Inventory and Loan Value: 2018" )
plot_loan_HI_2018
plot_loan_pvt_2018 <- ggplot(avg_loan_corr_df_2018, aes(x = log_pvt_establishments,
y = avg_loan_amount)) +
geom_point(size=1, shape=1) + scale_y_continuous(labels = comma)+
geom_smooth(method = 'lm') +
labs(x="Log of Number of Private Establishments", y="Average loan value",
title="Scatterplot of log of Private Establishments and Loan Value: 2018" )
plot_loan_pvt_2018
plot_loan_broadband_2018 <- ggplot(avg_loan_corr_df_2018, aes(x = log_broadband,
y = avg_loan_amount)) +
geom_point(size=1, shape=1) + scale_y_continuous(labels = comma)+
geom_smooth(method = 'lm') +
labs(x="Log of Number of Broadband Subscribed Houses", y="Average loan value",
title="Scatterplot of log of Broadband Subscribed Houses and Loan Value: 2018" )
plot_loan_broadband_2018
# using cowplot to join 2018 and 2019 graphs:
library(cowplot)
HI_combine_plot <- plot_grid(plot_loan_HI, plot_loan_HI_2018)
HI_combine_plot

pvt_combine_plot <- plot_grid(plot_loan_pvt, plot_loan_pvt_2018)
pvt_combine_plot

broadband_combine_plot <- plot_grid(plot_loan_broadband, plot_loan_broadband_2018)
broadband_combine_plot

# Q20. Make a grouped violin demonstrating the distribution of subprime mortgages by the ordinal poverty variable made earlier. 
#Each year should be a different color and the graph should have a clear legend. Use this plot to identify any extreme outlying counties.

q17 %>%
ggplot(aes(x = Poverty, y = (as.numeric(Equifax_subprime_credit)), fill =
as.factor(activity_year))) +
geom_violin(trim = FALSE, adjust = 2) +
labs(x = "Poverty", y = "Equifax Subprime Credit", title = "Violin Plot of the Distribution of
Subprime Mortgages by Poverty",
fill = "Year")

# From the violin plot what we can see is that the outliers lie beyond 30%. This includes
#the following counties below:

q20 <- unique(q17[, c("county_code", "Poverty", "Equifax_subprime_credit")])

q20 %>%
filter(Equifax_subprime_credit > 30)

#For 2018 and 2019 respectively, the county codes that are the outliers are 017163 and 017183.








