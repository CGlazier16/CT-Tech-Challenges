# loading any libraries

library(dplyr)
library(ggplot2)
library(SDMTools)
library(ROCR)
library(lattice)
library(plyr)
library(reshape2)

# uploading the loans data
loans <- read.csv("loanStats.csv")

# creating variable to denote inactive loans
loans$inactive <- loans$status == "Fully Paid" | loans$status == "Charged Off" | loans$status=="Default"

loans_inactive <-loans[loans$inactive == 1, ]




#4. What is the average loan amount, average DTI (debt-to-income ratio), and average PTI (loan payment-to-income ratio)?

mean(loans_inactive$loan_amnt) #12809.73
mean(loans_inactive$dti) # 15.497

# need to strip out NA's for Payment to income ratio
loans_inactive2 <- loans_inactive[!(is.na(loans_inactive$payment_inc_ratio)),]

mean(loans_inactive2$payment_inc_ratio) #7.56

#5. What is the most common month for issuing new loans?

loans_inactive2$year <- substr(loans_inactive2$issue_d, 1, 4)

loans_inactive2$month <- substr(loans_inactive2$issue_d, 5, 6)
plot(table(loans_inactive2$month)) #loans that are complete - August

loans$month <- substr(loans$issue_d, 5,6)
table(loans$month) # overall, october

#6. What is the most common day that loans were fully paid off?

#mode(loans_inactive$day of the week)

#7. Plot a bar graph for grades and subgrades. What does this tell you about the Lending Club's current portfolio of loans?
qplot(loans_inactive2$grade)
qplot(loans_inactive2$sub_grade)

#8. Check out the "status" variable. What do each of the categories mean? Which category is the largest?
qplot(loans$status)
table(loans$status)

#a. What does the size of each category imply about the Lending Club's current portfolio of loans?

#vast majority of the loans are paid off, by about a 4:1 margin. but this might 

#b. Is loan status important to your analysis? Why?

# increasingly important, since that is how we are trying to analyze loans ... attempting to find out what could predict a "bad
 # loan

#9. Based on your analysis of the status categories in the previous question, create a flag variable that indicates whether a loan is bad or not.
 
loans2 <- loans_inactive
loans2$bad_loans <- as.factor(loans2$bad_loans)

#a. Analyze bad versus good loans against 5 variables of your choice. Create bar plots for categorical variables and distributions for numerical variables.

# term, purpose, interest rate, loan grade, loan amount

ggplot(data=loans2, aes(x=loans2$term,fill=loans2$bad_loans)) + geom_bar(position="fill") + labs(title="Proportion of Loans in Default by Term",x="Loan Term" ,y="Proportion of Loans in Default") + scale_fill_manual(name="Loans in Default?",breaks=c(1,0),labels=c("Yes","No"),values=c("#00C094","#F8766D"))
ggplot(data=loans2, aes(x=loans2$grade,fill=loans2$bad_loans)) + geom_bar(position="fill") + labs(title="Proportion of Loans in Default by Grade",x="Loan Grade",y="Proportion of Loans in Default") + scale_fill_manual(name="Loans in Default?",breaks=c(1,0),labels=c("Yes","No"),values=c("#00C094","#F8766D"))
ggplot(data=loans2, aes(x=loans2$purpose,fill=loans2$bad_loans)) + geom_bar(position="fill") + labs(title="Proportion of Loans in Default by Purpose",x="Loan Purpose",y="Proportion of Loans in Default") + scale_fill_manual(name="Loans in Default?",breaks=c(1,0),labels=c("Yes","No"),values=c("#00C094","#F8766D"))

bwplot(bad_loans~int_rate, data = loans, main = "Loan Interest Rates by Loan Status", xlab = "Interest Rate (%)")
bwplot(bad_loans~loan_amnt, data = loans, main = "Borrower Income by Loan Status", xlab = "Annual Income ($)")

#b. Of the 5 variables you chose, could any of these cause loans to go bad?

# yes I think that you could use a few of these to gain some predictive insight

#i. If so, why did you choose that variable?

# Term, purpose, interest rate, and loan grade should all be important in predicting loan failures
# they give insight to what risker loans could be, which would fail at a higher rate.

#ii. If not, find a variable that could cause loans to go bad (in your opinion) and graph that variable. Explain why you chose it.