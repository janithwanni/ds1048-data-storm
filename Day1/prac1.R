data <- read.csv("./credit_card_default_train.csv")
data
summary(data)

colnames(data)

sum(is.na(data$Client_ID))
sum(is.na(data))

library(ggplot2)
##p1
data$Gender <- factor(data$Gender) # converts to a categorical variable
data$NEXT_MONTH_DEFAULT <- factor(data$NEXT_MONTH_DEFAULT) # converts to a categorical variable
p1 <- ggplot(data=data, aes(x=factor(1), stat="bin", fill=Gender)) + geom_bar(position="fill") # Stacked bar chart
p1 <- p1 + ggtitle("Gender by Next Month Default") + xlab("") + ylab("NEXT_MONTH_DEFAULT") # Adds titles
p1 <- p1 + facet_grid(facets=. ~ NEXT_MONTH_DEFAULT) # Side by side bar chart
p1 <- p1 + coord_polar(theta="y") # side by side pie chart
p1

library(gmodels)
##ct1
ct1 <- CrossTable(data$EDUCATION_STATUS ,
                  data$NEXT_MONTH_DEFAULT ,
                  prop.r= FALSE,
                  prop.c=TRUE,
                  prop.chisq=FALSE, 
                  chisq = FALSE,
                  prop.t=TRUE)
b2 <- barplot(t2, ylab="Frequency", xlab="Next Month Default", main="Side-By-Side Bar Chart", col=c("turquoise4", "turquoise3", "turquoise2","turquoise" ), beside=TRUE, width=.3)
b2 <- barplot(t2, ylab="Frequency", xlab="Next Month Default", main="Side-By-Side Bar Chart", col=c("turquoise4", "turquoise3", "turquoise2","turquoise" ), beside=TRUE, width=.3)
b2 <- barplot(t2, ylab="Frequency", xlab="Next Month Default", main="Side-By-Side Bar Chart", col=c("turquoise4", "turquoise3", "turquoise2","turquoise" ), beside=TRUE, width=.3)
b2 <- barplot(t2, ylab="Frequency", xlab="Next Month Default", main="Side-By-Side Bar Chart", col=c("turquoise4", "turquoise3", "turquoise2","turquoise" ), beside=TRUE, width=.3)


##p2
data$Gender <- factor(data$MARITAL_STATUS) # converts to a categorical variable
data$NEXT_MONTH_DEFAULT <- factor(data$NEXT_MONTH_DEFAULT) # converts to a categorical variable
p2 <- ggplot(data=data, aes(x=factor(1), stat="bin", fill=MARITAL_STATUS)) + geom_bar(position="fill") # Stacked bar chart
p2 <- p2 + ggtitle("Marital Status by Next Month Default") + xlab("") + ylab("NEXT_MONTH_DEFAULT") # Adds titles
p2 <- p2 + facet_grid(facets=. ~ NEXT_MONTH_DEFAULT) # Side by side bar chart
p2 <- p2 + coord_polar(theta="y") # side by side pie chart
p2

##ct2
ct2 <- CrossTable(data$AGE , data$NEXT_MONTH_DEFAULT , prop.r= FALSE, prop.c=TRUE,prop.chisq=FALSE, chisq = FALSE
                  ,prop.t=TRUE)
##b1
t1 <- table(data$AGE, data$NEXT_MONTH_DEFAULT, dnn=c("Age", "Next Month Default")) # Creates a contingency table
addmargins(t1) #Displays the table (Not necessary
b1 <- barplot(t1, ylab="Frequency", xlab="Next Month Default", main="Side-By-Side Bar Chart", col=c("turquoise4", "turquoise3", "turquoise2","turquoise" ), beside=TRUE, width=.3)

legend("right", title="Age", legend= sort(unique(data$AGE)), fill =c("turquoise4","turquoise3", "turquoise2", "turquoise" ), box.lty=0)

##
ct3 <- xtabs(~data$PAY_JULY+data$NEXT_MONTH_DEFAULT)
 ct4 <- xtabs(~data$PAY_AUG+data$NEXT_MONTH_DEFAULT)
 ct5 <- xtabs(~data$PAY_SEP+data$NEXT_MONTH_DEFAULT)
 ct6 <- xtabs(~data$PAY_OCT+data$NEXT_MONTH_DEFAULT)
ct7 <- xtabs(~data$PAY_NOV+data$NEXT_MONTH_DEFAULT)
 ct8 <- xtabs(~data$PAY_DEC+data$NEXT_MONTH_DEFAULT)
ct3
ct4
ct5
ct6
ct7
ct8

