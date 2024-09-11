## NOTES FOR CLASS 3B
## HEATHER KITADA SMALLEY

## STEP 1: Install Packages
#install.packages("tidyverse")

## STEP 2: Calling the library
library(tidyverse)

## STEP 3: Data sets in the Environment
data("OrchardSprays")

## STEP 4: Learning about the data
help("OrchardSprays")
?OrchardSprays

### What does the experiment look like?
ggplot(data=OrchardSprays, aes(x=colpos, y=rowpos, fill=treatment))+
  geom_tile(color="black")  #black outlines

## STOPPED HERE IN CLASS 3A

## STEP 5: Looking at the data
## head: first six rows
head(OrchardSprays)

## tail: last six rows
tail(OrchardSprays)

## View: creates a new tab to see the data
View(OrchardSprays)

## STEP 6: Data structure
str(OrchardSprays)


## STEP 7: Variable Assignment and $ Operator
response<-OrchardSprays$decrease

## what kind of class is this?
class(response)

## there are 4 types of classes
# 1) factors
explanatory<-OrchardSprays$treatment
class(explanatory)
# 2) character strings
my_name<-"heather"
class(my_name)
# 3) 
my_boolean<-TRUE
class(my_boolean)
# 4) 
my_pie<-pi
class(my_pie)
# 5) 
my_int<-13L
class(my_int)

## STEP 8: Graphics in base R
boxplot(decrease~treatment, data = OrchardSprays)

## BONUS: Reorder factors
### Is this the order we want?
OrchardSprays$treatment <- factor(OrchardSprays$treatment, 
                                  levels=c('H', 'G', 'F', 'E', 
                                           'D', 'C', 'B', 'A'))

## Plot again
boxplot(decrease~treatment, data = OrchardSprays)


## STEP 9: Vectors
### vectors are one dimensional arrays
n<-length(response)
n


## STEP 10: Common functions
## how much solution was consumed in the experiment?
sum(response)

## what is the average amount of solution consumed?
mean(response)

## STEP 11: Using variables in operations
sum(response)/n

# verify
mean(response)

