## NOTES FOR CLASS 3A and 3B
## KITADA SMALLEY

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

# PART II: Create an Experimental Design in R

## STEP 12: Giving Id's to Experimental Units
# recall in our example from class there were 24 plants (experimental units)
ids<-1:24
# note that we are storying the output as a vector
ids

# the colon is a way to create a vector of consecutive integers

## try it yourself
### Q1: What happens when you go from a bigger number to a smaller number?
10:1
### Q2: What happens when you go have a postive and negative number?
-3:4

## STEP 13: Organizing the experimental units into rows and columns 
# in the example we saw, the lab bench had 4 rows and 6 columns
labBench<-matrix(ids, nrow=4, ncol=6)
labBench

# this is a matrix!  
# a matrix is a very useful way to store numbers
# there are also special mathematical operations that can be performed on matrices

# let's read the documentation about matrix function to learn about it arguments
?matrix

# the inputs of this function are the data, nrow, and ncol 
### What would happen if the number of columns was omitted?
matrix(ids, nrow=4)
### What would happen if the number of columns times the number of rows was 
### not equal to the number of elements?
matrix(ids, nrow=4, ncol=7)
## non-conformable matrix

## STEP 14:  How can we randomly assign treatments?
# recall there were 4 treatments of fertilizers (including a control)

## Completely randomized design
## choose ID's and randomly assign to treatments
## to do this we will use the sample function 
## learn about the sample function 
?sample

crd_samp<-sample(ids, replace=FALSE)
crd_samp
# what happened? 
## we just mixed up the numbers
## is the order of numbers that same as your neighbor's? 

# here nrow will be the number of treatments
crd_mat<-matrix(crd_samp, nrow=4)
crd_mat

# let's say that
## row 1 = fertilizer A
## row 2 = fertilizer B
## row 3 = fertilizer D
## row 4 = Control 


## IMPORTANT TOOLS
## FOR LOOPS
for(i in 1:5){
  print(i)
}

## %in% operator
1 %in% c(1, 2, 3)
5 %in% c(1, 2, 3)

## STEP 15: Making a map of this design
## here we will learn to create a loop and how to write conditionals
treats<-matrix(nrow=24)
for(i in 1:24){
  if(i %in% crd_mat[1,]){
    treats[i]<-"A"
  }
  if(i %in% crd_mat[2,]){
    treats[i]<-"B"
  }
  if(i %in% crd_mat[3,]){
    treats[i]<-"D"
  }
  if(i %in% crd_mat[4,]){
    treats[i]<-"C"
  }
}

treats

## make the map!
expDes<-matrix(treats, nrow=4)
expDes

## How does this look? 
## Does it appear "random"?

## STEP 16: matrix indexing
# What treatments are to be assigned in row 3?
expDes[3,]
# What treatments are to be assigned in column 4?
expDes[,4]
# What treatments is to be assigned in row 3 column 2?
expDes[3,2]
# If a cat knocked over the plants in the 4th row what would it look like?
expDes[-4,]

## STEP 17: Blocked Design
## can we improve our design?
## blocking means that all treatments are present in each block
## if there is a gradient across columns there are 6 blocks

# we will learn how to concatenate here
# start with an emply list
blockTreats<-c()
for(i in 1:6){
  thisSample<-sample(c("A", "B", "C", "D"), replace=FALSE)
  blockTreats<-c(blockTreats, thisSample)
}

blockDes<-matrix(blockTreats, nrow=4)
blockDes
# Does this look more random?  


