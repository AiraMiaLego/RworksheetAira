##1.Compute the descriptive statistics using different packages (Hmisc and pastecs).
##Write the codes and its result.
library(Hmisc)

students<-c(1:10)
preT<- c(55,54,47,57,51,61,57,54,63,58)
postT<- c(61,60,56,63,56,63,59,56,62,61)

data<- data.frame(
  Student = students,
  PreTest = preT,
  PostTest = postT
)

data
num1<- describe(data)
num1


library(pastecs)

students<-c(1:10)
preT<- c(55,54,47,57,51,61,57,54,63,58)
postT<- c(61,60,56,63,56,63,59,56,62,61)

data2<- data.frame(
  Student = students,
  PreTest = preT,
  PostTest = postT
)

data2
num1a<- stat.desc(data)
num1a

##2. The Department of Agriculture was studying the effects of several levels of a fertilizer on the growth of a plant. For some analyses, it might be useful to convert the fertilizer levels to an ordered factor.
##The data were 10,10,10, 20,20,50,10,20,10,50,20,50,20,10. ##a. Write the codes and describe the result.

fertilizer_level<- c(10,10,10, 20,20,50,10,20,10,50,20,50,20,10)

fertilizer<- ordered(fertilizer_level, levels= c(10,20,50))

fertilizer

cat("The data have been converted to an ordered factor, and the levels are ordered as 10<20<50.")

##3. Abdul Hassan, president of Floor Coverings Unlimited, has asked you to study the exercise levels undertaken by 10 subjects were "l", "n", "n", "i", "l" , "l", "n", "n", "i", "l" ; n=none, l=light, i=intense
##a. What is the best way to represent this in R?
  
exercise_levels <- c("l", "n", "n", "i", "l", "l", "n", "n", "i", "l")
exercise<- factor(exercise_levels, levels = c("n", "l", "i"), labels= c("none", "light", "intense"))

exercise

##4.Sample of 30 tax accountants from all the states and territories of Australia and their individual state of origin is specified by a character vector of state mnemonics as:state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld","vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt","wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw","vic", "vic", "act")
##a. Apply the factor function and factor level. Describe the results.

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld","vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt","wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw","vic", "vic", "act")
state_factor<- factor(state)
state_factor


cat("This shows  the factor levels assigned to each state in the original order. The levels are automatically ordered alphabetically.")

##5. From #4 - continuation: ##Suppose we have the incomes of the same tax accountants in another vector (in suitablylarge units of money) ##incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)
##a. Calculate the sample mean income for each state we can now use the special function tapply():
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)
income_means <- tapply(incomes, state_factor, mean)
income_means

##b. Copy the results and interpret.
cat("act      nsw       nt      qld       sa      tas      vic       wa 
44.50000 57.33333 55.50000 53.60000 55.00000 60.50000 56.00000 52.25000")
cat("Tax accountants from the Australian Capital Territory (act) have a mean income of 44.50.New South Wales (nsw) tax accountants have a mean income of 57.33. Northern Territory (nt) tax accountants have a mean income of 55.50 and so on for the others.\n This analysis provides insights into the average income of tax accountants in each state based on the provided data." )


##7.Use the titanic dataset. ##a. subset the titatic dataset of those who survived and not survived. Show the codes and its result.

titanic<- as.data.frame(Titanic)
survived<- subset(titanic, Survived == 'Yes')
survived
not<- subset(titanic, Survived == 'No')
not

##8.The data sets are about the breast cancer Wisconsin. The samples arrive periodically as Dr. Wolberg reports his clinical cases. The database therefore reflects this chronologihttps://drive.google.com/file/d/16MFLoehCgx2MJuNSAuB2CsBy6eDIIru/view?usp=drive_link) ##Note Kindly click on the word BreastCancer to download the dataset. ##a. describe what is the dataset all about.

  library(readr)
breastcancer_wisconsin <- read.csv("breastcancer_wisconsin.csv")
breastcancer_wisconsin
cat("The data set is all about the collection of data that pertains to breast cancer diagnosis")

##Compute the descriptive statistics using different packages. Find the values of:
##d1.Standard error of the mean for clump thickness.

se_mean_clump_thickness <- sd(breastcancer_wisconsin$clump_thickness) / sqrt(length(breastcancer_wisconsin$clump_thickness))

cat("Standard Error of the Mean for Clump Thickness:", se_mean_clump_thickness, "\n")

##d2. Coefficient of variability for Marginal Adhesion.
cv_marginal_adhesion <- sd(breastcancer_wisconsin$marginal_adhesion) / mean(breastcancer_wisconsin$marginal_adhesion) * 100

cat("Coefficient of Variability for Marginal Adhesion:", cv_marginal_adhesion, "%\n")

##d3. Number of null values of Bare Nuclei.
null_values_bare_nuclei <- sum(is.na(breastcancer_wisconsin$bare_nucleoli))
null_values_bare_nuclei

cat("Number of Null Values of Bare Nuclei:", null_values_bare_nuclei, "\n")

##d4. Mean and standard deviation for Bland Chromatin
mean_bland_chromatin <- mean(breastcancer_wisconsin$bland_chromatin)
sd_bland_chromatin <- sd(breastcancer_wisconsin$bland_chromatin)

cat("Mean for Bland Chromatin:", mean_bland_chromatin, "\n")
cat("Standard Deviation for Bland Chromatin:", sd_bland_chromatin, "\n")

##d5. Confidence interval of the mean for Uniformity of Cell Shape
ci_mean_uniformity_cell_shape <- t.test(breastcancer_wisconsin$shape_uniformity)$conf.int

cat("Confidence Interval of the Mean for Uniformity of Cell Shape:", ci_mean_uniformity_cell_shape, "\n")

##How many attributes?
num_attributes <- ncol(breastcancer_wisconsin)

cat("Number of attributes (columns):", num_attributes, "\n")

