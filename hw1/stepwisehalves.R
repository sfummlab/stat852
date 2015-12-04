prostate <-  read.table("~/stat852/data/Prostate.csv", header=TRUE, sep=",", na.strings=" ")
head(prostate)
# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

# Stepwise selection using "step()".  Uses Information Criterion rather than 
#   t-test p-value for selection. This is mostly irrelevant, because both are based 
#   on RSS and will order the variables the same way for models of a given size.
#   Where it can matter is in the stopping rule.  But for any alpha in the 
#   test-based approach you can find a "k" penalty that gives the same results.
#
# Procedure:
# Fit minimum and maximum model first ("initial" and "final" below)
# In step(), object=SMALLEST MODEL, scope=list(upper=LARGEST MODEL),
#   argument to k gives form of IC to use.  Here we use BIC via log(n).

# First half of data

initial.1 <- lm(data=prostate[which(prostate$set==1),], 
                formula=lpsa~ 1)
final.1 <- lm(data=prostate[which(prostate$set==1),], 
              formula=lpsa~lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45)

step1 <- step(object=initial.1, scope=list(upper=final.1), 
     k = log(nrow(prostate[which(prostate$set==1),])))

# Second half of data

initial.2 <- lm(data=prostate[which(prostate$set==2),], 
                formula=lpsa~ 1)
final.2 <- lm(data=prostate[which(prostate$set==2),], 
              formula=lpsa~lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45)
step2 <- step(object=initial.2, scope=list(upper=final.2),  
     k = log(nrow(prostate[which(prostate$set==2),])))

summary(step1)
summary(step2)


# If you specify no penalty in the IC, you can run the selection all the way to the end.

step(object=initial.1, scope=list(upper=final.1), direction = "forward", k = 0)
step(object=initial.2, scope=list(upper=final.2), direction = "forward", k = 0)
