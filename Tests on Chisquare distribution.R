# CHI-SQUARE GOODNESS OF FIT (Chi-squared test for given probabilities)
# H0: The fit between the observed and expected frequencies is good i.e. the coin is unbiased
# H1: The fit between the observed and expected frequencies is not good i.e. the coin is not unbiased
x<-c(0,1,2,3,4,5,6,7,8)
n=length(x)
f<-c(4,12,24,32,1,8,4,1,1)
N<-sum(f);N
mean<-sum(f*x)/N;mean
p=mean/n;p
probabilities=dbinom(x,size=n,prob=0.5)
E=N*probabilities;E
chisq.test(E)
# Since the cal value of Chisq is > the tab value of Chisq for 8 df at 5% LOS, we reject H0.


# CHI-SQUARE FOR INDEPENDENCE OF ATTRIBUTES (Pearson's Chi-squared test)
# H0: The two attributes A and B are independent
# H1: The two attributes A and B are dependent
A<-matrix(c(24,36,42,53,32,44,62,18,48,38,15,10,40,30,15,10),nrow=4,ncol=4,byrow=TRUE)
colnames(A)<-c("B1","B2","B3","B4")
rownames(A)<-c("A1","A2","A3","A4")
chisq.test(A)
# Since the cal value of Chisq is > the tab value of Chisq for 9 df at 5% LOS, we reject H0.