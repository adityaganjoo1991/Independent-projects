Field that addresses how computers understand texts is called Natural language processing.

#Enron involved in california energy crisis in 2000-2001. Artificially reduced poweer supply to profit.

#eDiscovery problem: Attorneys manually label some documents, and then text analytics models are used to predict if the rrest of the documents are responsive or not
#FERC publicly released emails from Enron. >600,000 emails, 158 users.
#Dataset will be used for predictive coding.
#Using labeled emails from 2010
# email: text of the message
# responsive: does email related to energy schedules or bids?
 
emails= read.csv("energy_bids.csv",stringsAsFactors=FALSE)
str(emails)
emails$email[1]
#strings as factors = false so that strings not treated as factors
strwrap(emails$email[1])
#strwrap breaks a long string into shorter lines so that it's easier to read


#load the tm package
library(tm)
corpus = Corpus(VectorSource(emails$email))# to apply preprocessing, this is necessary

#convert to lower case
corpus=tm_map(corpus,tolower)

corpus=tm_map(corpus,PlainTextDocument) #Have to run this for further preprocessing

#remove punctuation
corpus= tm_map(corpus, removePunctuation)
corpus= tm_map(corpus,removeWords,stopwords("english")) #a,so,as,because, etc
corpus= tm_map(corpus,stemDocument) #reducing words to their stem,base or root form. Eg- argue,argued,arguing, etc belong to argu stem

#document term matrix building

dtm = DocumentTermMatrix(corpus)#convert to bag of words
#too many terms (22164 terms that have appeared atleast once in 855 documents
#remove terms that don't appear too often using removeSparseTerms function
dtm= removeSparseTerms(dtm, .97) #remove any term that doesn't appear in at least 3% of the documents
labeledTerms= as.data.frame(as.matrix(dtm))
#added response variable for further text analytics
labeledTerms$responsive= emails$responsive

#ready to split data into training and test sets

library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive,0.7)
train= subset(labeledTerms, spl==TRUE)
test =subset(labeledTerms, spl==FALSE)

#CART
library(rpart)
library(rpart.plot)
emailCART= rpart(responsive~., data=train, method="class")
prp(emailCART) #Tree
pred = predict(emailCART, newdata=test)
pred.prob= pred[,2]
table(test$responsive, pred.prob>=0.5)#accuracy of 85.6%

#compare to baseline model. Documents are all non responsive (majority class)
table(test$responsive) #83.65%

#going to assign a higher cost to false negatives. If a document is non responsive but is mistakenly labeled as responsive by the model, it will still be reviewed by attoruneys because all responsive docs are reviewd. So it is not harmful
#however, if a responsive doc is mistakenly labeled as non responsive, we will miss the document entirely. 

predROCR = prediction(pred.prob, test$responsive)
performance(predROCR,"auc")@y.values



