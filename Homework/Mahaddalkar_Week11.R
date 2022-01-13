################################################
#
# IST687, Homework 11
#
# Student name: Shivani Sanjay Mahaddalkar
# Homework number: 11
# Date due: 11/8/2020
#

# Set working directory 
setwd("C:/Users/shiva/Documents/R") # Change to the folder containing your homework data files

# Your homework specific code goes below here

#Part	1:	Load	and	visualize	the	text	file
#A. Read	in	the	file
charVector	<- scan("TheRoadNotTaken.txt",	character(0),	sep	=	"\n")

#B. Transform	charVector	into	a	document-feature	matrix
library(tm)
library(wordcloud)
library(quanteda)
sentenceCorpus <- corpus(charVector)
sentenceDFM <- dfm(sentenceCorpus, remove_punct=TRUE,remove=stopwords("english"))

#C. Plot	a	word	cloud
textplot_wordcloud(sentenceDFM, min_count	=	1)

#Part	2:	Create	a	sorted	list	of	word	counts	from	the speech
#D.	Create	a	named	list	of	word	counts	by	frequency
m	<- as.matrix(sentenceDFM)
wordCounts <- colSums(m)
wordCounts

#E. Comment on varA
#All of the words that occur in the document occur with the frequency of either one or two.

#Part 3:	Match	the	speech	words	with	positive	and	negative	words
#F. Read in	the	list	of	positive	words and negative words
posWords	<- scan("positive-words.txt",	character(0),	sep	=	"\n")
negWords	<- scan("negative-words.txt",	character(0),	sep	=	"\n")

#G. 	matching	the	words	from	the	speech	(stored	in	wordCounts)	to	the	list	of	positive	words and negative words
#This shows that the there are three words matching indices with the positive words
matchedP	<- match(names(wordCounts),	posWords,	nomatch	=	0)

#This shows that the there are three words matching indices with the negative words
matchedN <- match(names(wordCounts), negWords, nomatch = 0)


#H. Examine	the	contents	of	matchedP
#Every non zero entry is the index of the matching words in the positive words text
matchedP
#Every non zero entry is the index of the matching words in the negative words text
matchedN

#I. Use	R	to	print	out	which	positive	words	were	in	the	speech.
posDF <- data.frame(posWords)
posDF[matchedP,]

#J. Use	R	to	print	out	the	total	number	of	positive	words	were	there	in	the	speech
#This shows that there are 3 positive words in the text
sum(matchedP!=0)

#K. Negative	words in	the	speech
negDF <- data.frame(negWords)
negDF[matchedN,]
#This shows that there are 3 negative words in the text
sum(matchedN!=0)

#L. describing	what	you	found	for	matching	positive	and	negative words
# There are 3 positive and negative words in the text. The positive words are fair, better and leads and negative words are sorry, bent and worn.
#The text is a poem by Robert Frost. As it is a poem heavy with emotions it is difficult to classify it as neutral emotion based on our analysis. 
#The context of the text document is important. 