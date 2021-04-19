'''
Guided Project: Building a Spam Filter with Naive Bayes

Introduction:

The goal of this project is to create a model program that allows the user to differenciate between spam and non-spam SMS messages given 5572
SMS messages provided.

'''
library(tidyverse)
library(readr)
library(dplyr)
set.seed(1)

spam <- read.csv('~/Google Drive/Work/Data Analyst in R/Guided Projects/Spam Filter/SMSSpamCollection',
                sep = '\t',
                header = FALSE,
                stringsAsFactors = FALSE)
colnames(spam) <- c('label', 'sms')

spam_range <- 1:nrow(spam) #spam has 3184 rows
#spam_cols <- 1:ncol(spam) #spam has 2 columns

#head(spam, 10)

#lbl_percs <- spam %>%
#  group_by(label) %>%
#  summarize(Freq = n()) %>%
#  mutate(Percentage = Freq / nrow(spam) * 100)

#86.2% of the dataframe spam has ham SMS, while 13.8% are spam

'''
2. Training, Cross-validation and Test Sets
'''
dummy_range <- spam_range

non_dupli <- function(sample, list) { #removes numbers from a list of integers to prevent duplication
  sample <- sort(sample)
  n <- list[-sample]
}

train <- sample(dummy_range, 2547, replace = FALSE)
cross <- sample(dummy_range <- non_dupli(train, dummy_range),
                318, replace = FALSE)
test <- sample(dummy_range <- non_dupli(cross, dummy_range),
                319, replace = FALSE)

#total <- sort(c(train, cross, test))
#spam_range == total #Both the spam_range and 3 sets match up.

spam.training <- spam[train,]
spam.cross_valid <- spam[cross,]
spam.testing <- spam[test,]

'''
3. Data Cleaning
'''

tidy.training <- spam.training %>%
  mutate(sms = tolower(str_replace_all(sms, "(([!\\#$%&'()*+,\\-./:;<=>?@\\[\\]^_‘{|}~£])+|(\u0092s)|[0-9]+)", '')))


'''
4.Creating the Vocabulary
'''

vocabulary <- c()
messages <- tidy.training$sms
for (m in messages) {
  word <- str_split(m, ' ')[[1]]
  word <- word[!word %in% '']
  vocabulary <- c(vocabulary, word)
}
vocabulary <- unique(vocabulary)

#Cleaned up the vocabulary so it has only unique words.

'''
5. Calculating Constants First
'''

#First thing is to calculate p(Spam) and p(Ham) from the training set.


lbl_percs.tidy.training <- tidy.training %>%
  group_by(label) %>%
  summarize(Freq = n()) %>%
  mutate(Percentage = Freq / nrow(spam) * 100)

total <- as.integer(lbl_percs.tidy.training[1,2] + lbl_percs.tidy.training[2,2])
p_spam <- as.integer(lbl_percs.tidy.training[2,2])/total
p_ham <- as.integer(lbl_percs.tidy.training[1,2])/total

n_of <- function(df, value) {
  list <- c()
  percs <- df %>%
    filter(label == value)
  for (p in percs$sms) {#can be changed from sms and label if you choose to add another variable.
      word <- str_split(p, ' ')[[1]]
      word <- word[!word %in% '']
      list <- c(list, word)
  }
  return(length(list))
}

n_spam <- n_of(tidy.training, 'spam') #3450
n_ham <- n_of(tidy.training, 'ham') #22020
n_vocabulary <- length(vocabulary) #10573
alpha = 1

'''
6. Calculating Parameters
'''

spam.tidy.training <- tidy.training %>%
  filter(label == 'spam')

ham.tidy.training <- tidy.training %>%
  filter(label == 'ham')

spam_list <- c()
for (s in spam.tidy.training$sms) {
  word <- str_split(s, ' ')[[1]]
  word <- word[!word %in% '']
  spam_list <- c(spam_list, word)
}

ham_list <- c()
for (h in ham.tidy.training$sms) {
  word <- str_split(h, ' ')[[1]]
  word <- word[!word %in% '']
  ham_list <- c(ham_list, word)
}

word_prob_spam <- data.frame(word = NA,
                        n_word_given_spam = NA,
                        p_word_given_spam = NA)

n = 0
for (w in 1:length(vocabulary)) {
  for (s in 1:length(spam_list)) {
    if (vocabulary[w] == spam_list[s]) {
      n = n + 1
    } else {
      next
    }
  }
  p_word_given_spam = (n + alpha)/(n_spam + alpha * n_vocabulary)
  newdf <- c(vocabulary[w], n, p_word_given_spam)
  word_prob_spam <- rbind(word_prob_spam, newdf)
  n = 0
}
word_prob_spam <- word_prob_spam[2:nrow(word_prob_spam),]

word_prob_ham <- data.frame(word = NA,
                             n_word_given_ham = NA,
                             p_word_given_ham = NA)

n = 0
for (w in 1:length(vocabulary)) {
  for (h in 1:length(ham_list)) {
    if (vocabulary[w] == ham_list[h]) {
      n = n + 1
    } else {
      next
    }
  }
  p_word_given_ham = (n + alpha)/(n_ham + alpha * n_vocabulary)
  newdf <- c(vocabulary[w], n, p_word_given_ham)
  word_prob_ham <- rbind(word_prob_ham, newdf)
  n = 0
}
word_prob_ham <- word_prob_ham[2:nrow(word_prob_ham),]

'''
7. Classifying A New Message
'''

sentence <- sample(spam.training$sms, 1)

p_sentence <- function(sentence) {
  sentence <- tolower(str_replace_all(sentence, "(([!\\#$%&'()*+,\\-./:;<=>?@\\[\\]^_‘{|}~£])+|(\u0092s)|[0-9]+)", ''))
  word <- str_split(sentence, ' ')[[1]]
  word <- word[!word %in% '']
  p_spam_given_sentence = 1
  p_ham_given_sentence = 1
  if (identical(word, character(0))) {
    return('ham')
  } else {
    for (w in 1:length(word)) {
      for (p in 1:nrow(word_prob_spam)) {
        if (word[w] == word_prob_spam$word[p]) {
          p_spam_given_sentence <- p_spam_given_sentence * as.numeric(word_prob_spam$p_word_given_spam[p])
        } else {
          next
        }
      }
      for (h in 1:nrow(word_prob_ham)) {
        if (word[w] == word_prob_ham$word[h]) {
          p_ham_given_sentence <- p_ham_given_sentence * as.numeric(word_prob_ham$p_word_given_ham[h])
        } else {
          next
        }
      }
    }
  ifelse(p_spam_given_sentence > p_ham_given_sentence,
           'spam',
           'ham')
  }
}

spam.training <- spam.training %>%
  mutate(classifier = map(sms, p_sentence))

'''
8. Accuracy
'''

n = 0
for (i in 1:nrow(spam.training)) {
  if (spam.training$label[i] == spam.training$classifier[i]) {
    n = n + 1
  } else {
    next
  }
}
avg = n / nrow(spam.training) * 100
print(avg) #the filter is 97.4% accurate.

'''
9. Hyperparameter Tuning and Cross-validation
'''

#Alpha must be changed in order to make a more accurate filter.

#Create an empty data frame for alpha and accuracy values along with a list of alpha values
alpha_grid <- data.frame(alpha = NA, accuracy = NA)
alpha_list <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

word_prob_spam <- data.frame(word = NA,
                             n_word_given_spam = NA,
                             p_word_given_spam = NA)

word_prob_ham <- data.frame(word = NA,
                            n_word_given_ham = NA,
                            p_word_given_ham = NA)

alpha = 1
{
  n = 0
  for (w in 1:length(vocabulary)) {
    for (s in 1:length(spam_list)) {
      if (vocabulary[w] == spam_list[s]) {
        n = n + 1
      } else {
        next
      }
    }
    p_word_given_spam = (n + alpha)/(n_spam + alpha * n_vocabulary)
    newdf <- c(vocabulary[w], n, p_word_given_spam)
    word_prob_spam <- rbind(word_prob_spam, newdf)
    n = 0
  }
  word_prob_spam <- word_prob_spam[2:nrow(word_prob_spam),]
  
  n = 0
  for (w in 1:length(vocabulary)) {
    for (h in 1:length(ham_list)) {
      if (vocabulary[w] == ham_list[h]) {
        n = n + 1
      } else {
        next
      }
    }
    p_word_given_ham = (n + alpha)/(n_ham + alpha * n_vocabulary)
    newdf <- c(vocabulary[w], n, p_word_given_ham)
    word_prob_ham <- rbind(word_prob_ham, newdf)
    n = 0
  }
  word_prob_ham <- word_prob_ham[2:nrow(word_prob_ham),]
  
  spam.cross_valid <- spam.cross_valid %>% mutate(classifier = map(sms, p_sentence))
  
  n = 0
  for (i in 1:nrow(spam.cross_valid)) {
    if (spam.cross_valid$label[i] == spam.cross_valid$classifier[i]) {
      n = n + 1
    } else {
      next
    }
  }
  avg = n / nrow(spam.cross_valid) * 100
  list <- c(alpha, avg)
  alpha_grid <- rbind(alpha_grid, list)
}
#It seems that 0.4 is the best value for alpha given the tests.

'''
10. Test Set Performance
'''

#Time to test out the filter for real now.

alpha = 0.4
spam.testing <- spam.testing %>%
  mutate(classifier = map(sms, p_sentence))

n = 0
for (i in 1:nrow(spam.testing)) {
  if (spam.testing$label[i] == spam.testing$classifier[i]) {
    n = n + 1
  } else {
    next
  }
}
avg = n / nrow(spam.cross_valid) * 100

#I expected the value to be a little bit better, but this where further improvements can be made to make the filter better.