ZAJECIA 4 DZREWA DECYZYJNE

1. Biblioteki
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
library(caret) # do oceny wynikĂłw

2. Wczytywanie danych
titanic_fpath <- "https://raw.githubusercontent.com/kaftanowicz/sgh_ird_2017/bae80011e646b910bab577b87019edc2aad67379/data/titanic_full.csv"
tdf <- read.csv(titanic_fpath)

3. Ułamek liczby rekordów przeznaczony do zbioru testowego
test_prop <- 0.25
test_bound <- floor(nrow(tdf)* test_prop)
tdf <- tdf[sample(nrow(tdf)), ]
tdf.test <- tdf[1:test_bound, ]
tdf.train <- tdf[(test_bound+1):nrow(tdf), ]

4. Eksploracja danych

names(tdf) # kategorie 
str(tdf) # co jest w tych kategoriach
table(tdf$survived)  # 0/1 liczbowo
prop.table(table(tdf$survived)) # 0/1 procentowo

5. Budowa drzewa
tree <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
             data=tdf.train,
             method="class",
             control = list(maxdepth = 3))

6. Wiualizacja drzewa 

tree
plot(tree)
text(tree, pretty = TRUE)
rpart.plot(tree, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

7. Interpretacja wyników
prop.table(table(tdf$sex, tdf$survived), 1)
prop.table(table(tdf$sex, tdf$survived), 2)

8. Weryfikacja jakości klasyfikacji

EvaluateClassifier <- function(response_colname, prediction_colname, df,  positive="1")
{
  y <- factor(df[response_colname][[1]]) # factor of positive / negative cases
  predictions <- factor(df[prediction_colname][[1]]) # factor of predictions
  precision <- posPredValue(predictions, y, positive)
  recall <- sensitivity(predictions, y, positive)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  return(list(precision=precision, recall=recall, F1=F1))
}

9. Weryfikacja jakości klasyfikacji - zbiór uczący się i testowy 

EvaluateClassifier <- function(response_colname, prediction_colname, df,  positive="1")
{
  y <- factor(df[response_colname][[1]]) # factor of positive / negative cases
  predictions <- factor(df[prediction_colname][[1]]) # factor of predictions
  precision <- posPredValue(predictions, y, positive)
  recall <- sensitivity(predictions, y, positive)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  return(list(precision=precision, recall=recall, F1=F1))
}

10. Las losowy czyli kombinacja wielu drzew 

set.seed(123)
forest <- cforest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + fare + embarked,
                 data = tdf.train, 
                 controls=cforest_unbiased(ntree=200, mtry=3))

tdf.train$survival_predicted_forest<- predict(forest, tdf.train, OOB=TRUE, type = "response")
tdf.test$survival_predicted_forest<- predict(forest, tdf.test, OOB=TRUE, type = "response")

EvaluateClassifier('survived', 'survival_predicted_forest', tdf.train)
EvaluateClassifier('survived', 'survival_predicted_forest', tdf.test)

11. Przycinanie drzewa
  minimum.error <- which.min(rpart.model$cptable[, "xerror"])
  optimal.complexity <- rpart.model$cptable[minimum.error, "CP"]
  points(minimum.error, rpart.model$cptable[minimum.error, "xerror"],
         col = "red", pch = 19)
  rpart.model.pruned <- prune(rpart.model, cp = optimal.complexity)

12. Porownanie drzewa przed i po przycieciu
rpart.plot(rpart.model, under=FALSE, tweak=1.3, fallen.leaves = TRUE)
rpart.plot(rpart.model.pruned, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

13. Porownanie wynikow metod
confusion.matrix <- list()
print(confusion.matrix[[1]] <- table(predict(ctree.model, new = test.set),
                                     test.set$class))
print(confusion.matrix[[2]] <- table(predict(rpart.model, type = "class",
                                             newdata = test.set),
                                     test.set$class))
print(confusion.matrix[[3]] <- table(predict(rpart.model.pruned, type = "class",
                                             newdata = test.set),
                                     test.set$class))

CalculateAccuracy <- function(confusion.matrix) {
  return(sum(diag(confusion.matrix)) / sum(confusion.matrix))
}

print(data.frame(model = c("ctree.model", "rpart.model", "rpart.model.pruned"),
                 accuracy = sapply(confusion.matrix, CalculateAccuracy)),
      row.names = FALSE)

ZADANIE 1

 W oparciu o przyklad omowiony na wykladzie
(skrypt IRD_2017_lecture_2017-11-07_decision_trees_in_R.R)
zbuduj drzewo decyzyjne przewidujace klase samochodu 
w zaleznosci od jego pozostalych parametrow.
Uzyj 80% rekordow jako zbioru uczacego i 20% jako zbioru testowego.

 a) Biblioteki
install.packages("rpart")
library(rpart) # do drzewa
install.packages("rpart.plot")
library(rpart.plot) # do rysowania drzewa
install.packages("caret")
library(caret) # do oceny wynikĂłw

test_prop <- 0.20
test_bound <- floor(nrow(DATA_SET)* test_prop)
tdf <- DATA_SET[sample(nrow(DATA_SET)), ] # mieszamy losowo kolejnosc wierszy
tdf.test <- tdf[1:test_bound, ]
tdf.train <- tdf[(test_bound+1):nrow(tdf), ]

b) Budowa drzew decyzyjnych

tree3 <- rpart(class ~ buying + maint + doors + persons + lug_boot + safety,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 3))

rpart.plot(tree3, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

tree5 <- rpart(class ~ buying + maint + doors + persons + lug_boot + safety,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 5))

tree5 <- rpart(class ~ .,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 5))

rpart.plot(tree5, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

c) Ewaluacja wynikow

EvaluateClassifier <- function(response_colname, prediction_colname, df,  positive="1")
{
  y <- factor(df[response_colname][[1]]) # factor of positive / negative cases
  predictions <- factor(df[prediction_colname][[1]]) # factor of predictions
  precision <- posPredValue(predictions, y, positive)
  recall <- sensitivity(predictions, y, positive)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  return(list(precision=precision, recall=recall, F1=F1))
}

d) Weryfikacja jakoĹ›ci klasyfikacji
# tree3
tdf.test$prediction3 <- predict(tree3, tdf.test, type = "class")
EvaluateClassifier('class', 'prediction3', tdf.test)
# tree5
tdf.test$prediction5 <- predict(tree5, tdf.test, type = "class")
EvaluateClassifier('class', 'prediction5', tdf.test)


