
Sys.time()
Results <- dataGathering (timeInHours = 1, resultsFile = F)

write.table(Results,"C:/Users/Jurgis/Desktop/Github/Automatic-article-Searcher/Potencialus plagiatas/Results.txt", row.names = F)