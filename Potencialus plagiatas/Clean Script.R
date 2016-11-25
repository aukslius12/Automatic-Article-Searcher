
Sys.time()
Results <- dataGathering (timeInHours = 2, resultsFile = F)

write.table(Results,"C:/Users/Jurgis/Desktop/Github/Automatic-article-Searcher/Potencialus plagiatas/Results.txt", row.names = F)