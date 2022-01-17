library(stringr)
require(reshape2)
library(readxl)
companies <- read_excel("List_500_companies.xlsx")

started = Sys.time()
single_com <- c()
for (j in companies$symbol){

  single_com[[j]] <- rjson::fromJSON(file = 
                                    paste0("https://fmpcloud.io/api/v3/quote/",j,"?apikey=d044d11c5bbbc7c89697083850466e50"))
  

  
} 
single_companies <- as.data.frame(unlist(single_com))
single_companies$des <- rownames(single_companies)
rownames(single_companies) <- NULL
single_companies$parameters <- word(single_companies$des,3,sep = "\\.")
single_companies$companies <- paste0(word(single_companies$des,1,sep = "\\."),".NS")
single_companies$des <- NULL
colnames(single_companies) <- c("Values","Parameter","Companies")
single_companies <- single_companies [!(single_companies$Parameter == "symbol" | single_companies$Parameter == "name"),]
single_companies <- dcast(single_companies, Companies~Parameter, value.var = "Values")
single_companies$earningsAnnouncement <- NULL
single_companies$exchange <- NULL
single_companies[2:19] <- lapply(single_companies[2:19], as.numeric)
single_companies$`High-low` <- single_companies$dayHigh - single_companies$dayLow
single_companies$`year high - year low` <- single_companies$yearHigh - single_companies$yearLow
# View(single_companies[c(1,17,20,21,2:16,18:19)])
  
ended = Sys.time()
ended  - started
