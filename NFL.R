factor2char <- function(df){
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  return(df)
}

getBounds_matrix <- function(obj){
  li <- 1:NROW(obj)
  ui <- 1:NROW(obj)
  lb <- ifelse(obj$lb, obj$lb, 0)
  ub <- ifelse(obj$ub, obj$ub, Inf)
  nobj <- max(li, ui)
  bounds <- V_bound(li = li, ui = ui, lb = lb, ub = ub, nobj = nobj)
  if ((length(bounds$lower$ind) == 0) && (length(bounds$upper$ind) == 0)){
    bounds <- NULL
  }
  bounds
}


#Get NFL Data into Data Frame
cols <- c('player_name','team', 'position','salary','points','ceil','floor','opp','player_id','schedule.data.date')
#Get Position Players
nflflexURL <- paste('https://rotogrinders.com/projected-stats/nfl-flex?site=fanduel&date=',Sys.Date(),sep = '')
nflflex <- read_html(nflflexURL)
nflflex <- html_text(html_nodes(nflflex, "body > div > div > section > div > script"))
nflflex <- sub('.[^]]+$', '', nflflex)
nflflex <- sub('.[^]]+$', '\\]', nflflex)
nflflex <- sub("^.*?\\[","\\[",nflflex)
dfnflflex <- fromJSON(nflflex, flatten=TRUE)
dfnflflex <- dfnflflex[cols]
dfnflflex$util <- 1
#dfnflflex <- apply(dfnflflex,2,as.character)
#get QB
nflqbURL <- paste('https://rotogrinders.com/projected-stats/nfl-qb?site=fanduel&date=',Sys.Date(),sep = '')
nflqb <- read_html(nflqbURL)
nflqb <- html_text(html_nodes(nflqb, "body > div > div > section > div > script"))
nflqb <- sub('.[^]]+$', '', nflqb)
nflqb <- sub('.[^]]+$', '\\]', nflqb)
nflqb <- sub("^.*?\\[","\\[",nflqb)
dfnflqb <- fromJSON(nflqb, flatten=TRUE)
dfnflqb <- dfnflqb[cols]
dfnflqb$util <- 0
#dfnflqb <- apply(dfnflqb,2,as.character)
#get kicker
nflkickerURL <- paste('https://rotogrinders.com/projected-stats/nfl-kicker?site=fanduel&date=',Sys.Date(),sep = '')
nflkicker <- read_html(nflkickerURL)
nflkicker <- html_text(html_nodes(nflkicker, "body > div > div > section > div > script"))
nflkicker <- sub('.[^]]+$', '', nflkicker)
nflkicker <- sub('.[^]]+$', '\\]', nflkicker)
nflkicker <- sub("^.*?\\[","\\[",nflkicker)
dfnflkicker <- fromJSON(nflkicker, flatten=TRUE)
dfnflkicker <- dfnflkicker[cols]
dfnflkicker$util <- 0
#dfnflkicker <- apply(dfnflkicker,2,as.character)
#get defense
nfldefenseURL <- paste('https://rotogrinders.com/projected-stats/nfl-defense?site=fanduel&date=',Sys.Date(),sep = '')
nfldefense <- read_html(nfldefenseURL)
nfldefense <- html_text(html_nodes(nfldefense, "body > div > div > section > div > script"))
nfldefense <- sub('.[^]]+$', '', nfldefense)
nfldefense <- sub('.[^]]+$', '\\]', nfldefense)
nfldefense <- sub("^.*?\\[","\\[",nfldefense)
dfnfldefense <- fromJSON(nfldefense, flatten=TRUE)
dfnfldefense <- dfnfldefense[cols]
dfnfldefense$util <- 0
#dfnfldefense <- apply(dfnfldefense,2,as.character)

#combine all players
dfnfl <- do.call("rbind", list(dfnflqb, dfnflflex, dfnflkicker,dfnfldefense))
#names(dfnfl)[5] <- "points"
dfnfl <- suppressWarnings(dfnfl[!is.na(as.numeric(as.character(dfnfl$salary))),]) #remove non numbers for salary
dfnfl$salary <- as.numeric(dfnfl$salary )
dfnfl <- dfnfl[dfnfl$salary > 0,]
dfnfl <- dfnfl[with(dfnfl,order(position,-points,-salary)),]
dfnfl <- factor2char(dfnfl)

getNFLLineup <- function(NFLData=dfnfl,cbExclude=NA,cbLock=NA,method='points'){
  cols <- c('player_name','team', 'position','salary',method,'opp','player_id','util')
  NFLData <- NFLData[cols]
  names(NFLData)[5] <- "points"
  cbLock[is.na(cbLock)] <- 0
  cbExclude[is.na(cbExclude)] <- 0
  NFLData$cbExclude <- cbExclude
  NFLData$cbLock <- cbLock
  players <- as.character(NFLData[NFLData$cbLock== 1,c("player_id")])
  n <- length(players)
  NFLData <- NFLData[NFLData$cbExclude== 0,]
  #names(NFLData)[names(NFLData) == 'schedule.data.date'] <- 'date'
  #NFLData$date <- as.Date(as.character(NFLData$date))
  
  NFLData$QB <- ifelse(NFLData$position == 'QB',1,0)
  NFLData$WR <- ifelse(NFLData$position == 'WR',1,0)
  NFLData$RB <- ifelse(NFLData$position == 'RB',1,0)
  NFLData$TE <- ifelse(NFLData$position == 'TE',1,0)
  NFLData$K <- ifelse(NFLData$position == 'K',1,0)
  NFLData$D <- ifelse(NFLData$position == 'D',1,0)
  NFLData$lb <- 0
  NFLData$ub <- 1
  NFLData$type <- 'B'
  
  objcols <- c('player_id','points','lb','ub','type')
  obj <- subset(NFLData, select = objcols)
  names(obj)[names(obj) == 'player_id'] <- 'variable'
  names(obj)[names(obj) == 'points'] <- 'coefficient'
  
  concols <- c('player_id','salary','QB','RB','WR','TE','D','K','util')
  con <- subset(NFLData, select = concols)
  names(con)[names(con) == 'player_id'] <- 'variable'
  dir <- data.frame(dir=c('<=','==','>=','>=','>=','==','==','=='), rhs=c(60000,1,2,3,1,1,1,7))
  adddir <- dir[rep(2,n),]
  dir <- rbind(dir,adddir)
  
  con <- factor2char(con)
  con[ ,players] <- 0 #this will be the input list 
  obj <- factor2char(obj)
  matCols <- !(names(con) %in% c("variable"))
  matRows <- !(con$variable %in% c("dir", "description", "rhs"))
  
  A <- t(sapply(con[matRows, matCols, drop = F], as.numeric))
  colnames(A) <- con$variable[matRows]
  A[outer(rownames(A), colnames(A), "==")] <- 1
  B <- cbind(
    constraints = setdiff(colnames(con), 'variable'),
    dir
  )
  B <- factor2char(B)
  con[is.na(con)] <- 0
  #con <- as.simple_triplet_matrix(con)
  
  
  objective <- as.vector(obj$coefficient)
  constraints <- L_constraint(
    L = A,
    dir = B$dir,
    rhs = B$rhs
  )
  
  op_obj <- OP(
    objective = objective,
    constraints = constraints,
    types = obj$type,
    bounds = getBounds_matrix(obj),
    maximum = TRUE
  )
  
  sol <- ROI_solve(op_obj,solver = 'glpk')
  solution <- data.frame(sol$solution,player_id = obj$variable)
  solcols <- c('player_name','team', 'position','salary','points')
  solution <- merge(solution, NFLData, by = c("player_id"))
  solution <- solution[solution$sol.solution == 1,]
  solution <- solution[,c(solcols)]
  solution <- solution[with(solution,order(-points,-salary)),]
  solution <- rbind(solution, data.frame(player_name='Total',team = '',position = '', salary = sum(solution$salary),points = sum(solution$points)))
  solution
}

NFLData <- dfnfl
outputcols <- c('player_name','team', 'position','salary','points','ceil','floor','opp')
NFLData <- NFLData[,c(outputcols)]
NFLData <- type.convert(NFLData)
NFLData$player_name <- as.character(NFLData$player_name)
