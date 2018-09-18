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

#Get MLB Data into Data Frame
cols <- c('player_name','team', 'position','salary','points','ceil','floor','opp','player_id','schedule.data.date')
#Get Position Players
mlbhitterURL <- paste('https://rotogrinders.com/projected-stats/mlb-hitter?site=fanduel&date=',Sys.Date(),sep = '')
mlbhitter <- read_html(mlbhitterURL)
mlbhitter <- html_text(html_nodes(mlbhitter, "body > div > div > section > div > script"))
mlbhitter <- sub('.[^]]+$', '', mlbhitter)
mlbhitter <- sub('.[^]]+$', '\\]', mlbhitter)
mlbhitter <- sub("^.*?\\[","\\[",mlbhitter)
dfmlbhitter <- fromJSON(mlbhitter, flatten=TRUE)
dfmlbhitter <- dfmlbhitter[cols]
dfmlbhitter$util <- 1
#get Pitcher
mlbpitcherURL <- paste('https://rotogrinders.com/projected-stats/mlb-pitcher?site=fanduel&date=',Sys.Date(),sep = '')
mlbpitcher <- read_html(mlbpitcherURL)
mlbpitcher <- html_text(html_nodes(mlbpitcher, "body > div > div > section > div > script"))
mlbpitcher <- sub('.[^]]+$', '', mlbpitcher)
mlbpitcher <- sub('.[^]]+$', '\\]', mlbpitcher)
mlbpitcher <- sub("^.*?\\[","\\[",mlbpitcher)
dfmlbpitcher <- fromJSON(mlbpitcher, flatten=TRUE)
dfmlbpitcher <- dfmlbpitcher[cols]
dfmlbpitcher$util <- 0

#combine all players
dfmlb <- do.call("rbind", list(dfmlbpitcher, dfmlbhitter))
#names(dfmlb)[5] <- "points"
dfmlb <- suppressWarnings(dfmlb[!is.na(as.numeric(as.character(dfmlb$salary))),]) #remove non numbers for salary
dfmlb$salary <- as.numeric(dfmlb$salary )
dfmlb <- dfmlb[dfmlb$salary > 0,]
dfmlb <- dfmlb[with(dfmlb,order(position,-points,-salary)),]
dfmlb <- factor2char(dfmlb)

getMLBLineup <- function(MLBData=dfmlb,cbExclude=NA,cbLock=NA,method='points'){
  cols <- c('player_name','team', 'position','salary',method,'opp','player_id','util')
  MLBData <- MLBData[cols]
  names(MLBData)[5] <- "points"
  cbLock[is.na(cbLock)] <- 0
  cbExclude[is.na(cbExclude)] <- 0
  MLBData$cbExclude <- cbExclude
  MLBData$cbLock <- cbLock
  players <- as.character(MLBData[MLBData$cbLock== 1,c("player_id")])
  n <- length(players)
  MLBData <- MLBData[MLBData$cbExclude== 0,]
  
  MLBData$P <- ifelse(MLBData$position == 'P',1,0)
  MLBData$'C-1B' <- ifelse(MLBData$position == 'C-1B',1,0)
  MLBData$'2B' <- ifelse(MLBData$position == '2B',1,0)
  MLBData$SS <- ifelse(MLBData$position == 'SS',1,0)
  MLBData$'3B' <- ifelse(MLBData$position == '3B',1,0)
  MLBData$OF <- ifelse(MLBData$position == 'OF',1,0)
  MLBData$Util <- 1
  MLBData$lb <- 0
  MLBData$ub <- 1
  MLBData$type <- 'B'
  
  objcols <- c('player_id','points','lb','ub','type')
  obj <- subset(MLBData, select = objcols)
  names(obj)[names(obj) == 'player_id'] <- 'variable'
  names(obj)[names(obj) == 'points'] <- 'coefficient'

  concols <- c('player_id','salary','P','C-1B','2B','SS','3B','OF','Util')
  con <- subset(MLBData, select = concols)
  names(con)[names(con) == 'player_id'] <- 'variable'
  dir <- data.frame(dir=c('<=','==','>=','>=','>=','>=','>=','=='), rhs=c(35000,1,1,1,1,1,3,8))
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
  solution <- merge(solution, MLBData, by = c("player_id"))
  solution <- solution[solution$sol.solution == 1,]
  solution <- solution[,c(solcols)]
  solution <- solution[with(solution,order(-points,-salary)),]
  solution <- rbind(solution, data.frame(player_name='Total',team = '',position = '', salary = sum(solution$salary),points = sum(solution$points)))
  solution
}
  
MLBData <- dfmlb
outputcols <- c('player_name','team', 'position','salary','points','ceil','floor','opp')
MLBData <- MLBData[,c(outputcols)]
MLBData <- type.convert(MLBData) 
MLBData$player_name <- as.character(MLBData$player_name)
  
