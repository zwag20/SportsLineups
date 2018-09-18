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

#Get nba Data into Data Frame
cols <- c('player_name','team', 'position','salary','points','ceil','floor','opp','player_id','schedule.data.date')
#Get Position Players
nbaplayerURL <- paste('https://rotogrinders.com/projected-stats?site=fanduel&sport=nba&date',Sys.Date(),sep = '')
nbaplayer <- read_html(nbaplayerURL)
nbaplayer <- html_text(html_nodes(nbaplayer, "body > div > div > section > div > script"))
nbaplayer <- sub('.[^]]+$', '', nbaplayer)
nbaplayer <- sub('.[^]]+$', '\\]', nbaplayer)
nbaplayer <- sub("^.*?\\[","\\[",nbaplayer)
dfnbaplayer <- fromJSON(nbaplayer, flatten=TRUE)
if(length(dfnbaplayer)==0){
  dfnbaplayer <- data.frame(matrix(vector(), 0, 10,dimnames=list(c(), cols)),stringsAsFactors=F)
}else{
dfnbaplayer <- dfnbaplayer[cols]
}

dfnba <- dfnbaplayer
dfnba <- suppressWarnings(dfnba[!is.na(as.numeric(as.character(dfnba$salary))),]) #remove non numbers for salary
dfnba$salary <- as.numeric(dfnba$salary )
dfnba <- dfnba[dfnba$salary > 0,]
dfnba <- dfnba[with(dfnba,order(position,-points,-salary)),]
dfnba <- factor2char(dfnba)

getNBALineup <- function(NBAData=dfnba,cbExclude=NA,cbLock=NA,method='points'){
  if(dim(NBAData)[1] == 0){
    solution <- data.frame(matrix(vector(),0,5,dimnames = list(c(),c('player_name','team', 'position','salary','points'))),stringsAsFactors = F)
  }else{
  cols <- c('player_name','team', 'position','salary',method,'opp','player_id')
  NBAData <- NBAData[cols]
  names(NBAData)[5] <- "points"
  cbLock[is.na(cbLock)] <- 0
  cbExclude[is.na(cbExclude)] <- 0
  NBAData$cbExclude <- cbExclude
  
  NBAData$cbLock <- cbLock
  players <- as.character(NBAData[NBAData$cbLock== 1,c("player_id")])
  n <- length(players)
  NBAData <- NBAData[NBAData$cbExclude== 0,]
  
  NBAData$C <- ifelse(NBAData$position == 'C',1,0)
  NBAData$PF <- ifelse(NBAData$position == 'PF',1,0)
  NBAData$PG <- ifelse(NBAData$position == 'PG',1,0)
  NBAData$SF <- ifelse(NBAData$position == 'SF',1,0)
  NBAData$SG <- ifelse(NBAData$position == 'SG',1,0)
  NBAData$lb <- 0
  NBAData$ub <- 1
  NBAData$type <- 'B'
  
  objcols <- c('player_id','points','lb','ub','type')
  obj <- subset(NBAData, select = objcols)
  names(obj)[names(obj) == 'player_id'] <- 'variable'
  names(obj)[names(obj) == 'points'] <- 'coefficient'
  
  concols <- c('player_id','salary','C','PF','PG','SF','SG')
  con <- subset(NBAData, select = concols)
  names(con)[names(con) == 'player_id'] <- 'variable'
  dir <- data.frame(dir=c('<=','==','==','==','==','=='), rhs=c(60000,1,2,2,2,2))
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
  solution <- merge(solution, NBAData, by = c("player_id"))
  solution <- solution[solution$sol.solution == 1,]
  solution <- solution[,c(solcols)]
  solution <- solution[with(solution,order(-points,-salary)),]
  solution <- rbind(solution, data.frame(player_name='Total',team = '',position = '', salary = sum(solution$salary),points = sum(solution$points)))
  solution
  }
}

NBAData <- dfnba
outputcols <- c('player_name','team', 'position','salary','points','ceil','floor','opp')
NBAData <- NBAData[,c(outputcols)]
NBAData <- type.convert(NBAData) 
NBAData$player_name <- as.character(NBAData$player_name)




