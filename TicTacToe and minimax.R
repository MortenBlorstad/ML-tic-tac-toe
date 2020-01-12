
tic.tac.toe = function(first = T){
  if (!interactive()) 
    return()
  if(first==T){
    players = c(Player="X", AI ="O")
    turn= T
  }else{
    players = c(AI="X", Player ="O")
    turn = F
  }

  row <- 3
  col <- 3
  end = F
  board = matrix("", ncol = col, nrow = row)
  drawBord = function(board){
    col = ncol(board)
    row = nrow(board)
    plot(NULL,xlim=c(0,row),ylim=c(0, col),xlab  = "", ylab = "", axes= F,asp=)
    clip(0,row,0, col)
    for (i in col:0) {
      abline(h=c(col-i))
    }
    for (i in row:0) {
      abline(v=c(row-i))
    }
  }
  mat2arrInd = function(row,col,mat){
    row + nrow(mat) * (col - 1)
  }
  updateBoard = function(board){
    for (i in 1:col) {
      for (j in 1:row) {
        if(board[j,i] =="X"){
          color = "red"
        }else{
          color = "blue" 
        }
        text(j-0.5,i-0.5, board[j,i],col = color,cex = 5)
      }
    }
  }
  CheckWinner <- function(board,players){
    winner = NULL
    end = F
    for (j in 1:nrow(board)) {
      for(i in 1:length(players)){
        if(all(board[j,]==players[i])){
          
          winner = players[i]
          end = T
        }
      }
    }
    for (j in 1:ncol(board)) {
      for(i in 1:length(players)){
        if(all(board[,j]==players[i])){
          #print(paste(players[i], "won"))
          winner = players[i]
          end = T
        }
      }
    }
    for(i in 1:length(players)){
      if(all(diag(board)==players[i])){
        #print(paste(players[i], "won"))
        winner = players[i]
        end = T
      }
      if(all(diag(apply(board,2,rev))==players[i])){
        #print(paste(players[i], "won"))
        winner = players[i]
        end = T
      }
    }
    if(all(which(board == "")==F) & is.null(winner)){
      end = T
      winner = "Tie"
    }
    
    return(list(end = end, winner = winner))
  }
  
  AI.move = function(board){
    mm = as.logical(players["AI"]=="X")
    bestscore = ifelse(mm==T,-Inf,Inf)
    a = -Inf
    b = Inf
    bestmove = NULL
    availableMoves = which(board =="")
    for (i in availableMoves) {
      board[i] = players["AI"]
      score = minimax(board,0,a, b, isMaximizing = !mm)
      board[i] = ""
      if(mm ==T){
        if(score > bestscore){
          bestscore = score
          bestmove = i
        }
      }else{
        if(score < bestscore){
          bestscore = score
          bestmove = i
        }
      }
    }
    board[bestmove] = players["AI"]
    return(board)
  }
  minimax = function(board,depth, a, b, isMaximizing = T){
    check = CheckWinner(board,players)
    result = check$winner
    if(!is.null(result) | (all(which(board == "")==F) & is.null(result))){
      if(result =="X"){
        return(10)
        #return(result)
      }
      if(result =="O"){
        return(-10)
        #return(result)
      }
      if(result =="Tie"){
        return(0)
        #return(result)
      }
      
   
    }
    
    if(isMaximizing ==T){
      bestscore = -Inf
      availableMoves = which(board =="")
      for (move in availableMoves) {
        board[move] = "X"
        score = minimax(board,depth+1,a,b,FALSE)
        board[move] = ""
        bestscore = max(c(bestscore,score))
        a = max(c(a,score))
        if(b<=a){
          break()
        }
      }
      return(bestscore)
    }else{
      bestscore = Inf
      availableMoves = which(board =="")
      for (move in availableMoves) {
        board[move] = "O"
        score = minimax(board,depth+1,a,b,TRUE)
        board[move] = ""
        bestscore = min(c(bestscore,score))
        b = min(c(b,score))
        if(b<=a){
          break()
        }
      }
      return(bestscore)
    }
  }
  
  
  X11(type = "Xlib")
  drawBord(board)
  mousedown <- function(buttons, x, y) {
    currentboard = board
    currentturn = turn
    if(turn ==T){
      if(buttons == 0){
        clicked.x <- grconvertX(x, "ndc", "user")
        clicked.y <- grconvertY(y, "ndc", "user")
        if((clicked.y <=col & clicked.y>=0) & (clicked.x <=row & clicked.x>=0)){
          colI <-  floor(clicked.y)
          getcol<- colI+0.5
          rowI <-  floor(clicked.x)
          getrow<- rowI +0.5
          move <- mat2arrInd(rowI+1,colI+1,currentboard)
          availableMoves = which(board =="")
          if(move %in%availableMoves ==T){
            currentboard[move] = players["Player"]
            turn = F
            return(list(board=currentboard,turn=turn))
          }
        }
      }
    }
  }

  # Game loop
  while (end == F) {
    if (end == T){
      break
    }else{
      if (turn == T & end ==F){
        status <- getGraphicsEvent(prompt = "", onMouseDown = mousedown)
        board = status$board
        updateBoard(board)
        turn = status$turn
        check = CheckWinner(board,players)
        end = check$end
      }
      if(turn ==F & end ==F){
        board = AI.move(board)
        updateBoard(board)
        turn = T
        check = CheckWinner(board,players)
        end = check$end
      }
      
      if(!is.null(check$winner)){
        if(check$winner=="Tie"){
          WinnerText = "it's a tie!"
        }else{
          WinnerText = paste(check$winner,"won")
        }
        title(main = WinnerText)
      }
    }
  }
}

tic.tac.toe()




