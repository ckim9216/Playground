```{r, error = TRUE}
# Seulchan Kim, tic-tac-toe

triples <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))



update<-function(state, who, pos){
  if(!(pos %in% c(1:9)))
  {stop("You entered a wrong position!")}

  if(!is.na(state[pos]))
  {stop("You cannot choose a position that is already taken!") }
  state[pos]<-who
  return(state)
}


display<-function(state){

  display_state<-state

  for(i in 1:9){ 
    if(is.na(display_state[i]))
  display_state[i]<-i}
  cat("",display_state[1],"|", display_state[2], "|", display_state[3], "\n")
  cat("---+---+---\n")
  cat("",display_state[4],"|", display_state[5], "|", display_state[6], "\n")
  cat("---+---+---\n")
  cat("",display_state[7],"|", display_state[8], "|", display_state[9], "\n")
} 

computer_turn<-function(state) 

{triples <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))
  o_variable<-0
  x_variable<-0
  token<-""
  for(k in state)
  { if(!is.na(k) & k=="O")
    {
    o_variable<-o_variable+1}
    
    else if(!is.na(k) & k=="X"){

      x_variable<-x_variable+1
    }
  }
  # Computer choosing either "O" or "X"
if(x_variable>o_variable)

  { token = "O"

  }else if(x_variable == o_variable){
  token = "X"

  }
  
  # Check if there is a winning poisition

  for(i in triples)

  { if (!is.na(state[i][1]) && !is.na(state[i][2]) &&state[i][1] == token && state[i][1]==state[i][2] && is.na(state[i][3]))

    { return(i[3])

    }else if(!is.na(state[i][1]) && !is.na(state[i][3]) &&state[i][1] == token &&state[i][1]==state[i][3]&& is.na(state[i][2])){

      return(i[2])

    }else if(!is.na(state[i][2]) && !is.na(state[i][3]) &&state[i][2] == token &&state[i][2]==state[i][3]&& is.na(state[i][1])){

      return(i[1])

    }else if (!is.na(state[i][1]) && !is.na(state[i][2]) && state[i][1]==state[i][2] && is.na(state[i][3]))

    {

      return(i[3])

    }else if(!is.na(state[i][1]) && !is.na(state[i][3]) &&state[i][1]==state[i][3]&& is.na(state[i][2])){

      return(i[2])

    }else if(!is.na(state[i][2]) && !is.na(state[i][3]) &&state[i][2]==state[i][3]&& is.na(state[i][1])){

      return(i[1])
    }
  }

  

  # If not, computer choose the position by order

  for(j in 1:9)

  {   
    if(is.na(state[j]))
    {
      return(j) 
    }
  }
}

check_winner<-function(state){

  triples <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))

  #If triples-indexed position has same token, then there is a winner.

  for(i in triples){

    if(!is.na(state[i][1]) && !is.na(state[i][2]) && !is.na(state[i][3]) && state[i][1] == state[i][2] && state[i][1]==state[i][3]){

      return(TRUE)

    }
  }

  return(FALSE)

}



is.finish<-function(state){

  # Determine whether the game is done though there is still no winner.

  for(i in state)

    if(is.na(i)){

      return(FALSE)

    }

  return(TRUE)

}



play <- function(){

  # Set the game: either 1 or 2 players. If computer plays, it goes player 1 or 2.

  number_player<-readline(prompt = "How many players? Either 1 or 2:")

  number_player<-as.numeric(number_player)

  state<-c(NA,NA,NA,NA,NA,NA,NA,NA,NA)

  if(number_player == 1)

  {

    # Set the order of player

    order<-readline(prompt = "Will you be the 1st player? or 2nd? Please choose either 1 or 2:")

    order<-as.numeric(order)

    if(order ==1){
      # Human plays first
      while(!check_winner(state) & !is.finish(state)) #Repeat the loop until the game finishes.

      {
        display(state)
        player1_pos<-readline(prompt="Choose position which is not taken.")
        player1_pos<-as.numeric(player1_pos)
        state<-update(state,"X",player1_pos)
        if(check_winner(state))

        {
          display(state)
          cat("Congrats! You win!\n ")
          break
        }else if(is.finish(state)){
          display(state)
          cat("Draw\n")
          break

        }
        display(state)
        cat("It's computer's turn.\n")
        computer_pos<-computer_turn(state)
        state<-update(state,"O",computer_pos)
        if(check_winner(state))
        { 
          display(state)
          cat("You lose!\n ")
        }

      }
      
    }else if(order ==2){
      #Computer plays first
      while(!check_winner(state) & !is.finish(state))  #Repeat the loop until the game finishes.
      {

        display(state)
        cat("It's computer's turn.\n")
        computer_pos<-computer_turn(state)
        state<-update(state,"X",computer_pos)
        if(check_winner(state))

        {
          display(state)
          cat("You lose!\n ")
          break
        }else if(is.finish(state)){
          display(state)
          cat("Draw\n")
          break

        }
        display(state) 
        player2_pos<-readline(prompt="Choose position which is not taken.")
        player2_pos<-as.numeric(player2_pos)
        state<-update(state,"O",player2_pos)
        if(check_winner(state))

        { 
          display(state)
          cat("Congrats! You win! \n")
        }

      }
    }else{
      stop("You entered a wrong number!\n")
    }

    

  }else if(number_player ==2){

    #When two players play

    while(!check_winner(state) & !is.finish(state))

    {
      display(state)
      player1_pos<-readline(prompt="Player1, Choose position which is not taken.")
      player1_pos<-as.numeric(player1_pos)
      state<-update(state,"X",player1_pos)
      if(check_winner(state))
      {
        display(state)
        cat("Congrats! Player 1 wins!\n ")
        break
      }else if(is.finish(state)){

        display(state)

        cat("Draw.\n")
        break
      }

      display(state)

      player2_pos<-readline(prompt="Player2, Choose position which is not taken.")

      player2_pos<-as.numeric(player2_pos)

      state<-update(state,"O",player2_pos)

      if(check_winner(state))
      { 
        display(state)
        cat("Congrats! Player 2 win!\n ")
      }
    }
  }else{
    stop("You entered a wrong number!")
  }

}
```