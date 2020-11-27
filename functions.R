##sum cards
##why so much effort to add cards...because of the freaking ACES!  That's why.
##go ahead, google around, outside of Hadley, most everyone has an error in this part.

soft_or_hard <- function(hand) {
  
  aces <- FALSE
  ace_locs <- numeric()
  soft=FALSE
  sum <- 0
  
  # if the hand is only 2 cards, just sum and exit
  #if (length(hand) == 2) {
  # sum <- hand[1] + hand[2]
  #  print("I'm in here")
  #    return(soft)
  #  }
  
  # find aces
  for (k in 1:length(hand)) {
    if(hand[k] == 11) {
      aces <- TRUE
      ace_locs <- c(ace_locs, k)
    }
  }
  
  # if no aces were found, sum all items and return
  if (aces == FALSE) {
    for (i in hand) {
      sum <- sum + i
    }
    return(soft)
  }
  
  # otherwise aces were found so first sum items that are NOT aces
  not_aces <- subset(hand, !(hand %in% c(11)))
  #print("not aces")
  #print(not_aces)
  for (i in not_aces) {
    # print(sum)
    #print(i)
    sum <- sum + i
    #print("i should be in here")
    #print(sum)
  }
  
  # if sum of non-aces > 10 all aces must be treated as 1's
  if (sum > 10) {
    for (i in ace_locs) {
      sum <- sum + 1
    }
    soft = FALSE
    return(soft)
  }
  
  # if sum of non-aces <= 9, need to check count of aces. 
  # If > 1 one can be treated as 11 while other must be treated as 1
  if (sum <= 9 & length(ace_locs) > 1) {
    sum <- sum + 11
    soft=TRUE
    for (i in 1:(length(ace_locs) - 1)) {
      sum <- sum + 1
      soft = TRUE
    }
    return(soft)
  }
  
  if (sum <= 9 & length(ace_locs) == 1) {
    #  print("I definitely should be here")
    sum <- sum + 11
    soft = TRUE
    return(soft)
  }
  
  # if sum of non-aces == 10, need to check count of aces.
  # if == 1, then add 11 to sum and exit. if > 1, treat all aces as 1's
  if (sum == 10 & length(ace_locs) == 1) {
    sum <- sum + 11
    soft=TRUE
    return(soft)
  } else {
    for (i in 1:length(ace_locs)) {
      sum <- sum + 1
      soft=FALSE
    }
    return(soft)
  }
  
  
  
  
}
sum_cards <- function(hand) {
  
  aces <- FALSE
  ace_locs <- numeric()
  sum <- 0
  
  # if the hand is only 2 cards, just sum and exit
  if (length(hand) == 2 & hand[1] != 11 & hand[2] != 11) {
    sum <- hand[1] + hand[2]
    return(sum)
  }
  
  # find aces
  for (k in 1:length(hand)) {
    if(hand[k] == 11) {
      aces <- TRUE
      ace_locs <- c(ace_locs, k)
    }
  }
  
  # if no aces were found, sum all items and return
  if (aces == FALSE) {
    for (i in hand) {
      sum <- sum + i
    }
    return(sum)
  }
  
  # otherwise aces were found so first sum items that are NOT aces
  not_aces <- subset(hand, !(hand %in% c(11)))
  for (i in not_aces) {
    sum <- sum + i
  }
  
  # if sum of non-aces > 10 all aces must be treated as 1's
  if (sum > 10) {
    for (i in ace_locs) {
      sum <- sum + 1
    }
    return(sum)
  }
  
  # if sum of non-aces <= 9, need to check count of aces. 
  # If > 1 one can be treated as 11 while other must be treated as 1
  if (sum <= 9 & length(ace_locs) > 1) {
    sum <- sum + 11
    for (i in 1:(length(ace_locs) - 1)) {
      sum <- sum + 1
    }
    return(sum)
  }
  
  if (sum <= 9 & length(ace_locs) == 1) {
    sum <- sum + 11
    return(sum)
  }
  
  # if sum of non-aces == 10, need to check count of aces.
  # if == 1, then add 11 to sum and exit. if > 1, treat all aces as 1's
  if (sum == 10 & length(ace_locs) == 1) {
    sum <- sum + 11
    return(sum)
  } else {
    for (i in 1:length(ace_locs)) {
      sum <- sum + 1
    }
    return(sum)
  }
  
}



check_dealer_blackjack <- function(hand) {
  up_card <-  hand[1]
  hole_card <- hand[2]
  dealer_blackjack = FALSE
  
  if(up_card == 11)
  {
    #up card is an ace, check for black jack
    if(hole_card == 10)
    {
      print("Dealer has blackjack, you lose, or maybe tie?")
      dealer_blackjack= TRUE
    }
  }
  if(up_card == 10)
  {
    #up card is an ace, check for black jack
    if(hole_card == 11)
    {
      print("Dealer has blackjack, you lose, or maybe tie?")
      dealer_blackjack = TRUE
    }
  }
 
    return(dealer_blackjack)
  
  
}

check_player_blackjack <- function(hand) {
  up_card <-  hand[1]
  hole_card <- hand[2]
  player_blackjack = FALSE
  
  if(up_card == 11)
  {
    #up card is an ace, check for black jack
    if(hole_card == 10)
    {
      print("Player has blackjack, you win!")
      player_blackjack= TRUE
    }
  }
  if(up_card == 10)
  {
    #up card is an ace, check for black jack
    if(hole_card == 11)
    {
      print("Player has blackjack, you win!")
      player_blackjack = TRUE
    }
  }
  
  return(player_blackjack)
  
  
}

check_player_blackjack(c(10,11))


check_splits <- function(hand,currenthand,player_hands,six_deck_shuffled) {
  done_splitting = FALSE
 
  

  if(hand[1]==hand[2])
  {
    print("you have a pair, you may want to split")
    print(player_hands[2,1])
    #splits depend on dealer upcard.  
    if(player_hands[2,1]==0)
    {
      #split the cards here
      print("i'm in the splitter")
      player_hands[2,1]=1
      player_hands[2,3] <-player_hands[currenthand,4]
      player_hands[2,2] <-player_hands[currenthand,2]
      player_hands[currenthand,4] <-six_deck_shuffled[1]
      player_hands[2,4] <-six_deck_shuffled[2]
      six_deck_shuffled <- six_deck_shuffled[c(-1,-2)]
      done_splitting=FALSE
      print(player_hands)
     
      
    }
    else if(player_hands[3,1]==0)
    {
      print("i'm in the splitter")
      player_hands[3,1]=1
      player_hands[3,3] <-player_hands[currenthand,4]
      player_hands[3,2] <-player_hands[currenthand,2]
      player_hands[currenthand,4] <-six_deck_shuffled[1]
      player_hands[3,4] <-six_deck_shuffled[2]
      print(player_hands)
      six_deck_shuffled <- six_deck_shuffled[c(-1,-2)]
      done_splitting=FALSE
    }
    else if(player_hands[4,1]==0)
    {
      print("i'm in the splitter")
      player_hands[4,1]=1
      player_hands[4,3] <-player_hands[currenthand,4]
      player_hands[4,2] <-player_hands[currenthand,2]
      player_hands[currenthand,4] <-six_deck_shuffled[1]
      player_hands[4,4] <-six_deck_shuffled[2]
      six_deck_shuffled <- six_deck_shuffled[c(-1,-2)]
      done_splitting=FALSE
    }
    else if(player_hands[4,1]==1)
    {
      print("no more splits")
      done_splitting=TRUE
    }
    
    

    

  }
  if(hand[1]!=hand[2])
  {
    done_splitting=TRUE
  }
  newlist <-  list("it1"=done_splitting,"it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}



double_hand <- function(hand,currenthand,player_hands,six_deck_shuffled) {
  print("doubling down")
  player_hands[currenthand,5] <- six_deck_shuffled[1]
  player_hands[currenthand,2] <- player_hands[currenthand,2]*2
  six_deck_shuffled <- six_deck_shuffled[-1]
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}

draw_card <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) {
  print("in draw card function")
  player_hands[currenthand,current_card] <- six_deck_shuffled[1]
  six_deck_shuffled <- six_deck_shuffled[-1]
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
}



draw_card_dealer <- function(hand,currenthand,six_deck_shuffled,current_card) {
  print("in dealer draw card function")
  dealer_hand <- hand
  dealer_hand[current_card] <- six_deck_shuffled[1]
  print(paste0("dealer_hand_in_function ",dealer_hand))
  six_deck_shuffled <- six_deck_shuffled[-1]
  newlist <-  list("it2"=dealer_hand, "it3" = six_deck_shuffled)
  return(newlist)
}




