
dealer_hand_11 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
  while(split_exit == FALSE){
  if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8))
  {
    print(hand)
    split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
    player_hands <- split_out$it2
    hand <- player_hands[currenthand,3:14]
    six_deck_shuffled <- split_out$it3
    split_exit <- split_out$it1
  }
    else{
    split_exit=TRUE}
  }
  
  #define doubles (ONLY hard 11)
  if(val == 11 & soft==FALSE)
  {
    double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
    player_hands <- double_out$it2
    six_deck_shuffled <- double_out$it3
    exit_all = TRUE
    
  }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
  #define stand rules
    #start with hard rules
  
  if(sum_cards(hand) %in% c(17,18,19,20,21) & soft_or_hard(hand)==FALSE)
  {
    print("standing")
    exit_all=TRUE
  }
  #now do soft rules
    if(sum_cards(hand) %in% c(19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
  #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 17 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 19 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
  
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}






dealer_hand_10 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val == 11 & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 17 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 19 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}






dealer_hand_9 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==9 & hand[2]==9))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 17 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 19 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}



dealer_hand_8 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==9 & hand[2]==9))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 17 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 18 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}





dealer_hand_7 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==7 & hand[2]==7))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 17 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 18 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}






dealer_hand_6 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==9 & hand[2]==9)
         || (hand[1]==7 & hand[2]==7) || (hand[1]==7 & hand[2]==7) || (hand[1]==6 & hand[2]==6)
         || (hand[1]==4 & hand[2]==4) || (hand[1]==3 & hand[2]==3) || (hand[1]==2 & hand[2]==2))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(9,10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    if(val %in% c(13,14,15,16,17,18,19) & soft==TRUE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(12,13,14,15,16,17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 12 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 18 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}




dealer_hand_5 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==9 & hand[2]==9)
         || (hand[1]==7 & hand[2]==7) || (hand[1]==7 & hand[2]==7) || (hand[1]==6 & hand[2]==6)
         || (hand[1]==4 & hand[2]==4) || (hand[1]==3 & hand[2]==3) || (hand[1]==2 & hand[2]==2))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(9,10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    if(val %in% c(13,14,15,16,17,18) & soft==TRUE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(12,13,14,15,16,17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 12 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 18 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}



dealer_hand_4 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==9 & hand[2]==9)
         || (hand[1]==7 & hand[2]==7) || (hand[1]==7 & hand[2]==7) || (hand[1]==6 & hand[2]==6)
         || (hand[1]==3 & hand[2]==3) || (hand[1]==2 & hand[2]==2))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(9,10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    if(val %in% c(15,16,17,18) & soft==TRUE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(12,13,14,15,16,17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 12 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) < 18  & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}



dealer_hand_3 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==9 & hand[2]==9)
         || (hand[1]==7 & hand[2]==7) || (hand[1]==7 & hand[2]==7) || (hand[1]==6 & hand[2]==6)
         || (hand[1]==3 & hand[2]==3) || (hand[1]==2 & hand[2]==2))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(9,10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    if(val %in% c(17,18) & soft==TRUE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(13,14,15,16,17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 13 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) <= 17  & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}




dealer_hand_2 <- function(hand,currenthand,player_hands,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  split_exit = FALSE
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    while(split_exit == FALSE){
      if((hand[1]==11 & hand[2] == 11) || (hand[1]==8 & hand[2]==8) || (hand[1]==9 & hand[2]==9)
         || (hand[1]==7 & hand[2]==7) || (hand[1]==7 & hand[2]==7) || (hand[1]==6 & hand[2]==6)
         || (hand[1]==3 & hand[2]==3) || (hand[1]==2 & hand[2]==2))
      {
        print(hand)
        split_out <- check_splits(hand,currenthand,player_hands,six_deck_shuffled)
        player_hands <- split_out$it2
        hand <- player_hands[currenthand,3:14]
        six_deck_shuffled <- split_out$it3
        split_exit <- split_out$it1
      }
      else{
        split_exit=TRUE}
    }
    
    #define doubles (ONLY hard 11)
    if(val %in% c(10,11) & soft==FALSE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    if(val %in% c(17,18) & soft==TRUE)
    {
      double_out <- double_hand(hand,currenthand,player_hands,six_deck_shuffled)
      player_hands <- double_out$it2
      six_deck_shuffled <- double_out$it3
      exit_all = TRUE
      
    }
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(13,14,15,16,17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) < 13 & soft_or_hard(hand)==FALSE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) <= 17 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card(hand,currenthand,player_hands,six_deck_shuffled,current_card)
      current_card <- current_card + 1
      player_hands <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- player_hands[currenthand,3:14]
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=player_hands, "it3" = six_deck_shuffled)
  return(newlist)
  
}






play_dealer_hand <- function(hand,currenthand,six_deck_shuffled,current_card) 
{
  val <- sum_cards(hand)
  soft <- soft_or_hard(hand)
  exit_all = FALSE
  
  #define splits (A's and 8's)
  while(exit_all == FALSE){
    
    #define bust
    if(sum_cards(hand) > 21 )
    {
      print("busted")
      exit_all=TRUE
    }
    
    #define stand rules
    #start with hard rules
    
    if(sum_cards(hand) %in% c(17,18,19,20,21) & soft_or_hard(hand)==FALSE)
    {
      print("standing")
      exit_all=TRUE
    }
    #now do soft rules
    if(sum_cards(hand) %in% c(18,19,20,21) & soft_or_hard(hand)==TRUE)
    {
      print("standing")
      exit_all=TRUE
    }
    
    #define draw card rules
    #start with hard rules
    if(sum_cards(hand) <= 16 & soft_or_hard(hand)==FALSE)
    {
      print(sum_cards(hand))
      print(hand)
      print("drawing another card")
      draw_out <- draw_card_dealer(hand,currenthand, six_deck_shuffled,current_card)
      current_card <- current_card + 1
      dealer_hand <- draw_out$it2
      print(paste0("dealer_hand_outside_function ",dealer_hand))
      six_deck_shuffled <- draw_out$it3
      hand <- dealer_hand
      print(paste0("hand: ", hand))
    }
    
    
    #now the soft rules
    if(sum_cards(hand) <= 17 & soft_or_hard(hand)==TRUE)
    {
      print("drawing another card")
      draw_out <- draw_card_dealer(hand,currenthand, six_deck_shuffled,current_card)
      current_card <- current_card + 1
      dealer_hand <- draw_out$it2
      six_deck_shuffled <- draw_out$it3
      hand <- dealer_hand
      print(paste0("hand: ", hand))
    }
    
    
    
    
    
  }
  
  
  newlist <-  list("it2"=dealer_hand, "it3" = six_deck_shuffled)
  return(newlist)
  
}

