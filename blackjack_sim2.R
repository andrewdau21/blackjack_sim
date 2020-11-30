##blackjack simulation

#runs, specify the length


#Build the Deck
#We will create six decks
#Suits don't matter, only the VALUE of the card matters
library(dplyr)
source("functions.R")
#source("functions_play_hands_hit_hard_16.R")
source("functions_play_hands.R")
bets_all_all <- list()
winnings_all_all <- list()
runs <- 1000

for(z in 1:runs)
{

'%!in%' <- function(x,y)!('%in%'(x,y))
bets_all <-vector()
winnings_all <- vector()
numsim <- 10
for (j in 1:numsim)
{
deck <- rep(c(2:10, 10, 10, 10, 11), 4)

six_deck <- rep(deck,6)

#i <- 150
set.seed(j*z+500)
#set.seed(j*3+500)
six_deck_shuffled <-sample(six_deck,length(six_deck), replace = FALSE)
six_deck_shuffled
six_deck_shuffled2 <- six_deck_shuffled

#in normal black jack, dealers try to cut somewhere between 75-100% of one deck out.
#so we will randomly simulate a number between 75 and 100.

number_of_cards_to_cut <- round(sample(75:100, 1, replace=TRUE)/100 * 52)
new_deck_length <- length(six_deck_shuffled)-number_of_cards_to_cut

#create our working deck
#this is our initial condition

##oops, don't actually cut it...last hand streches beyond.
#six_deck_shuffled_cut <- six_deck_shuffled[1:new_deck_length]

#the "book" defines the best rules for the player, lets simulate that first.
#first, lets deal one hand and get this to work.

#deal a hand

#number of players
bet <- 5
inshoe=TRUE
round = 1
player_hands_all <- list()
dealer_hands_all <- list()
while(inshoe == TRUE)
{

 # if (round > 3)
 #  {
 #    inshoe=FALSE
 #  }
  if (length(six_deck_shuffled) < number_of_cards_to_cut)
  {
    print("in here")
    inshoe=FALSE
  }

#deal player_hand
#we deal 4 hands, because most casinos limit the player to splits that create 4 hands
#we use the first column as an indicator to activate a hand
#we use the second column as an indicator for the value of the bet.
#splits are a pain in this process
hand1 <- matrix(c(1,bet,six_deck_shuffled[c(1,3)],0,0,0,0,0,0,0,0,0,0),nrow=1, ncol=14)
hand2 <- matrix(rep(0,14),nrow=1,ncol=14)
hand3 <- matrix(rep(0,14),nrow=1,ncol=14)
hand4 <- matrix(rep(0,14),nrow=1,ncol=14)
player_hands <- rbind(hand1,hand2,hand3,hand4)
colnames(player_hands) <- c("Playing","Bet","Card1","Card2","Card3","Card4","Card5","Card6","Card7","Card8",
                            "Card9","Card10","Card11","Card12")
rownames(player_hands) <- c("Hand1","Hand2","Hand3","Hand4")
                                             

#deal dealer hand 
dealer_hand <- six_deck_shuffled[c(2,4)]


#update the shuffled deck
six_deck_shuffled <- six_deck_shuffled[c(-1,-2,-3,-4)]

#play a hand
hand_counter <- 1

### FOR TESTING ONLY
## SET player hand manually
#layer_hands[1,3] = 7
#player_hands[1,4] = 7
#dealer_hand <- c(6,9)

up_card <-  dealer_hand[1]
hole_card <- dealer_hand[2]


for (i in 1:4)
{
  print(i)
  stand = FALSE
  while(stand == FALSE)
  {
    if(player_hands[i,1] == 1)
    {
      #lets play!!!
      print(paste0("playing hand #",i))
      if(check_dealer_blackjack(dealer_hand) == TRUE)
      {
        print("I'm in here")
        stand = TRUE
        break
        print(stand)
      }
      if(up_card == 11)
      {
        current_card_index = 5
        aaa <- dealer_hand_11(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 10)
      {
        print ("im where I shouldnt' be")
        current_card_index = 5
        aaa <- dealer_hand_10(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 9)
      {
        current_card_index = 5
        aaa <- dealer_hand_9(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 8)
      {
        current_card_index = 5
        aaa <- dealer_hand_8(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 7)
      {
        current_card_index = 5
        aaa <- dealer_hand_7(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 6)
      {
        current_card_index = 5
        aaa <- dealer_hand_6(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 5)
      {
        current_card_index = 5
        aaa <- dealer_hand_5(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 4)
      {
        current_card_index = 5
        aaa <- dealer_hand_4(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 3)
      {
        current_card_index = 5
        aaa <- dealer_hand_3(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      if(up_card == 2)
      {
        current_card_index = 5
        aaa <- dealer_hand_2(player_hands[i,3:14],i,player_hands,six_deck_shuffled,current_card_index)
        six_deck_shuffled <- aaa$it3
        player_hands <- aaa$it2
        stand=TRUE
      }
      
    }
    else(stand = TRUE)
  }
  dealer_card_index = 3
  temper <- play_dealer_hand(dealer_hand,1,six_deck_shuffled,dealer_card_index) 
  dealer_hand <- temper$it2
  
}



dealer_hands_all[[paste0("round", round)]] <- dealer_hand
player_hands_all[[paste0("round", round)]] <- player_hands


  
round = round+1
print(round)
print(inshoe)

}

print(round)
#check the outcome
bets<-vector()
winnings <-vector()
looper <- round -1
for(i in 1:looper)
{
  print(i)
dealer_sum <- sum_cards(unlist(dealer_hands_all[i]))
#player_sum <- sum_cards(player_hands_all[[i]][1,3:14])
#print("dealer")
#print(dealer_sum)
#print(unlist(dealer_hands_all[i]))
#print("player")
#print(player_sum)
#print(player_hands_all[[i]][1,3:14])
bets[i] <- 0
winnings[i] <-0
k = 1
for(k in 1:4)
{
  if(player_hands_all[[i]][k,1] == 1){
  player_sum <- sum_cards(player_hands_all[[i]][k,3:14])
#bets[i] <- player_hands_all[[i]][1,2]
bets[i] <- player_hands_all[[i]][k,2] + bets[i]
if (player_sum == 21 & player_hands_all[[i]][k,3] %in% c(10,11) & player_hands_all[[i]][k,4] %in% c(10,11) &
  dealer_sum == 21 & unlist(dealer_hands_all[i]))
{
  winnings[i] <- player_hands_all[[i]][k,2] *2.5 + winnings[i]
}
else if (dealer_sum <= 21 & player_sum <=21 & player_sum > dealer_sum)
{
  winnings[i] <- player_hands_all[[i]][k,2] *2 + winnings[i]
}
else if (dealer_sum <= 21 & player_sum <=21 & player_sum < dealer_sum)
{
  winnings[i] <- player_hands_all[[i]][k,2]*0 + winnings[i]
}
else if (dealer_sum <= 21 & player_sum <=21 & player_sum == dealer_sum)
{
  winnings[i] <- player_hands_all[[i]][k,2]*1 + winnings[i]
}
else if (dealer_sum > 21 & player_sum <=21 )
{
  winnings[i] <- player_hands_all[[i]][k,2]*2 + winnings[i]
}
else if (player_sum > 21 )
{
  winnings[i] <- player_hands_all[[i]][k,2]*0 + winnings[i]
}
  }
  k = k+1
}

#print(sum_cards(player_hands_all[[1]][2,3:14]))
#print(sum_cards(player_hands_all[[1]][3,3:14]))
#print(sum_cards(player_hands_all[[1]][4,3:14]))
}

bets_all[j] <- sum(bets)
winnings_all[j] <- sum(winnings)

}

bets_all_all[[paste0("sim", z)]] <-bets_all
winnings_all_all[[paste0("sim", z)]] <-winnings_all

z <- z+1

}


##limit
#limit = runs
limit = 100

bets_all_all
winnings_all
#for (u in 1:runs)
for (u in 1:limit)
{
  temp_bets <- bets_all_all[[u]]
  temp_winnings <- winnings_all_all[[u]]
  final <- as.data.frame(cbind(temp_bets,temp_winnings))
  final <- final %>%
    mutate(dif=temp_winnings-temp_bets)
  if(u == 1)
  {
    #plot((final$dif))
    plot(1,type='n',xlim=c(1,numsim),ylim=c(-600,600),xlab='Shoe Number', ylab='Cumulative Winnings')
    lines(cumsum(final$dif))
  }
  else
  {
    lines(cumsum(final$dif))
  }
  
  u <- u+1
  
}

abline(h=0,col="red")
# 
# sum(bets_all)
# sum(winnings_all)
# 
# 

# plot(cumsum(check$dif))
#plot(cumsum(final$dif,type=n))
#lines(cumsum(final$dif))
#hist(check$dif)
# 
# for(i in 1:50)
# {
#   
#   print(player_hands_all[[i]][,3:14])
#   print(dealer_hands_all[[i]])
#   print(bets[i])
#   print(winnings[i])
# }

#sum them all up
total_bets <-0
total_winnings <- 0
max_winnings <- 0
biggest_loss <- 0
winning <- 0
losing <- 0
break_even <- 0

#for(h in 1:runs)
for(h in 1:limit)
{
  total_bets <- sum(bets_all_all[[h]]) + total_bets
  total_winnings <- sum(winnings_all_all[[h]]) + total_winnings
  max_winnings <- max(sum(winnings_all_all[[h]])-sum(bets_all_all[[h]]), max_winnings)
  biggest_loss <- min(sum(winnings_all_all[[h]])-sum(bets_all_all[[h]]), biggest_loss)
  
  if(sum(winnings_all_all[[h]])-sum(bets_all_all[[h]]) > 0)
     {
       winning <- winning + 1
  }
  if(sum(winnings_all_all[[h]])-sum(bets_all_all[[h]]) < 0)
  {
    losing <- losing + 1
  }
  if(sum(winnings_all_all[[h]])-sum(bets_all_all[[h]]) == 0)
  {
    break_even <- break_even + 1
  }
  
 
}

total_bets
total_winnings
total_winnings-total_bets
max_winnings
biggest_loss
winning
losing
break_even
house_edge <- total_winnings/sum(total_winnings,total_bets)
house_edge
.50-house_edge

average_loss = (total_winnings-total_bets)/limit

average_loss
#setwd("C:/Users/Andrew/Documents/Georgia Tech/ISYE6644/simulation")
#save(winnings_all_all, file = "winnings_all_all.RData")
#save(bets_all_all, file = "bets_all_all.RData")
