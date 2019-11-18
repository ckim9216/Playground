install.packages("holdem")
library(holdem)
library(help="holdem")

## Write an R function that takes as inputs your cards and other variables used in
## gravity and decides whether you go all-in or fold. The function must return a 0
## integer if you fold or your numer of chips if you go all-in.
## all in with any pair higher than 7s, or if lower card is J or higher,
## or if you have less than 3 times the big blind
# (2x2 matrix) your hole cards. 
# cards1[1,1] = the number of card 1 (between 2 and 14). 
# cards[1,2] = suit of card 1 (between 1 and 4). 
# cards1[2,1] = the number (2-14) of card 2. 
# cards[2,2] = suit of card 2 (1-4).
# cards1[2,1] = the number (2-14) of card 2.

pmc <- function (numattable1, crds1, board1, round1, currentbet,
                  mychips1, pot1, roundbets, blinds1, chips1, ind1, dealer1,
                  tablesleft) {
  
  a1 = 0 
  if ((crds1[1, 1] == crds1[2, 1]) && (crds1[1, 1] > 9)) a1 = mychips1
  if((crds1[1,1] > 13.5) && (crds1[2,1] > 9.5)) a1 = mychips1
  if((crds1[1,1]-crds1[2,1]==1) && (crds1[2,1]>9.5)) a1 = mychips1
  if(mychips1 < 2*blinds1) a1 = mychips1
  a1
}


