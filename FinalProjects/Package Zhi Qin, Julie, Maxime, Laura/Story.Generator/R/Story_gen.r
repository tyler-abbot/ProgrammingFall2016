#' Story generator based on inputs
#'
#' This function generates stories.
#' @export
#' @examples
#' story_generator(character, place, ending)
#' story_gen()
#' Kadavresky()


### Programming Project ###

# 

## Requested Inputs

# In order to create the best story with your own amazing touch, we would like to have your input in this.
# We would need you to change/update the words in the inverted commas:

# For character you can choose either "boy", "girl" or "object". 
character <- "object"
# For place you can choose either "outside" or "inside".
place <- "inside"
# For ending you can choose either "happy", "sad" or "random". 
ending <- "happy"
  

# For the second part, please choose:
# Adjective, Villain Name, Colour, Shape, Weapon Name, Funny Word, Random Sound, Exclamation.

# For example:
# A <- "Funny"
# B <- "Bob"
# C <- "Pink"
# D <- "Diamond"
# E <- "Daedric Sword"
# F <- "HAHAHAHA"
# G <- "Jibabooom"
# H <- "OMG"


# Please change the words and fill in according to each category, within the inverted commas
A <- "Adjective"
B <- "Villain Name"
C <- "Colour"
D <- "Shape"
E <- "Weapon Name"
F <- "Funny Word"
G <- "Random Sound"
H <- "Exclamation"


# Data

# Part 1 Story components

first <- c("This is the story of an ingenuous girl who regularly wears a red riding hood.", 
"This is the story of a frivolous mermaid who yearns to have legs.", 
"This is the story of a cheeky boy who gets magic beans in exchange for a cow (his mother is NOT happy :( )",
"This is the story of an evil scientist who creates peculiar potions.", 
"This is the story of a nerdy bookworm who studies biophysics and biochemistry.",
"This is the story of a clumsy butler who was turned into a candlestick by a dark spell.")

second <- c("One day, our hero travels through the wood to visit their grandma and bring her wine and cakes.",
"One day, our hero goes to the edge of the ocean to meet a sorceress who grants wishes.",
"One day, our hero looks out of the window and a beanstalk has magically appeared.",
"One day, our hero is curious and drinks a rainbow-coloured potion.",
"One day, our hero goes to a science fair and gets bitten by a radioactive spider.",
"One day, our hero falls in love with a housemaid that was turned into a duster.")

third <- c("Then, they get eaten by a famished wolf.", 
"As a result, they turn into a beautiful woman.", 
"They end up knocking out a gentle giant after stealing his magic harp and goose.",
"As a result, they miraculously transform into a horrifying monster.",
"Then, they rescue their partner who had been captured by an elderly female ape.",
"One day, a certain Mr. Potter offers to fix all their problems, but he is only partially successful.")

fourth <- c("And they lived happily ever after, having unravelled all the mysteries of coding.",
"And they had to see an unaffordable therapist for ever after.",
"And they all changed into a magnificent herd of bicolored unicorns.")


# Part 2 Story listing

Story1 <- sprintf("One day, our hero encounters a %s Dragon called %s. 
The dragon was trying to destroy a village filled with people with %s hair and %s face. 
The hero drew out the lengendary weapon, %s and shouted at the dragon: '%s!' 
A bolt of lightning suddenly appeared from the weapon and struck the dragon. 
In a quick flash and a loud '%s!', the dragon vanished and never appeared again. 
The last word from the dragon was %s! So, our hero saved the day again!", A, B, C, D, E, F, G, H)

Story2 <- sprintf("One day, our hero decides to go into the %s mountains to find %s. 
After travelling during hours, he finds a %s and %s shaped house. 
All of a sudden, dark unicorns come from the sky, armed with %s. 
Our hero, who knew a lot about dark unicorns, starts to yell and repeat %s. Well Done!
The unicorns disappear and the hero enters in the house. The door creeks: %s. 
Intrigued, our hero keeps on exploring the house, but %s, he runs into a hord of zombies! 
Our hero was never seen again…", A, B, C, D, E, F, G, H)

Story3 <- sprintf("One day, our hero was exploring the 'Gourmet Island' filled with delicious food.
Suddenly, a dark figure appeared and our hero recognized it as the %s hungry demon, %s.
The demon is after the legendary %s, %s fruit that gives anyone who consumed it incredible power.
To prevent the demon from getting the power, our hero used the holy weapon %s that he obtained in recent adventure to fight.
To activate the special power of the weapon, our hero shouted '%s!'
Golden colour flashes appeared and with a loud %s, the demon is banished back to hell! %s, we expected nothing less from our hero!",  A, B, C, D, E, F, G, H)


# Part 3 Words listing by category

getwd()
setwd("C:/Users/Laura/Documents/GitHub/RepoLauraProgramming/Project/Story.Generator/Text files")

adj <- scan("adjectives.txt", what = "")
an_adj <- c(adj)

adv <- scan("an_adv.txt", what = "")
an_adv <- c(adv)

noun <- scan("anoun.txt", what = "")
a_noun <- c(noun)

name <- scan("aname.txt", what = "")
a_name <- c(name)

verb <- scan("verbs.txt", what = "")
a_verb <- c(verb)

a_connection = c("and the", "on the back of the", "unbeknownst to the", "with the help of the", 
                 "disobeying the" , "unlike the", "obeying the", "pleasing the", "infuriating the", 
                 "using the", "therefore betraying the")

a_CCT = c("Yesterday","Once upon a time", "A year or two ago", "Late last night", "This morning", 
          "Before the chime of midnight", "Every other day during two years",
          "As a child,", "During the Greek Golden Era", "While eating coconuts on a barren island",
          "At dinner", "Last Sunday", "During Econometrics Class", "During Tyler's programming class")

your_story <- c(rep(0, 4))
  
story_generator <- function(character, place, ending){
    if(character == "object"){
        your_story[1] <- first[6]
    }else if(character == "boy"){
        your_story[1] <- sample(first[3:5], 1)
    }else{
        your_story[1] <- sample(first[1:2], 1)
    }
    
    if(place == "outside"){
        your_story[2] <- sample(second[1:3], 1)}
    else{
        your_story[2] <- sample(second[4:6], 1)}
    
    if(ending == "happy"){
        your_story[4] <- fourth[1]}
    else if(ending =="sad"){
        your_story[4] <- fourth[2]}
    else{
        your_story[4] <- sample(fourth, 1)}
        
    your_story[3] <- sample(third, 1)
        
    print(your_story)
    
}


story_gen <- function(){
    X <- round(runif(1, min=1, max=3), digit=0)
    # print(X)
    if(X == 1){
        Story1
    }else if(X == 2){
        Story2
    }else{
        Story3
    }
}

pick = function(x){
  length(x)
  y = sample(1:length(x), 1)
  x[y]
}

Kadavresky = function() {
  a = rep(0,10)
  a[1]= pick(a_CCT)
  a[5] = "the"
  a[8] = pick(a_connection)
  a[2]= pick(a_name)
  a[3] = pick(an_adv)
  a[4] = pick(a_verb)
  a[6] = pick(an_adj)
  a[7] = pick(a_noun)
  a[9] = pick(an_adj)
  a[10] = pick(a_noun)
  cat(a)
}

story_generator(character, place, ending)

story_gen()

Kadavresky()



