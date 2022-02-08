# Suppose you have the vector beads from a previous video:
  
  beads <- rep(c("red", "blue"), times = c(2,3))
  beads

# To find the probability of drawing a blue bead at random, you can run:
    
  mean(beads == "blue")
