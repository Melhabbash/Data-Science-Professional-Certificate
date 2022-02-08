# One ball will be drawn at random from a box containing: 3 cyan balls, 
# 5 magenta balls, and 7 yellow balls.
# 
# What is the probability that the ball will be cyan?

beads <- rep(c("cyan", "magenta", "yellow"), times = c(3,5,7))    
beads    # view beads object

mean(beads == "cyan")

#######################

# Probability of not cyan
# 1 point possible (graded)
# One ball will be drawn at random from a box containing: 3 cyan balls, 
# 5 magenta balls, and 7 yellow balls.
# 
# What is the probability that the ball will not be cyan?

beads <- rep(c("cyan", "magenta", "yellow"), times = c(3,5,7))    
beads    # view beads object

mean(beads != "cyan")

######################

# Sampling without replacement
# 1 point possible (graded)
# Instead of taking just one draw, consider taking two draws. You take the 
# second draw without returning the first draw to the box. We call this 
# sampling without replacement.
# 
# What is the probability that the first draw is cyan and that the second draw 
# is not cyan?
# Provide at least 3 significant digits.

# P(A and !A)=P(A)P(!A|A)
# P(A)=0.2
# P(!A|A)=12/14=6/7
# P(A and !A)=(0.2)(6/7)=0.1714

########################

# Sampling with replacement
# 1 point possible (graded)
# Now repeat the experiment, but this time, after taking the first draw and 
# recording the color, return it back to the box and shake the box. We call 
# this sampling with replacement.
# 
# What is the probability that the first draw is cyan and that the second draw 
# is not cyan?

# P(A and !A)=P(A)P(!A|A)
# P(A)=0.2
# P(!A|A)=12/15=4/5
# P(A and !A)=(0.2)(4/5)=0.16

