

# Define the current status score for each component:
Chemicals  <- 100
Sedimentation   <- 100
Eutrophication   <- 10
Marine_Debris <- 100
Pathogens  <- 100

scores <- c(Chemicals, Sedimentation, Eutrophication, Marine_Debris, Pathogens)

# Arithmetic mean
mean(scores) # 82

# Geometric mean
(Chemicals * Sedimentation * Eutrophication * Marine_Debris * Pathogens)^(1 / length(scores)) # 63.09573


###############################
# How do we obtain the scores defined above?
## -> by averaging the current status scores for each indicator 