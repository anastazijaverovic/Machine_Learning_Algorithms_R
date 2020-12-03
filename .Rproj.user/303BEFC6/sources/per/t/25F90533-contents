# Thompson Sampling

# Data preprocessing

dataset = read.csv("Ads_CTR_Optimisation.csv")

# Implementing Thompson Sampling

N = 10000 # number of rounds
d = 10 # number of ads

ads_selected = integer(0)
# for each ad - containing the number of times it got the reward 1 or 0 (for each ad -> vector)
numbers_of_rewards_1 = integer(d)
numbers_of_rewards_0 = integer(d)
total_reward = 0


for(n in 1:N) {
  ad = 0 # ad selected at specific round
  max_random = 0 # maximum random draw
  
  for(i in 1:d) {
    random_beta = rbeta(n = 1, # number of observations - we want one random draw
                        shape1 = numbers_of_rewards_1[i] + 1,
                        shape2 = numbers_of_rewards_0[i] + 1) # random draws from the beta distribution
  
    
    if(random_beta > max_random) {
      max_random = random_beta
      ad = i
    }
  }
  
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
    
  if(reward == 1) {
    # number of rewards for selected ad
    numbers_of_rewards_1[ad] = numbers_of_rewards_1[ad] + 1
  } else {
    numbers_of_rewards_0[ad] = numbers_of_rewards_0[ad] + 1
  }
  
  total_reward = total_reward + reward
}

# Visualise Thompson Sampling

hist(ads_selected,
     col = "light blue",
     main = "Histogram of ads selection for Thompson Sampling",
     xlab = "Ads",
     ylab = "Number of times each ad was selected")

  