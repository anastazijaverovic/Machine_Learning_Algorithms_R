# Upper Confidence Bound

# Import the dataset

dataset = read.csv("Ads_CTR_Optimisation.csv")

# find out which version of the ad will result in the most clicks


# round - each time a user logs in the website
# 1 - user clicked on the ad

# in each round - algorithm decides which ad to show to the user on the basis of rewards of the rounds up to that round

# STEP 1

# numbers_of_selections - vector containing number of times ad i was selected up to round n
# integer(d) - vector containing only 0
# d - number of ads

d = 10

# sums_of_rewards - sum of rewards up to round n
numbers_of_selections = integer(d)
sums_of_rewards = integer(d)

# STEP 2
# calculate average reward of the ad i up to round n
# calculate confidence interval at round n

# n - round
# N - total number of rounds - 10000
# ads_selected - vector with different versions of the ad that were selected at each round
# upper_bound - upper bound for each ad version in the round
# max_upper_bound - highest upper confidence bound - maximum of all of the upper bounds

N = 10000
ads_selected = integer(d)

for(n in 1:N) {
  ad = 0
  max_upper_bound = 0
  
  for(i in 1:d) {
    if(numbers_of_selections[i] > 0) {   # going through the first 10 rounds, in the first round no ad was selected
    average_reward = sums_of_rewards[i] / numbers_of_selections[i]
    delta_i = sqrt(3/2 * log(n)/numbers_of_selections[i])
    upper_bound = average_reward + delta_i
    } else {
      upper_bound = 1e400
    }
    if(upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound     # STEP 3
      ad = i
    }
    }
}




