
T = 60 
lambda = function(t){3 + (t^2) * (2^(-t))} #Intensity function
lambda_max = 5 #Value used to bound the intensity function (Maximum found by hand) and create our probability / thinning function
prob_function = function(t){(3 + (t^2) * (2^(-t)))/lambda_max} #Thinning function
m = 3*T*lambda_max #Using large number to generate event times to ensure T=60 is reached
X = rexp(m, rate = 5) #Simulating homogeneous process with rate lambda_max = 5
S = cumsum(X) #Event times S'

u1 = runif(m) #Will be used to accept or reject events
#initializing accepted and rejected event vectors:
accepted_events = c()
rejected_events = c()

for(i in 1:m){
  if(u1[i] <= prob_function(S[i])){ #Evaluating probability function at generated S' event times
    accepted_events = c(accepted_events, S[i]) #Add to accepted events
  }
  else{
    rejected_events = c(rejected_events, S[i]) #Add to rejected events
  }
}

#keeping the events that occurred before time T=60:
accepted_events = accepted_events[accepted_events<T] #accepted events vector becomes our generated event times before T=60
rejected_events = rejected_events[rejected_events<T]#Same logic for rejected events

length(accepted_events) # Number of events before T=60

#Average inter-arrival time:
avg_inter_event_time = mean(diff(c(0,accepted_events))) #diff function used to calculate inter-event times
avg_inter_event_time

#rejections per accepted event (i.e. average number of rejections):
avg_num_rejections = length(rejected_events) / length(accepted_events) 
avg_num_rejections

#number of rejections Mi needed for event time Si
M = c()
for(i in 1:length(accepted_events)){
  M[i] = length(rejected_events[rejected_events < accepted_events[i]]) #taking subset of rejected events that are less than the ith accepted events
}
M
