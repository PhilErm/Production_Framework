# Production at least cost to the planet
# Emergent model

# Key terms ####


# Packages ####

library(deSolve)
library(tidyverse)
library(geomtextpath)

# Specify and solve model ####

# Specify system of equations
system <- function(t, n, parameters){
  with(as.list(parameters), {
    
    # Target species
    target.out <-  r*n[1]*(1-(n[1]/(1 - s)*K)) - q*n[3]*n[1] + m*((1-s)*n[2] - s*n[1])
    
    target.in <-  r*n[2]*(1-(n[2]/(s*K))) + m*(s*n[1] - (1-s)*n[2])            
    
    # Fishing effort
    effort.out <- ((c/(n[1]*q))-n[3])
    
    # Non-target 1
    non.target.1.out <-  r1*n[4]*(1-(n[4]/(1 - s)*K1)) - q1*n[3]*n[4] + m1*((1-s)*n[5] - s*n[4])
    
    non.target.1.in <-  r1*n[5]*(1-(n[5]/(s*K1))) + m1*(s*n[4] - (1-s)*n[5])   
    
    # Non-target 2
    non.target.2.out <-  r2*n[6]*(1-(n[6]/(1 - s)*K2)) - q2*n[3]*n[6] + m2*((1-s)*n[7] - s*n[6])
    
    non.target.2.in <-  r2*n[7]*(1-(n[7]/(s*K2))) + m2*(s*n[6] - (1-s)*n[7])   
    
    # Non-target 3
    non.target.3.out <-  r3*n[8]*(1-(n[8]/(1 - s)*K3)) - q3*n[3]*n[8] + m3*((1-s)*n[9] - s*n[8])
    
    non.target.3.in <-  r3*n[9]*(1-(n[9]/(s*K3))) + m3*(s*n[8] - (1-s)*n[9])   
    
    # Non-target 4
    non.target.4.out <-  r4*n[10]*(1-(n[10]/(1 - s)*K4)) - q4*n[3]*n[10] + m4*((1-s)*n[11] - s*n[10])
    
    non.target.4.in <-  r4*n[11]*(1-(n[11]/(s*K4))) + m4*(s*n[10] - (1-s)*n[11])   
    
    # Non-target 5
    non.target.5.out <-  r5*n[12]*(1-(n[12]/(1 - s)*K5)) - q5*n[3]*n[12] + m5*((1-s)*n[13] - s*n[12])
    
    non.target.5.in <-  r5*n[13]*(1-(n[13]/(s*K5))) + m5*(s*n[12] - (1-s)*n[13])
    
    # Combine equations into list
    list(c(target.out, 
           target.in, 
           effort.out, 
           non.target.1.out, 
           non.target.1.in,
           non.target.2.out, 
           non.target.2.in,
           non.target.3.out, 
           non.target.3.in,
           non.target.4.out, 
           non.target.4.in,
           non.target.5.out, 
           non.target.5.in))
  })
}

# Simulation range
targeted.r <- 1
common.K <- 1
reserve.size <- 0
demand.spec <- seq(((targeted.r*common.K/4)*(1-reserve.size))*0.5,((targeted.r*common.K/4)*(1-reserve.size))*0.5,0) # Spectrum of demands to explore
reserve.spec <- seq(0,0.9,0.1) # Spectrum of reserve sizes to explore
sensitivity.spec <- seq(0.2,2,0.2) # Spectrum of sensitivities to explore

results.list <- list()
iterations <- 1
start.time <- Sys.time()
for(i in demand.spec){ # Demand
  for(j in reserve.spec){ # Reserve size
    for(k in sensitivity.spec){ # Sensitivity
      
# Specify parameters
reserve.size <- j
targeted.q <- 0.1
q.modifier <- k
common.m <- 0.3
catch <- i
demand <- catch
reserve <- reserve.size
sensitivity <- q.modifier
print(paste("demand", demand, "reserve", reserve, "sensitivity", sensitivity, sep = " "))

parameters <- c(s=reserve.size, c=catch, # Universal
                r=targeted.r, K=common.K, q=targeted.q, m=common.m, # Target
                r1=0.5, K1=common.K, q1=0.20*q.modifier, m1=common.m, # Non-target 1
                r2=0.5, K2=common.K, q2=0.30*q.modifier, m2=common.m, # Non-target 2
                r3=0.5, K3=common.K, q3=0.40*q.modifier, m3=common.m, # Non-target 3
                r4=0.5, K4=common.K, q4=0.50*q.modifier, m4=common.m, # Non-target 4
                r5=0.5, K5=common.K, q5=0.60*q.modifier, m5=common.m) # Non-target 5

# Specify initial variable values
initial.n <- c(common.K*(1-reserve.size), # Target out
              common.K*reserve.size, # Target in
              0, # Effort
              common.K*(1-reserve.size), # Non-target 1 out
              common.K*reserve.size, # Non-target 1 in
              common.K*(1-reserve.size), # Non-target 2 out
              common.K*reserve.size, # Non-target 2 in
              common.K*(1-reserve.size), # Non-target 3 out
              common.K*reserve.size, # Non-target 3 in
              common.K*(1-reserve.size), # Non-target 4 out
              common.K*reserve.size, # Non-target 4 in
              common.K*(1-reserve.size), # Non-target 5 out
              common.K*reserve.size) # Non-target 5 in

# Specify time sequence
time <- seq(0,50,1)

# Solve ODEs
output <- ode(y = initial.n, 
           times = time, 
           func = system, 
           parms = parameters,
           maxsteps = 50)

# Process results ####

# Save results as vectors
time <- output[,1]
target.n.out <- output[,2]
target.n.in <- output[,3]
effort <- output[,4]
non.target.1.n.out <- output[,5]
non.target.1.n.in <- output[,6]
non.target.2.n.out <- output[,7]
non.target.2.n.in <- output[,8]
non.target.3.n.out <- output[,9]
non.target.3.n.in <- output[,10]
non.target.4.n.out <- output[,11]
non.target.4.n.in <- output[,12]
non.target.5.n.out <- output[,13]
non.target.5.n.in <- output[,14]

# Bind results
results.df <- cbind.data.frame(time, 
                               effort,
                               #target.in = target.n.in,
                               target.out = target.n.out,
                               target = target.n.out + target.n.in,
                               #non.target.1.in = non.target.1.n.in,
                               #non.target.1.out = non.target.1.n.out,
                               non.target.1 = non.target.1.n.out + non.target.1.n.in,
                               non.target.2 = non.target.2.n.out + non.target.2.n.in,
                               non.target.3 = non.target.3.n.out + non.target.3.n.in,
                               non.target.4 = non.target.4.n.out + non.target.4.n.in,
                               non.target.5 = non.target.5.n.out + non.target.5.n.in)

# Calculate catch
results.df <- results.df %>% 
  mutate(catch = target.out * effort * targeted.q)

# Tidy results
results.tidy <- results.df %>% pivot_longer(cols = c(target, non.target.1, non.target.2, non.target.3, non.target.4, non.target.5, effort, catch),
                                            names_to = "variable",
                                            values_to = "value")

# Plot results ####

# ggplot(results.tidy, aes(x = time,  y = value, colour = variable, label = variable)) +
#   geom_textpath() +
#   #geom_point() +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   theme_bw()

# Make sure format follows other ones. But you can do that later
# We just need sensitivity & demand & reserve size

results.final <- filter(results.tidy, time == last(time))
results.final$q.modifier <- q.modifier
results.final$catch <- catch
results.final$reserve.size <- reserve.size
results.final

results.summary <- results.final %>% filter(variable != "effort" & variable != "catch")

gm.mean <- function(x){
  prod(x, na.rm = F)^(1/length(x))
}

(concern <- gm.mean(results.summary$value))
time <- last(time)
(results.summary <- cbind.data.frame(concern,demand,reserve,sensitivity,time))
results.list[[iterations]] <- results.summary
iterations <- iterations+1
    }
  }
}
results.df <- data.table::rbindlist(results.list)
results.df$concern[results.df$concern < 0] <- 0 # Change any negative concerns to 0
results.df <- results.df %>% 
  filter(time == 50)

#View(results.df)

plotly::plot_ly(
  results.df %>% filter(!is.na(concern)),
  x = ~demand,
  y = ~reserve,
  z = ~sensitivity,
  color = ~concern)

ggplot(results.df, aes(x = reserve, y = sensitivity, z = demand, fill = concern)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Concern", na.value = "transparent") +
  scale_x_continuous(name = "Reserve") +
  scale_y_continuous(name = "Sensitivity")

# And that's done. Just simulate with a better resolution