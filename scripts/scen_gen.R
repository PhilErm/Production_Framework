# Production at least cost to the planet
# Scenario generator

# Key terms ####


# Packages ####

library(tidyverse)
library(geomtextpath)
library(gganimate)
library(ggnewscale)

# Parameters ####

concern.start <- 1 # Pristine concern per unit area
product.start <- 1 # Pristine product per unit area
demand.spec <- seq(0.1,0.1,0) # Spectrum of demands to explore
reserve.spec <- seq(0,0.99,0.01) # Spectrum of reserve sizes to explore
sensitivity.spec <- seq(0.01,1,0.01) # Spectrum of sensitivities to explore

# Define relationship between concern sensitivity and product efficiency ####
# (I.e., change per unit of intensity)

efficiency.func <- function(sensitivity, efficiency.option){
  if(efficiency.option == 1){ # Efficiency and sensitivity are independent
    efficiency <- 1
    return(efficiency)
  } else if(efficiency.option == 2){ # Efficiency and sensitivity are the same
    efficiency <- sensitivity
    return(efficiency)
  } else if(efficiency.option == 3){ # Efficiency increases quickly, then with diminishing returns
    efficiency <- sensitivity^(1/2)
    return(efficiency)
  } else if(efficiency.option == 4){ # Efficiency increases slowly, then with compounding returns
    efficiency <- sensitivity^2
    return(efficiency)
  }
}

# Plot relationship between sensitivity and efficiency
efficiency.option <- 1
sensitivity <- seq(1,0,-0.01)
efficiency <- efficiency.func(sensitivity = sensitivity,
                              efficiency.option = efficiency.option)
efficiency.df <- cbind.data.frame(sensitivity, efficiency)
efficiency.plot <- ggplot(efficiency.df, aes(x = sensitivity, y = efficiency)) +
  geom_line() +
  scale_x_continuous(name = "Sensitivity") +
  scale_y_continuous(name = "Efficiency") +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dotted") +
  theme_bw()
efficiency.plot

# Define relationship between area under production and product availability ####

availability.func <- function(reserve, availability.option){
  if(availability.option == 1){ # Availability decreases linearly with area open to production
    availability <- 1-reserve
    return(availability)
  } else if(availability.option == 2){ # Availability decreases rapidly as more area reserved
    availability <- (1-reserve)^2
    return(availability)
  } else if(availability.option == 3){ # Availability decreases slowly as more area reserved
    availability <- (1-reserve)^(1/2)
    return(availability)
  }
}

# Plot relationship between reserve and availability
availability.option <- 1
reserve <- seq(0,0.99,0.01)
availability <- availability.func(reserve = reserve,
                                  availability.option = availability.option)
availability.df <- cbind.data.frame(reserve, availability)
availability.plot <- ggplot(availability.df, aes(x = reserve, y = availability)) +
  geom_line() +
  scale_x_continuous(name = "Reserve") +
  scale_y_continuous(name = "Availability") +
  geom_abline(slope = -1,
              intercept = 1,
              linetype = "dotted") +
  theme_bw()
availability.plot

# Plot relationship between production intensity and amount of concern
loss.exponent <- (1) # x > 1 = concave density-yield. 1 < x < 0 = convex density-yield
loss.scale <- 1
demand <- seq(0,1,0.01)
reserve <- 0
sensitivity <- 1
(intensity <- demand / (efficiency * availability.func(reserve, availability.option)))
(loss <- (((concern.start * (1-reserve)) * sensitivity * intensity)^loss.exponent)*loss.scale)
# If the loss is greater than the concern in the productive zone, then everything, but no more than that, is lost in the productive zone
(loss <- ifelse(loss > (concern.start * (1-reserve)), (concern.start * (1-reserve)), loss))
(concern <- concern.start - loss)
concern.df <- cbind.data.frame(demand, concern)
concern.plot <- ggplot(concern.df, aes(x = demand, y = concern)) +
  geom_line() +
  scale_x_continuous(name = "Demand") +
  scale_y_continuous(name = "Concern") +
  geom_abline(slope = -1,
              intercept = 1,
              linetype = "dotted") +
  theme_bw()
concern.plot

# Generate results ####

results.df <- data.frame()
for(i in demand.spec){ # Demand
  for(j in reserve.spec){ # Reserve size
    for(k in sensitivity.spec){ # Sensitivity
      demand <- i
      reserve <- j
      sensitivity <- k
      
      # If efficiency is less than 0, make it 0
      efficiency <- ifelse(efficiency.func(sensitivity, efficiency.option) > 0,
                           efficiency.func(sensitivity, efficiency.option), 0)
      
      # If efficiency is greater than 0, continue, else end the simulation
      if(efficiency > 0){
        print(paste("demand", demand, "reserve", reserve, "sensitivity", sensitivity, sep = " "))
        (intensity <- demand / (efficiency * availability.func(reserve, availability.option)))
        concern <- NA
        impact <- NA
        loss <- NA
        # If intensity is less than X, continue, else end the simulation
        if(intensity < 999){
          # Fulfillment = how much product is produced in practice
          (fulfillment <- efficiency * intensity * availability.func(reserve, availability.option))
          # If fulfillment is less than available product given reserve, continue, else end the simulation
          if(fulfillment < availability.func(reserve, availability.option)){
            (loss <- (((concern.start * (1-reserve)) * sensitivity * intensity)^(loss.exponent))*loss.scale)
            # If the loss is greater than the concern in the productive zone, then everything, but no more than that, is lost in the productive zone
            (loss <- ifelse(loss > (concern.start * (1-reserve)), (concern.start * (1-reserve)), loss))
            (concern <- concern.start - loss)
            (impact <- (concern.start/concern.start) * sensitivity * (1 / efficiency))
            temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
            results.df <- rbind.data.frame(results.df, temp.df)
          } else {
            temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
            results.df <- rbind.data.frame(results.df, temp.df)
          }
        } else {
          temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
          results.df <- rbind.data.frame(results.df, temp.df)
        }
      } else {
        temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
        results.df <- rbind.data.frame(results.df, temp.df)
      }
    }
  }
}

# Process results ####

results.df$concern[results.df$concern < 0] <- 0 # Change any negative concerns to 0
results.df$concern[results.df$concern > concern.start] <- concern.start # Reduce any losses above limit
results.df <- results.df %>% mutate(production.area = (1-reserve)) # Calculate the area put in production

# Plot results without costs ####

# 3D plot
# plotly::plot_ly(
#   results.df %>% filter(!is.na(concern)),
#   x = ~demand,
#   y = ~reserve,
#   z = ~sensitivity,
#   color = ~concern)

# Animation
animation.plot <- ggplot(results.df, aes(x = reserve, y = sensitivity, z = demand, fill = concern)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Concern", na.value = "transparent") +
  scale_x_continuous(name = "Reserve") +
  scale_y_continuous(name = "Sensitivity") +
animation.plot

animation <- animation.plot +
  transition_time(demand) +
  labs(title = "Demand: {frame_time}") +
  ease_aes(default = "linear")

fps <- 25
duration <- 7
#animate(animation, fps = fps, duration = duration, start_pause = fps*1, end_pause = fps*2, height = 5, width = 10, res = 225, units = "in")

# Calculate costs for particular scenario ####

# Starting conditions
reserve.start <- 0.25
sensitivity.start <- 0.25
budget <- -1000

# Costs associated with reserves
reserve.change.price <- 1 # Price to move reserve by 0.1
reserve.change.exponent <- 1 # x > 1 = exponentially less expensive. 1 < x < 0 = exponentially more expensive
reserve.ongoing.price <- 1 # Ongoing cost of reserving whole seascape
reserve.ongoing.exponent <- 1

# Costs associated with impact
sensitivity.change.price <- 1 # Price to move sensitivity by 0.1
sensitivity.change.exponent <- 1 # x > 1 = exponentially less expensive. 1 < x < 0 = exponentially more expensive
sensitivity.ongoing.price <- 1 # Ongoing cost of keeping sensitivity at 0
sensitivity.ongoing.exponent <- 1

# Costs associated with product
product.price <- 0 # Revenue per unit of product
intensity.price <- 0 # Expenses per unit of intensity
services.price <- 0 # Revenue per unit of concern

costs.df <- results.df %>% 
  mutate(reserve.change.cost = (abs(reserve.start-reserve)^reserve.change.exponent)*reserve.change.price,
         sensitivity.change.cost = (abs(sensitivity.start-sensitivity)^sensitivity.change.exponent)*sensitivity.change.price,
         total.change.cost = (sqrt((reserve.change.cost^2)+(sensitivity.change.cost^2)))*(concern/concern),
         reserve.ongoing.cost = (reserve*reserve.ongoing.price)^reserve.ongoing.exponent,
         sensitivity.ongoing.cost = ((1-sensitivity)*sensitivity.ongoing.price)^sensitivity.ongoing.exponent,
         total.ongoing.cost = (reserve.ongoing.cost + sensitivity.ongoing.cost)*(concern/concern),
         revenue.ongoing = demand*product.price,
         expenses.ongoing = intensity*intensity.price,
         services.ongoing = concern*services.price,
         net.cost = (total.change.cost+total.ongoing.cost-revenue.ongoing+expenses.ongoing-services.ongoing)*-1)

# Plot costs for a particular scenario ####

plot.df <- costs.df %>% filter(demand == min(demand.spec))
optimal <- plot.df %>% filter(net.cost >= budget) %>% 
  slice_max(concern) #%>% 
  #slice_max(net.cost)

costs.plot <- ggplot() +
  geom_raster(data = plot.df, aes(x = reserve, y = sensitivity, fill = concern)) +
  geom_textcontour(data = plot.df, aes(x = reserve, y = sensitivity, z = net.cost, colour = "Net profit/loss")) +
  scale_colour_manual(name = "Costs", values = c("Net profit/loss" = "white")) +
  guides(colour = guide_legend(override.aes = aes(label = "1"))) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "#2a8b8a", color = NA)) +
  new_scale_color() + # Data below will require a new colour scale
  geom_point(aes(x = reserve.start, y = sensitivity.start, colour = "Starting management")) +
  geom_point(data = optimal, aes(x = reserve, y = sensitivity, colour = "Optimal management\ngiven constraints")) +
  scale_fill_viridis_c(name = "Concern outcome", na.value = "transparent") +
  scale_colour_manual(name = "Management", 
                      values = c("Starting management" = "black", 
                                 "Optimal management\ngiven constraints" = "red")) +
  scale_x_continuous(name = "Reserve") +
  scale_y_continuous(name = "Sensitivity") +
  theme_bw() +
  theme(legend.key = element_rect(fill = "#2a8b8a", color = NA))
costs.plot
