# Production at least cost to the planet
# Scenario generator

# Key terms ####


# Packages ####

library(tidyverse)
library(geomtextpath)
library(gganimate)
library(ggnewscale)

# Parameters ####

# Simulation range
demand.spec <- seq(0.1,1,0.1) # Spectrum of demands to explore
reserve.spec <- seq(0,0.99,0.01) # Spectrum of reserve sizes to explore
sensitivity.spec <- seq(0.01,1,0.01) # Spectrum of sensitivities to explore

# Pristine values
concern.start <- 1 # Pristine concern per unit area
product.start <- 1 # Pristine product per unit area

# Efficiency parameters
efficiency.exponent <- 0
efficiency.scale <- 1

# Plot relationship between sensitivity and efficiency
sensitivity <- seq(1,0,-0.01)
efficiency <- ((sensitivity^efficiency.exponent)*efficiency.scale)
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

# Availability parameters
availability.exponent <- 1/2
availability.scale <- 1

# Plot relationship between reserve and availability
reserve <- seq(0,0.99,0.01)
availability <- ((((1-reserve))^availability.exponent)*availability.scale)
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

# Fulfillment parameters
fulfillment.exponent <- 1
fulfillment.scale <- 1

# Plot relationship between intensity and fulfillment
intensity <- seq(0,1,0.01)
reserve <- 0
sensitivity <- 1
efficiency <- ((sensitivity^efficiency.exponent)*efficiency.scale)
(fulfillment <- ((efficiency * intensity * (((1-reserve)^availability.exponent)*availability.scale))^fulfillment.exponent)*fulfillment.scale)
fulfillment.df <- cbind.data.frame(intensity, fulfillment)
fulfillment.plot <- ggplot(fulfillment.df, aes(x = intensity, y = fulfillment)) +
  geom_line() +
  scale_x_continuous(name = "Intensity") +
  scale_y_continuous(name = "Fulfillment") +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dotted") +
  theme_bw()
fulfillment.plot

# Concern parameters
loss.exponent <- 1/2 # x > 1 = concave density-yield. 1 < x < 0 = convex density-yield
loss.scale <- 1

# Plot relationship between production intensity and amount of concern
demand <- seq(0,1,0.01)
reserve <- 0
sensitivity <- 1
efficiency <- ((sensitivity^efficiency.exponent)*efficiency.scale)
(intensity <- demand / (efficiency * (((1-reserve)^availability.exponent)*availability.scale)))
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

results.list <- list()
iterations <- 1
start.time <- Sys.time()
for(i in demand.spec){ # Demand
  for(j in reserve.spec){ # Reserve size
    for(k in sensitivity.spec){ # Sensitivity
      demand <- i
      reserve <- j
      sensitivity <- k
      
      # If efficiency is less than 0, make it 0
      efficiency <- ifelse(((sensitivity^efficiency.exponent)*efficiency.scale) > 0,
                           ((sensitivity^efficiency.exponent)*efficiency.scale), 0)
      
      # If efficiency is greater than 0, continue, else end the simulation
      if(efficiency > 0){
        print(paste("demand", demand, "reserve", reserve, "sensitivity", sensitivity, sep = " "))
        (intensity <- demand / (efficiency * (((1-reserve)^availability.exponent)*availability.scale)))
        concern <- NA
        impact <- NA
        loss <- NA
        # If intensity is less than X, continue, else end the simulation
        if(intensity < 999){
          # Fulfillment = how much product is produced in practice
          (fulfillment <- ((efficiency * intensity * (((1-reserve)^availability.exponent)*availability.scale))^fulfillment.exponent)*fulfillment.scale)
          # If fulfillment is less than available product given reserve, continue, else end the simulation
          if(fulfillment < (((1-reserve)^availability.exponent)*availability.scale)){
            (loss <- (((concern.start * (1-reserve)) * sensitivity * intensity)^(loss.exponent))*loss.scale)
            # If the loss is greater than the concern in the productive zone, then everything, but no more than that, is lost in the productive zone
            (loss <- ifelse(loss > (concern.start * (1-reserve)), (concern.start * (1-reserve)), loss))
            (concern <- concern.start - loss)
            (impact <- (concern.start/concern.start) * sensitivity * (1 / efficiency))
            temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
            results.list[[iterations]] <- temp.df
            iterations <- iterations+1
          } else {
            temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
            results.list[[iterations]] <- temp.df
            iterations <- iterations+1
          }
        } else {
          temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
          results.list[[iterations]] <- temp.df
          iterations <- iterations+1
        }
      } else {
        temp.df <- cbind.data.frame(demand, reserve, sensitivity, impact, concern, intensity, loss)
        results.list[[iterations]] <- temp.df
        iterations <- iternations+1
      }
    }
  }
}

# Process results ####

results.df <- data.table::rbindlist(results.list)
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
  scale_y_continuous(name = "Sensitivity")
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
product.price <- 1 # Revenue per unit of product
intensity.price <- 0 # Expenses per unit of intensity
services.price <- 1 # Revenue per unit of concern

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

# Diagnostic plots ####
# diagnostic.df <- costs.df %>% filter(demand == min(demand.spec))
# # Reserve change cost
# ggplot(diagnostic.df, aes(x = reserve, y = reserve.change.cost)) +
#   geom_line() +
#   scale_x_continuous("Reserve") +
#   scale_y_continuous("Cost") +
#   theme_bw()
# # Reserve ongoing cost
# ggplot(diagnostic.df, aes(x = reserve, y = reserve.ongoing.cost)) +
#   geom_line() +
#   scale_x_continuous("Reserve") +
#   scale_y_continuous("Cost") +
#   theme_bw()
# # Sensitivity change cost
# ggplot(diagnostic.df, aes(x = sensitivity, y = sensitivity.change.cost)) +
#   geom_line() +
#   scale_x_continuous("Sensitivity") +
#   scale_y_continuous("Cost") +
#   theme_bw()
# # Sensitivity ongoing cost
# ggplot(diagnostic.df, aes(x = sensitivity, y = sensitivity.ongoing.cost)) +
#   geom_line() +
#   scale_x_continuous("Sensitivity") +
#   scale_y_continuous("Cost") +
#   theme_bw()
# # Revenue ongoing return
# ggplot(diagnostic.df, aes(x = demand, y = revenue.ongoing)) +
#   geom_line() +
#   geom_point() +
#   scale_x_continuous("Demand") +
#   scale_y_continuous("Product revenue") +
#   theme_bw()
# # Services expenses ongoing return
# ggplot(diagnostic.df, aes(x = concern, y = services.ongoing)) +
#   geom_line() +
#   scale_x_continuous("Concern") +
#   scale_y_continuous("Concern revenue") +
#   theme_bw()

# Plot costs for a particular scenario ####

plot.df <- costs.df %>% filter(demand == min(demand.spec))
optimal <- plot.df %>% filter(net.cost >= budget) %>% 
  slice_max(concern) %>% 
  slice_max(net.cost)

costs.plot <- ggplot() +
  geom_raster(data = plot.df, aes(x = reserve, y = sensitivity, fill = concern)) +
  geom_textcontour(data = plot.df, aes(x = reserve, y = sensitivity, z = net.cost, colour = "Net profit/loss")) +
  scale_colour_manual(name = "Costs", values = c("Net profit/loss" = "white")) +
  guides(colour = guide_legend(override.aes = aes(label = "1"))) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "#2a8b8a", color = NA)) +
  new_scale_color() + # Data below will require a new colour scale
  geom_point(aes(x = reserve.start, y = sensitivity.start, colour = "Starting management"), size = 3) +
  geom_point(data = optimal, aes(x = reserve, y = sensitivity, colour = "Optimal management\ngiven constraints"), size = 3) +
  scale_fill_viridis_c(name = "Concern outcome", na.value = "transparent") +
  scale_colour_manual(name = "Management", 
                      values = c("Starting management" = "black", 
                                 "Optimal management\ngiven constraints" = "red")) +
  scale_x_continuous(name = "Reserve") +
  scale_y_continuous(name = "Sensitivity") +
  ggtitle(paste("Demand =", min(plot.df$demand))) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "#2a8b8a", color = NA))
costs.plot
