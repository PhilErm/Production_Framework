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
demand.spec <- seq(0.01,0.99,0.01) # Spectrum of demands to explore
reserve.spec <- seq(0.01,0.99,0.01) # Spectrum of reserve sizes to explore
sensitivity.spec <- seq(0.01,0.99,0.01) # Spectrum of sensitivities to explore

# demand.spec <- seq(0.1,1,0.1) # Spectrum of demands to explore
# reserve.spec <- seq(0,0.9,0.1) # Spectrum of reserve sizes to explore
# sensitivity.spec <- seq(0.1,1,0.1) # Spectrum of sensitivities to explore

# Pristine values
concern.start <- 1 # Pristine concern per unit area
product.start <- 1 # Pristine product per unit area

# Efficiency parameters
efficiency.exponent <- 1/2
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
library(plotly)
plotly.df <- results.df
plotly.df$Biodiversity <- plotly.df$concern

axis.range <- c(0,1)
font.list <- list(family = "Arial")

axx <- list(
  title = "Reserve size",
  #ticktext = list("Low", "Medium", "High"),
  #tickvals = list(0,0.5,1),
  nticks = 10,
  range = axis.range,
  titlefont = font.list,
  tickfont = font.list
)

axy <- list(
  title = "Local impact",
  #ticktext = list("Low", "Medium", "High"),
  #tickvals = list(0,0.5,1),
  nticks = 10,
  range = axis.range,
  titlefont = font.list,
  tickfont = font.list
)

axz <- list(
  title = "Demand",
  #ticktext = list("Low", "Medium", "High"),
  #tickvals = list(0,0.5,1),
  nticks = 10,
  range = axis.range,
  titlefont = font.list,
  tickfont = font.list
)

plot_ly(
  plotly.df %>% filter(!is.na(concern)) %>% filter(demand == 0.2),
  x = ~reserve,
  y = ~sensitivity,
  z = ~demand) %>%
  add_markers(color = ~Biodiversity) %>%
layout(scene = list(xaxis = axx,
                    yaxis = axy,
                    zaxis = axz,
                    legend = list(title = list(font = list(family = "Times New Roman"))))
)

# Slice at particular demand
demand.slice <- 0.2
ggplot() +
  geom_raster(data = plotly.df %>% filter(demand == 0.2), aes(x = reserve, y = sensitivity, fill = Biodiversity)) +
  #scale_fill_viridis_c(name = "Biodiversity", na.value = "transparent", limits = c(min(plotly.df$Biodiversity),max(plotly.df$Biodiversity))) +
  scale_fill_viridis_c(name = "Biodiversity", na.value = "transparent", limits = c(min(plotly.df$Biodiversity,na.rm = F),max(plotly.df$Biodiversity,na.rm = F))) +
  
  scale_x_continuous(name = "Reserve size") +
  scale_y_continuous(name = "Local impact") +
  scale_y_reverse(name = "Local impact") +
  scale_x_reverse(name = "Reserve size") +
  ggtitle(paste("Demand =", demand.slice)) +
  theme_bw() +
  theme(legend.position="bottom")
    
# Cost mountain slice.



#%>%
  # layout(scene = list(xaxis = list(title = "Area", autorange = TRUE, showgrid = TRUE, zeroline = T, showline = T, autotick = T, ticks = '', showticklabels = T),
  #                     yaxis = list(title = "Local impact", autorange = TRUE, showgrid = TRUE, zeroline = T, showline = T, autotick = T, ticks = '', showticklabels = T),
  #                     zaxis = list(title = "Demadn", autorange = TRUE, showgrid = TRUE, zeroline = T, showline = T, autotick = T, ticks = '', showticklabels = T)
  #        )
  # )

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
#animate(animation, fps = fps, duration = duration, start_pause = fps*1, end_pause = fps*2, height = 5, width = 5, res = 225, units = "in")

# Calculate costs for particular scenario ####

# Starting conditions
reserve.start <- 0.10
sensitivity.start <- 0.25
budget <- 2.5

# Costs associated with reserves
reserve.change.price <- 1 # Price to move reserve by 0.1
reserve.change.exponent <- 1 # x > 1 = exponentially less expensive. 1 < x < 0 = exponentially more expensive
reserve.ongoing.price <- 0.5 # Ongoing cost of reserving whole seascape
reserve.ongoing.exponent <- 1

# Costs associated with impact
sensitivity.change.price <- 1 # Price to move sensitivity by 0.1
sensitivity.change.exponent <- 1 # x > 1 = exponentially less expensive. 1 < x < 0 = exponentially more expensive
sensitivity.ongoing.price <- 0.5 # Ongoing cost of keeping sensitivity at 0
sensitivity.ongoing.exponent <- 1

# Costs associated with product
product.price <- 5 # Revenue per unit of product
intensity.price <- 0.1 # Expenses per unit of intensity
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

# Diagnostic plots ####
diagnostic.df <- costs.df %>% filter(demand == min(demand.spec))
# # Reserve change cost
# ggplot(diagnostic.df, aes(x = reserve, y = reserve.change.cost)) +
#   geom_line() +
#   scale_x_continuous("Reserve") +
#   scale_y_continuous("Change cost") +
#   theme_bw()
# # # Reserve ongoing cost
# ggplot(diagnostic.df, aes(x = reserve, y = reserve.ongoing.cost)) +
#   geom_line() +
#   scale_x_continuous("Reserve") +
#   scale_y_continuous("Ongoing cost") +
#   theme_bw()
# # # Sensitivity change cost
# ggplot(diagnostic.df, aes(x = sensitivity, y = sensitivity.change.cost)) +
#   geom_line() +
#   scale_x_continuous("Sensitivity") +
#   scale_y_continuous("Change cost") +
#   theme_bw()
# # # Sensitivity ongoing cost
# ggplot(diagnostic.df, aes(x = sensitivity, y = sensitivity.ongoing.cost)) +
#   geom_line() +
#   scale_x_continuous("Sensitivity") +
#   scale_y_continuous("Ongoing cost") +
#   theme_bw()
# # # Revenue ongoing return
# ggplot(diagnostic.df, aes(x = demand, y = revenue.ongoing)) +
#   geom_line() +
#   geom_point() +
#   scale_x_continuous("Demand") +
#   scale_y_continuous("Ongoing revenue") +
#   theme_bw()
# # # Services expenses ongoing return
# ggplot(diagnostic.df, aes(x = concern, y = services.ongoing)) +
#   geom_line() +
#   scale_x_continuous("Concern") +
#   scale_y_continuous("Ongoing concern revenue") +
#   theme_bw()

# Plot costs for a particular scenario ####

plot.df <- costs.df %>% filter(demand == min(demand.spec))
optimal <- plot.df %>% filter(net.cost >= budget) %>% 
  slice_max(concern) %>% 
  slice_max(net.cost)

costs.plot <- ggplot() +
  geom_raster(data = plot.df, aes(x = reserve, y = sensitivity, fill = concern)) +
  geom_textcontour(data = plot.df, aes(x = reserve, y = sensitivity, z = net.cost, colour = "Net profit")) +
  scale_colour_manual(name = "Costs", values = c("Net profit" = "white")) +
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

costs.plot <- ggplot() +
  geom_raster(data = plot.df, aes(x = reserve, y = sensitivity, fill = concern)) +
  geom_textcontour(data = plot.df, aes(x = reserve, y = sensitivity, z = net.cost, colour = "Net profit")) +
  scale_colour_manual(name = "Costs", values = c("Net profit" = "white")) +
  guides(colour = guide_legend(override.aes = aes(label = "1"))) +
  theme_bw() +
  theme(legend.key = element_rect(fill = "#2a8b8a", color = NA)) +
  new_scale_color() + # Data below will require a new colour scale
  #geom_point(aes(x = reserve.start, y = sensitivity.start, colour = "Starting management"), size = 3) +
  #geom_point(data = pseudo.pareto.df, aes(x = reserve, y = sensitivity, colour = "Optimal management\ngiven constraints"), size = 3) +
  geom_line(data = pseudo.pareto.df, aes(x = reserve, y = sensitivity, colour = "Optimal management\ngiven constraints"), size = 1) +
  
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

costs.plot <- ggplot() +
  geom_raster(data = plot.df %>% filter(net.cost >= budget), aes(x = reserve, y = sensitivity, fill = concern)) +
  geom_textcontour(data = plot.df, aes(x = reserve, y = sensitivity, z = net.cost, colour = "Net profit")) +
  scale_colour_manual(name = "Costs", values = c("Net profit" = "white")) +
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

# Benefit divided by cost
costs.plot <- ggplot() +
  geom_raster(data = plot.df, aes(x = reserve, y = sensitivity, fill = concern/net.cost)) +
  #geom_textcontour(data = plot.df, aes(x = reserve, y = sensitivity, z = net.cost, colour = "Net profit")) +
  scale_colour_manual(name = "Costs", values = c("Net profit" = "white")) +
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

# # Attempting to make surfaces with plotly ####

test.df <- plot.df %>% 
  select(sensitivity, reserve, net.cost) %>% 
  pivot_wider(names_from = reserve, values_from = net.cost) %>% 
  print()
#View(test.df)
test.mat <- as.matrix(test.df)
test.mat <- test.mat[,-1]
View(test.mat)

# test.df <- plot.df %>% select(sensitivity, reserve, net.cost) 
# test.df <- spread(test.df, reserve, net.cost)


colour.df <- plot.df %>% 
  select(sensitivity, reserve, concern) %>% 
  pivot_wider(names_from = reserve, values_from = concern) %>% 
  print()
colour.mat <- as.matrix(colour.df)
colour.mat <- colour.mat[,-1]

axz$range <- c(-3,1)
axz$title <- "Profitability"

plot_ly() %>%
  add_surface(z = as.matrix(test.mat),
              surfacecolor = colour.mat,
              contours = list(
                z = list(show = TRUE, 
                         start = -1.8,
                         end = 0.6,
                         size = 0.1,
                         width = 4,
                         color = "white"))) %>% 
      layout(scene = list(#xaxis = axx,
                    #yaxis = axy,
                    zaxis = axz,
                    legend = list(title = list(font = list(family = "Times New Roman")))))
       


plotly::plot_ly(plot.df,
        x=plot.df$reserve,
        y=plot.df$sensitivity,
        z=plot.df$net.cost*-1,
        color=plot.df$concern,
        conours=list(z))

# Attempting to make surfaces with rayshader ####

# height <- ggplot() +
#   ggtitle(paste("Demand =", min(plot.df$demand))) +
#   geom_tile(data = plot.df, aes(x=reserve, y=sensitivity, fill=net.cost*-1)) +
#   geom_contour(data = plot.df, aes(x=reserve, y=sensitivity, z=net.cost*-1), colour = "black") +
#   scale_x_continuous("Reserve",expand = c(0,0)) +
#   scale_y_continuous("Sensitivity",expand = c(0,0)) +
#   scale_fill_viridis_c("Net cost") +
#   coord_fixed() +
#   theme(legend.position = "none")
# height
# 
# surface <- ggplot(data = plot.df) +
#   ggtitle(paste("Demand =", min(plot.df$demand))) +
# 
#   geom_tile(aes(x=reserve,y=sensitivity,fill=concern)) +
#   geom_textcontour(aes(x=reserve,y=sensitivity,z=net.cost), colour = "black") +
#   geom_point(aes(x = reserve.start, y = sensitivity.start, colour = "Starting management"), size = 3) +
#   geom_point(data = optimal, aes(x = reserve, y = sensitivity, colour = "Optimal management\ngiven constraints"), size = 3) +
#   scale_x_continuous("Reserve",expand = c(0,0)) +
#   scale_y_continuous("Sensitivity",expand = c(0,0)) +
#   scale_fill_viridis_c() +
#   scale_colour_manual(name = "Management",
#                       values = c("Starting management" = "black",
#                                  "Optimal management\ngiven constraints" = "red")) +
#   coord_fixed() +
#   theme(legend.position = "none")
# surface
# 
# rayshader::plot_gg(surface, ggobj_height = height,
#         multicore = TRUE, width = 5, height = 5,
#         scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 45, theta = 15,
#         shadow = FALSE, raytrace = FALSE, sunangle = 90)
# render_snapshot()

# For interpolation
# dat1 <- akima::interp(x = plot.df$reserve, y = plot.df$sensitivity, z = plot.df$net.cost, ny = 200, nx = 200)
# image(dat1,asp=1)

# net.cost = (total.change.cost+total.ongoing.cost-revenue.ongoing+expenses.ongoing-services.ongoing)*-1

# Pareto cost-benefit analysis ####

## For each amount of budget, what's the optimal level of concern ####
ggplot(costs.df, aes(x = net.cost, y = concern)) +
  geom_point(alpha = 0.2)

(min.budget <- min(plot.df$net.cost, na.rm = TRUE))
(max.budget <- max(plot.df$net.cost, na.rm = TRUE))
interval <- (max.budget-min.budget)/100
(budget.spec <- seq(min.budget,max.budget,interval))

pseudo.pareto.list <- list() 
iterations <- 1
for(i in budget.spec){
  optimal <- plot.df %>% filter(net.cost >= i) %>% 
    slice_max(concern) %>% 
    slice_max(net.cost)
  pseudo.pareto.list[[iterations]] <- optimal
  iterations <- iterations + 1
}
pseudo.pareto.df <- data.table::rbindlist(pseudo.pareto.list)

ggplot(pseudo.pareto.df, aes(x = net.cost, y = concern)) +
  #geom_point(data = costs.df, aes(x = net.cost, y = concern, colour = reserve), alpha = 0.2) +
  geom_point(colour = "red") +
  geom_line(colour = "red") +
  scale_colour_viridis_c(name = "Reserve size") +
  scale_x_continuous(name = "Cost (net profit)") +
  scale_y_continuous(name = "Biodiversity") +
  theme_bw()

## Genuine Pareto ####

# Create lookup table
conc.div <-max(plot.df$concern, na.rm = T)
#conc.div <- 1
cost.div <- max(plot.df$net.cost, na.rm = T)
#cost.div <- 1

# For every cost, there's one best level of concern
value.function <- function(mu, concern, cost){
  value <- ((1-mu)*(concern/conc.div)) + (mu*cost/cost.div)
  return(value)
}

mu.spec <- seq(0,1,0.01)
# mu.spec <- c(seq(0,0.066,0.001),
#              seq(0.066,0.067,0.00001),
#              seq(0.067,0.752,0.001),
#              seq(0.7529,0.754,0.00001),
#              seq(0.754,1,0.001))

mu.list <- list()
int.val <- 1
for(i in mu.spec){
  mu <- i
  print(mu)
  # For our entire biodiversity response and a particular weighting mu
  ## Figure out the value associated with each cost/concern combination
  (values <- value.function(mu = mu,
                 concern = plot.df$concern,
                 cost = plot.df$net.cost))
  ## Figure out where the value function is highest
  (optimal.value <- max(values, na.rm = T))
  ## If there's more then one concern/cost combination that achieves the highest value, just take the first
  (indice <- min(which(values == optimal.value),na.rm = T)) # Have just put min on here so only one optimal
  ## Figure out the concern associated with the optimal value
  (optimal.concern <- plot.df$concern[indice])
  (optimal.net.cost <- plot.df$net.cost[indice])
  (strategies <- plot.df %>% filter(concern == optimal.concern & net.cost == optimal.net.cost))
  (strategies <- slice_head(strategies))
  (strategies$mu <- mu)
  (strategies$optimal.value <- optimal.value) 
  (mu.list[[int.val]] <- strategies)
  int.val <- int.val + 1
}
pareto.df <- data.table::rbindlist(mu.list)

ggplot(pareto.df, aes(x = mu, y = optimal.value)) +
  geom_point() +
  geom_line()

t<-ggplot(pareto.df, aes(x = net.cost, y = concern, fill = mu)) +
  geom_point(data = costs.df, aes(x = net.cost, y = concern), alpha = 0.2) +
  geom_point(colour = "red") +
  #geom_line(colour = "red") +
  scale_x_continuous(name = "Cost (net profit)") +
  scale_y_continuous(name = "Biodiversity") +
  theme_bw()
t
plotly::ggplotly(t)
# Mu switches at
# 0.022 to 0.023
# 0.494 to numbers above



# What to do when more than one optimal?
# At moment let's just take one with higher concern

# For specific optima
optimal <-pseudo.pareto.df %>% slice_max(net.cost) # Change slice as appropriate

costs.plot <- ggplot() +
  geom_raster(data = plot.df, aes(x = reserve, y = sensitivity, fill = concern)) +
  geom_textcontour(data = plot.df, aes(x = reserve, y = sensitivity, z = net.cost, colour = "Net profit")) +
  scale_colour_manual(name = "Costs", values = c("Net profit" = "white")) +
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

optimal.plot <- optimal %>% pivot_longer(cols = c("demand", "reserve", "sensitivity"), names_to = "management", values_to = "level")

ggplot(optimal.plot, aes(y = level/nrow(optimal), fill = management, x = management)) +
  geom_col() +
  scale_fill_viridis_d(name = "Management", labels = c("Demand", "Reserve size", "Sensitivity")) +
  scale_x_discrete(name = "Management", labels = c("Demand", "Reserve size", "Sensitivity")) +
  scale_y_continuous(name = "Optimal level") +
  theme_bw()

demand <- 0.68
reserve <- 0.5
sensitivity <- 0.5
management <- c("demand", "reserve", "sensitivity")
level <- c(demand, reserve, sensitivity)
scenario <- 1
before.df <- cbind.data.frame(management, level, scenario)
optimal.plot$scenario <- 2
both.df <- bind_rows(before.df, optimal.plot)

both.df <- both.df %>% dplyr::select(management, level, scenario)

ggplot(both.df, aes(y = level/nrow(optimal), fill = management, x = management)) +
  facet_grid(.~scenario) +
  geom_col() +
  scale_fill_viridis_d(name = "Management", labels = c("Demand", "Reserve size", "Sensitivity")) +
  scale_x_discrete(name = "Management", labels = c("Demand", "Reserve size", "Sensitivity")) +
  scale_y_continuous(name = "Optimal level") +
  theme_bw()

# Animation
animation.plot <- ggplot(both.df, aes(y = level/nrow(optimal), fill = management, x = management, z = scenario)) +
  geom_col() +
  scale_fill_viridis_d(name = "Management", labels = c("Demand", "Reserve size", "Sensitivity")) +
  scale_x_discrete(name = "Management", labels = c("Demand", "Reserve size", "Sensitivity")) +
  scale_y_continuous(name = "Optimal level") +
  theme_bw()
animation.plot

animation <- animation.plot +
  transition_time(scenario) +
  labs(title = "Scenario: {frame_time}") +
  ease_aes(default = "linear")
fps <- 25
duration <- 7
#animate(animation, fps = fps, duration = duration, start_pause = fps*1, end_pause = fps*2, height = 5, width = 5, res = 225, units = "in")
