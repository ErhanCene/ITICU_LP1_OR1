rm(list=ls())

source('helpers.R')

#################################################################################
########## BALANCED #############################################################
#################################################################################

supply <- c(20, 30, 25)
demand <- c(10, 25, 20, 20)

cost <- matrix(
  c( 8,  6, 10,  9,
     9, 12, 13,  7,
     14,  9, 16,  5),
  nrow = 3, byrow = TRUE
)

rownames(cost) <- paste0("S", 1:nrow(cost))
colnames(cost) <- paste0("D", 1:ncol(cost))
cost

ex1 <- balance_transport(cost=cost, supply = supply,
                  demand = demand)

ex1$cost
ex1$supply
ex1$demand
#################################################################################
########## SUPPLY > DEMAND ######################################################
#################################################################################

supply <- c(20, 30, 55)
demand <- c(10, 25, 20, 20)

cost <- matrix(
  c( 8,  6, 10,  9,
     9, 12, 13,  7,
     14,  9, 16,  5),
  nrow = 3, byrow = TRUE
)

rownames(cost) <- paste0("S", 1:nrow(cost))
colnames(cost) <- paste0("D", 1:ncol(cost))
cost


ex2 <- balance_transport(cost=cost, supply = supply,
                         demand = demand)

ex2$cost
ex2$supply
ex2$demand


#################################################################################
########## DEMAND > SUPPLY ######################################################
#################################################################################

supply <- c(20, 30, 25)
demand <- c(10, 25, 20, 40)

cost <- matrix(
  c( 8,  6, 10,  9,
     9, 12, 13,  7,
     14,  9, 16,  5),
  nrow = 3, byrow = TRUE
)

rownames(cost) <- paste0("S", 1:nrow(cost))
colnames(cost) <- paste0("D", 1:ncol(cost))
cost

ex3 <- balance_transport(cost=cost, supply = supply,
                         demand = demand)

ex3$cost
ex3$supply
ex3$demand

################################################################################
########## NW EXAMPLE ##########################################################
################################################################################

ex1_nw <- northwest_corner(cost = ex1$cost,supply = ex1$supply, demand = ex1$demand)
ex1_nw$allocation
ex1_nw$total_cost
ex1$cost

ex2_nw <- northwest_corner(cost = ex2$cost,supply = ex2$supply, demand = ex2$demand)
ex2_nw$allocation
ex2_nw$total_cost
ex2$cost

ex3_nw <- northwest_corner(cost = ex3$cost,supply = ex3$supply, demand = ex3$demand)
ex3_nw$allocation
ex3_nw$total_cost
ex3$cost



################################################################################
########## LC EXAMPLE ##########################################################
################################################################################

ex1_lc <- least_cost_method(cost = ex1$cost,supply = ex1$supply, demand = ex1$demand)
ex1_lc$allocation
ex1_lc$total_cost
ex1$cost

ex2_lc <- least_cost_method(cost = ex2$cost,supply = ex2$supply, demand = ex2$demand)
ex2_lc$allocation
ex2_lc$total_cost
ex2$cost

ex3_lc <- least_cost_method(cost = ex3$cost,supply = ex3$supply, demand = ex3$demand)
ex3_lc$allocation
ex3_lc$total_cost
ex3$cost

################################################################################
########## VAM EXAMPLE #########################################################
################################################################################

ex1_vam <- vam_method(cost = ex1$cost,supply = ex1$supply, demand = ex1$demand)
ex1_vam$allocation
ex1_vam$total_cost
ex1$cost

ex2_vam <- vam_method(cost = ex2$cost,supply = ex2$supply, demand = ex2$demand)
ex2_vam$allocation
ex2_vam$total_cost
ex2$cost

ex3_vam <- vam_method(cost = ex3$cost,supply = ex3$supply, demand = ex3$demand)
ex3_vam$allocation
ex3_vam$total_cost
ex3$cost