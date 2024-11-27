# README -----------------------------------------------------------------------
# 
#   file: monty-hall.R
#   goal: prove to myself that the monty hall problem is true
# 
# ------------------------------------------------------------------------------

# packages ---------------------------------------------------------------------

  library(ggplot)


# function to perform experiment -----------------------------------------------

  run_monty_hall = function(n_experiments, n_doors, change = FALSE) {
    
    ### repeatedly performs monty hall simulation. returns proportion correct. 
    ### - n_experiments:  number of times to run simulation. 
    ### - n_doors:        number of doors in each simulation. 
    ### - change:         whether or not the "agent" should change it's choice. 
    ### 
    
    correct = 0 
    
    for (i in 1:n_experiments) {
      
      # initialize vector representing doors
      doors = rep(0, n_doors)
      door_indices = 1:n_doors
      
      # randomly select and add the door that contains a "prize"
      correct_door = sample(door_indices, size = 1)
      doors[correct_door] = 1
      
      # simulate the agent's first choice
      agent_choice = sample(door_indices, size = 1)
      not_chosen_indices = door_indices[-which(door_indices == agent_choice)]

      # simulate host's choice of door to open
      
      host_door_indices = not_chosen_indices
      if (correct_door %in% host_door_indices) {
        host_door_indices = host_door_indices[-which(host_door_indices == correct_door)]
      }
      
      if (length(host_door_indices) == 1) {
        host_choice = host_door_indices[1]
      } else {
        host_choice = sample(host_door_indices, size = 1)
      }
      
      not_chosen_indices = not_chosen_indices[-which(not_chosen_indices == host_choice)]
      
      # "would you like to change your choice?" 
      if (change) {
        if (length(not_chosen_indices) == 1) {
          agent_choice = not_chosen_indices[1]
        } else {
          agent_choice = sample(not_chosen_indices, size = 1)
        }
      }
      
      # final reveal - was the agent correct? 
      if (agent_choice == correct_door) {
        correct = correct + 1
      }
      
    }
    
    return(correct / n_experiments)
    
  }
  
  
# perform simulation -----------------------------------------------------------
  
  prop_correct_df = data.frame()
  max_experiments = 50000
  step_size = 100
  
  for (n in seq(step_size, max_experiments, by = step_size)) {
    
    without_change = run_monty_hall(n, n_doors = 3, change = FALSE)
    with_change    = run_monty_hall(n, n_doors = 3, change = TRUE)
    
    temp_prop_correct = data.frame(
      n = n, 
      prop_correct_without_change = without_change, 
      prop_correct_with_change = with_change
    )
    
    prop_correct_df = rbind(prop_correct_df, temp_prop_correct)
    
  }
  
  
# plot results -----------------------------------------------------------------
  
  ## transform data into long format 
  
    prop_correct_long = prop_correct_df %>% 
      pivot_longer(
        cols = c("prop_correct_without_change", "prop_correct_with_change"), 
        names_to = "change_status", 
        values_to = "prop_correct"
      ) %>% 
    mutate(
      change_status = ifelse(
        change_status == "prop_correct_without_change", 
        "Agent Doesn't Switch Doors", 
        "Agent Switches Doors"
      )
    )
  
  ## calculate overall average by group 
  
    group_means = prop_correct_long %>% 
      group_by(change_status) %>% 
      summarize(mean_prop_correct = mean(prop_correct)) %>% 
      ungroup()
    
    prop_correct_long = prop_correct_long %>% 
      left_join(
        group_means, 
        by = "change_status", 
        relationship = "many-to-one"
      )
  
  ## build plot 
  
    p = ggplot(prop_correct_long) + 
      geom_line(aes(x = n, y = prop_correct, color = change_status)) + 
      facet_wrap(~change_status) + 
      geom_hline(aes(yintercept = mean_prop_correct), color = "black") + 
      xlab("Number of Experiments") + ylab("Proportion Correct") + 
      theme_bw() + 
      theme(
        axis.title.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold"), 
        legend.position = "none"
      )
    
    p
    
  ## export plot 
    
    ggsave("./monty-hall/monty-hall-comparison-3-door.png", p, width = 7, height = 3)
    
  
  