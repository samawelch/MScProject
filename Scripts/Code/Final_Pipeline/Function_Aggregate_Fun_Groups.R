# Groups isolates by species and stressors by tenuous functional group.
# Takes a tibble of stuff as an input. Needs to have a categoric isolate column and 8 stressor presence/absence columns

aggregate_functional_groups <- function(input_tibble, clear_columns = FALSE)
{
  # Turn isolate into species first
  input_tibble <- input_tibble %>%
    mutate(Species = as.character(Isolate))
  # Ugly for loop w/ if statements to assign species based on isolate. I tried to do this in a nicer way, but it was not to be.
  for (q in 1:nrow(input_tibble))
  {
    if (input_tibble$Isolate[q] == "KUE4_10")
      input_tibble$Species[q] <- "S. acidaminiphila"
    if (input_tibble$Isolate[q] == "LUF4_5")
      input_tibble$Species[q] <- "L. rhizovicinus"
    if ((input_tibble$Isolate[q] == "NUE1_1") || (input_tibble$Species[q] == "KUE4_4"))
      input_tibble$Species[q] <- "B. simplex"
    if ((input_tibble$Isolate[q] == "KUB5_13") || (input_tibble$Species[q] == "NUF1_3"))
      input_tibble$Species[q] <- "V. paradoxus"
  }
  # Now convert stressor presence/absence into functional group presence/absence
  input_tibble <- input_tibble %>%
    mutate(HMetal = 0) %>%
    mutate(AB = 0) %>%
    mutate(Pesti = 0) %>%
    mutate(Fungi = 0) 
  
  for (m in 1:nrow(input_tibble))
  {
    # This is ugly but I tried quasiquotation and it didn't work.
    if (input_tibble$Copper[m] == 1)
      input_tibble$HMetal[m] <- input_tibble$HMetal[m] + 1 
    if (input_tibble$Nickel[m] == 1)
      input_tibble$HMetal[m] <- input_tibble$HMetal[m] + 1
    if (input_tibble$Chloramphenicol[m] == 1)
      input_tibble$AB[m] <- input_tibble$AB[m] + 1 
    if (input_tibble$Ampicillin[m] == 1)
      input_tibble$AB[m] <- input_tibble$AB[m] + 1 
    if (input_tibble$Atrazine[m] == 1)
      input_tibble$Pesti[m] <- input_tibble$Pesti[m] + 1 
    if (input_tibble$Metaldehyde[m] == 1)
      input_tibble$Pesti[m] <- input_tibble$Pesti[m] + 1
    if (input_tibble$Tebuconazole[m] == 1)
      input_tibble$Fungi[m] <- input_tibble$Fungi[m] + 1 
    if (input_tibble$Azoxystrobin[m] == 1)
      input_tibble$Fungi[m] <- input_tibble$Fungi[m] + 1 
  }
  
  # If clear_columns is true, delete the individual stressor and isolate rows. To keep things clean.
  if (clear_columns == TRUE)
  {
    input_tibble <- input_tibble %>%
      select(-Copper, -Nickel, -Chloramphenicol, -Ampicillin, -Atrazine, -Metaldehyde, -Tebuconazole, -Azoxystrobin, -Isolate)
  }
  
  return(input_tibble)
}
