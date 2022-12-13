# Function to generate the possible mappings of option choices to the number of
# choices where the better option was chosen. This is used to give a range of
# possible proportions of correct catch trial responses, which is necessary
# because the forgone option was omitted from the data.
generate_catch_permutations <- function() {
    # Calculate all possible permutations for 24 binary catch trials. Options are
    # mapped to the columns presented below. Choosing the better option of the 
    # pair is represented by a 1 and the worse option by 0:

    # *Columns*     *Worse option*      *Better option*
    # 1, 2          low_safe            medium_safe
    # 3, 4          low_safe            medium risky
    # 5, 6          low_safe            high_safe
    # 7, 8          low_safe            high_risky
    # 9, 10         low_risky           medium_safe
    # 11, 12        low_risky           medium_risky
    # 13, 14        low_risky           high_safe
    # 15, 16        low_risky           high_risky
    # 17, 18        medium_safe         high_safe
    # 19, 20        medium_safe         high_risky
    # 21, 22        medium_risky        high_safe
    # 23, 24        medium_risky        high_risky
    choices <- expand.grid(rep(list(0:1), 24)) %>%
        # Calculate the number of choices for each option in each row by
        # the number of columns where option was worse minus 
        # the sum of columns where option was worse plus
        # the sum of columns where the option was better
        mutate(
            n_low_safe = 8 - rowSums(.[1:8]),
            n_low_risky = 8 - rowSums(.[9:16]),
            n_medium_safe = 4 - rowSums(.[17:20]) + rowSums(.[c(1, 2, 9, 10)]),
            n_medium_risky = 4 - rowSums(.[21:24]) + rowSums(.[c(3, 4, 11, 12)]),
            n_high_safe = rowSums(.[c(5, 6, 13, 14, 17, 18, 21, 22)]),
            n_high_risky = rowSums(.[c(7, 8, 15, 16, 19, 20, 23, 24)]),
            n_better = rowSums(.)
        ) %>% 
            # Remove permutations used to calculate choices
            select(-starts_with("Var")) %>% 
            # Remove duplicates
            distinct()

}