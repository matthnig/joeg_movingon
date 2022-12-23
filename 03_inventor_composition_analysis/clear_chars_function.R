#########################################
# function to clean special characters  #
# from a sequence of names. The result  # 
# is a character vector of names.       #
#########################################

clear_chars <- function(names, special_chars, repl_vec){
        
        # encode all special chars to a regex pattern
        sc <- paste0(special_chars, collapse = "|")
        
        # find those names in the name list with one of these special chars
        repl_names <- grepl(sc, x = names)
        repl_names <- which(repl_names == TRUE)
        
        # replace special characters in these names
        tmp <- unlist(lapply(names[repl_names],
                             function(x) stri_replace_all_fixed(str = x,
                                                                pattern = special_chars,
                                                                replacement = repl_vec,
                                                                vectorize_all = FALSE)))
        
        # replace the uncleaned names with the cleaned ones
        names[repl_names] <- tmp
        return(names)
}
