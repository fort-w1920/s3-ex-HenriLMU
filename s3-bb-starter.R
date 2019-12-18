# class methods
bb.default <- function(text) {
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb_text <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  
  structure(bb_text,
            class = c("bb", class(text))
  )
  
}

bb.list <- function(text) {
  
  rapply(text, bb.default, how = "replace")

}


bb.factor <- function(text) {
  
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb_text <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  #save levels
  tmp_lev <- levels(text)
  bb_levels <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = levels)
  tmp_order <- is.ordered(text)
  
  bb_factor <- factor(bb_text, levels = bb.default(tmp_lev),
         ordered = tmp_order)
  
  structure(bb_factor,
    class = c("bb", text)
  )
  
}

# unfortunately gsub confuses order of arrays dims
bb.array <- function(text) {
  # use default on arrays vector
  tmp <- bb.default(as.vector(text))
  array(tmp, dim = dim(text))
}

#### Konstruktor ####

new_bb <- function(text) {
  
  UseMethod("bb")
  
}


#### Validiate ####

validate_bb <- function(x) {
  
  base_values <- unclass(x)
  
  ####TODO
  
  x

}


#### Interface ####
bb <- function(text) {
  validate_bb(
    structure(new_bb(text),
              class = c("bb", class(text))
    )
  )
}

