# class methods
bb.default <- function(text) {
  # replace ((consonants) (1-2 vowels) (consonants)) with
  # ((consonants) (vowels) b (same vowels again) (consonants)):
  new_bb(text)
  
}

bb.list <- function(text) {
  
  structure(
    rapply(text, new_bb, how = "replace"),
    class = c("bb", class(text))
  )
}


bb.factor <- function(text) {
  
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb_text <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  # save levels
  tmp_lev <- levels(text)
  bb_levels <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = tmp_lev)
  tmp_order <- is.ordered(text)
  
  bb_fct <- factor(bb_text, levels = bb_levels,
        ordered = tmp_order)
  
  structure(bb_fct,
    class = c("bb", class(bb_fct))
  )
  
}

# unfortunately gsub confuses order of arrays dims
bb.array <- function(text) {
  # use default on arrays vector
  tmp <- new_bb(as.vector(text))
  structure(
    array(tmp, dim = dim(text)),
    class = c("bb", class(text))
  )
}

#### Konstruktor ####

new_bb <- function(text) {
  
  UseMethod("bb")
  
}


#### Validiate ####

validate_bb <- function(x) {
  
  # Get all atomic types in list
  if (is.list(x)) type <- unique(rapply(x, typeof))
  else            type <- unique(typeof(x))
  
  # character for regular strings, 
  checkmate::assert_choice(type, choices = c("character",  "integer"))
  if (any(type == "integer")) checkmate::assert_factor(x, any.missing = FALSE)
  
  x

}


### Interface
bb <- function(text, ...) {
  UseMethod("bb")
}

new_bb <- function(text) {
  
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  bb_text <- gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
  
  structure(
    bb_text,
     class = c("bb", class(text))
  )
  
  
}

