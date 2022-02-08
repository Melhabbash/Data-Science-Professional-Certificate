
s <- "Hello!"    # double quotes define a string
s
s <- 'Hello!'    # single quotes define a string
s
s <- `Hello`    # backquotes do not

# s <- "10""    # error - unclosed quotes

s <- '10"'    # correct
s
# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape with \
s <- '5'10"'    # error
s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)