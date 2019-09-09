library(tidyverse)
library(stringr)

string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string use single quotes'

#escaping quotes
double_qoute <- "\""
single_quote <- '\''

#escaping backslash
x <- c("\"", "\\")
writeLines(x)
x
writeLines(double_qoute)

#nonenglish
x <- "\u00b5"
x

#character vector
c("one", "two", "three")

#length
str_length(c("a", "R for data science", NA))

#combine
str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep=", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY"
)

#collapsing
str_c(c("x", "y", "z"), collapse = ", ")


#substr
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

##locales
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")
str_sort(x, locale = "haw")

#exs p 199
#1
?paste

paste("g", NA)
paste0("g", NA)
str_c("g", NA)

#2
str_c('goulet', c("a", "b", "c"), sep = ":")
str_c('goulet', c("a", "b", "c"), collapse = ":")

#3
x <- "flipping heck you melon farmer"
str_sub(x, round(str_length(x)/2), round((str_length(x)/2)))
#4
?str_wrap
y <- "This is a really long line that is filled sith irrelevant nonsense that I dont actually want to read ever again.  Why am I wasting my time?  Well, I can only say it is to make a useful example for this question."
cat(str_wrap(y))
#5
?str_trim
?str_pad
#6
combo <- function(vec){
  len_vec <- length(vec)
  min_one <- len_vec - 1

  if (len_vec >= 3) {
    last <- vec[len_vec]
    str_c(str_c(vec[0:min_one], collapse=', '), last, sep = ', and ')
  } else if (len_vec >= 2){
    str_c(vec, collapse=', and ')
  } else {
    vec
  }
}

mine <- c("a", "b", "c")
combo(mine)
mine2 <- c("a", "b", "c", "d")
combo(mine2)
mine3 <- c("a", "b")
combo(mine3)

##REG EX p 200
x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x, ".a.")
dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")
x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

#ex p201
#1
x <- '\\'
str_view(x, "\\\\")
#2
y <- "\"\'\\?"
writeLines(y)
str_view(y, "\"\'\\\\\\?")

#anchors
x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

#p203 exercises
#1
x <- "$^$"
writeLines(x)
str_view(x, "\\$\\^\\$")
#2
#a
str_view(stringr::words, "^y", match = TRUE)
#b
str_view(stringr::words, "x$", match = TRUE)
#c
str_view(stringr::words, "^...$", match = TRUE)
#d
str_view(stringr::words, ".......", match = TRUE)


str_view(c("grey", "gray"), "gr(e|a)y")

#exs p204
#1a
str_view(stringr::words, "^[aeiou]", match = TRUE)
#1b
str_view(stringr::words, "^[^aeiou]+$", match = TRUE)
#1c
str_view(stringr::words, "[^e]ed$", match = TRUE)
str_view(stringr::words, "ed$", match = TRUE)
#1d
str_view(stringr::words, "(ing|ize)$", match = TRUE)
#2
str_view(stringr::words, "([^c]ei|[c]ie)", match = TRUE)
#3
str_view(stringr::words, "q[^u]", match = TRUE)
#4
str_view(stringr::words, "x$", match = TRUE)
#5
str_view("0208 555 8738", "\\d\\d\\d\\d\\s\\d\\d\\d\\s\\d\\d\\d\\d")

#repetition
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')
str_view(x, 'C{2}')
str_view(x, 'C{2,}')
str_view(x, 'C{2,3}')
str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

#p 206 exs
#2
#a
str_view("wrapped up like a {doosh}", "^.*$")
str_view("wrapped up like a {doosh}", "\\{.+\\}")
str_view("2015-08-23", "\\d{4}-\\d{2}-\\d{2}")
str_view("\\\\\\\\", "\\\\{4}")

#groups and backreferences
str_view(fruit, "(..)\\1", match = TRUE)

#ex p 201
#1
str_view("jjj", "(.)\\1\\1")
str_view("abba", "(.)(.)\\2\\1")
str_view("1212", "(..)\\1")
str_view("12131", "(.).\\1.\\1")
str_view("abc thats the way I like it cba", "(.)(.)(.).*\\3\\2\\1")
#2
str_view("a and I will admit that is awesome doncha", "^(.).*\\1$")
str_view("church", "(.+).*\\1")
str_view("eleven", "(.).*\\1.*\\1")

#detect match
x <- c("apple", "banana", "pear")
str_detect(x, "e")
#number of words starting with t
sum(str_detect(words, "^t"))
#proportion of words ending with a vowel
mean(str_detect(words, "[aeiou]$"))

#comparing logical operators
#look for all words that have at least one vowel and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
#find all words consisting of consonants
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")

identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]
str_subset(words, "x$")

#in tibble
df <- tibble(
  word = words,
  t = seq_along(word)
)

df %>% 
  filter(str_detect(words, "x$"))

#str_count
x
str_count(x, "a")
#average instance
mean(str_count(words, "[aeiou]"))

df %>% mutate(
  vowels = str_count(word, "[aeiou]"),
  consonants = str_count(word, "[^aeiou]")
)

#no overlap on count
str_count("abababa", "aba")
str_view_all("abababa", "aba")

#exs p211
#a
a <- str_detect(words, "^x")
b <- str_detect(words, "x$")
a | b
c <- str_detect(words, "(^x|x$)")
identical((a|b), c)
#b
d <- str_detect(words, "^[aeiou]")
e <- str_detect(words, "[^aeiou]$")
f <- str_detect(words, "^[aeiou].*[^aeiou]$")
identical(d & e, f)
#c
g <- str_detect(words, "a")
h <- str_detect(words, "e")
i <- str_detect(words, "i")
j <- str_detect(words, "o")
k <- str_detect(words, "u")
words[g&h&i&j&k]
###hard to think of a way to solve it otherwise
#d
df <- tibble(
  word = words,
) %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    letters = str_count(word, "."),
    prop_vowels = vowels/letters
  )
)

df %>% arrange(desc(vowels))
df %>% arrange(desc(prop_vowels))

#using stringr sentendces
length(sentences)
head(sentences)

#creating the search expression
colours <- 
  c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)

####more than one match
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

str_extract(more, colour_match)
str_extract_all(more, colour_match)
str_extract_all(more, colour_match, simplify=TRUE)

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify=TRUE)

#exs p213
#1
colours <- 
  c("\\bred", "orange", "yellow", "green", "blue", "purple\\b")
colour_match <- str_c(colours, collapse = "\\b|\\b")
more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

#2
str_extract(sentences, "^.+?\\s")
head(sentences)
has_ing <- str_subset(sentences, "[a-z']+?ing\\b")
str_extract_all(has_ing, "[a-z']+?ing\\b", simplify=TRUE)

#3 all plurals (going to assume ends with s)
has_pl <- str_subset(sentences, "\\b[a-z']+?s\\b")
str_extract(has_pl, "\\b[a-z']+?s\\b")
has_pl


#p214 group extracts
#extracting 'nouns'
noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% 
  str_extract(noun)

has_noun %>% 
  str_match(noun)

#doing it the tidyr way
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

#exs p215
#1
numb <- "(one|two|three|four|five|six|seven|eight|nine|ten) ([^ ]+)"
sentences %>% 
  str_subset(numb) %>% 
  str_match(numb)

#2 
contract <- "([^ ]+)'([^ ]+)"
sentences %>% 
  str_subset(contract) %>% 
  str_match(contract)

#replace
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1"="one", "2"="two", "3"='three'))

#backrefs in the replacement string
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(10)

sentences %>% head(10)

#exs p216
#1
str_replace_all("/home/doosh/moosh/geronimo/play.html",
            "/", "\\\\") 
#2
str_to_lower("WHAT YA DOIN")
up_to_down <- 
  c("A"="a", "B"="b", "C" = "c", "D" = "d", "E" = "e",
    "F"="f", "G"="g", "H" = "h", "I" = "i", "J" = "j",
    "K"="k", "L"="l", "M" = "m", "N" = "n", "O" = "o",
    "P"="p", "Q"="q", "R" = "r", "S" = "s", "T" = "t",
    "U"="u", "V"="v", "W" = "w", "X" = "x", "Y" = "y",
    "Z"="z")
str_replace_all("WHAT YA DOIN", up_to_down)

#3
str_replace(words, "([^ ])(.*)([^ ])", "\\3\\2\\1")

#splitting
sentences %>% 
  head(5) %>% 
  str_split(" ")

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]

sentences %>% 
  head(5) %>% 
  str_split(" ", simplify=TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify=TRUE)

#word boundary
x <- "This is a sentence.  This is another sentence"
str_view_all(x, boundary("word"))

str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]

#exs p 217
#1
fruity <- c("apples, pears, and bananas")
str_split(fruity, boundary("word"))[[1]]
#3
fruity %>% split("")

####other types of pattern p218
str_view(fruit, "nana")
str_view(fruit, regex("nana"))

#ignore case
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

#multiline
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

#comments
phone <- regex("
               \\(?       # optional opening parens
               (\\d{3})   # area code
               [)- ]?     # optional closing parens, dash, or space
               (\\d{3})   # another three numbers
               [ -]?      # optional space or dash
               (\\d{4})   # four more numbers
               ", comments=TRUE)
str_match("514-919-8141", phone)

####fixed
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
a1 == a2
str_detect(a1, fixed(a2))
str_detect(a1, coll(a2))

###coll
i <- c("I", "İ", "i", "ı")
i

str_subset(i, coll("i", ignore_case = TRUE))
str_subset(i, coll("i", ignore_case = TRUE,
           locale = 'tr'))
stringi::stri_locale_info()

#boundary
x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))


#exs p221
#1
slash <- c("yes there is a slash \\ here", "and a slash \\ here", "not here")
#
str_subset(slash, "\\\\")

str_subset(slash, fixed("\\"))

#2 
sentences %>% 
  str_extract_all(boundary("word"), simplify=TRUE) %>% 
  as_tibble() %>% 
  gather(V1:V12, key='key', value='value') %>% 
  filter(value != "") %>%
  mutate(value = str_to_lower(value)) %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(5)

#other functions using regex
apropos("replace")

dir(pattern = "\\.R$")

## p 222 exs
#1
stringi::stri_count_regex(sentences, " ")

?stringi::stri_duplicated
duppi <- c("goulet", "poulet", "goulet", "a", "the", "a")
library(stringi)
stri_duplicated(duppi)
stri_rand_strings(5, 20)

#2
?stri_sort
stri_sort(, locale="tk")