library(tidyverse)

#read in file
read_csv("heights.csv")

#reading inline text
read_csv("a, b, c
         1, 2, 3
         4, 5, 6")

#using skip
read_csv("a line of metadata
another line of metadata
a, b, c
         1, 2, 3
         4, 5, 6", skip = 2)


#skipping rows that start with a particular character
read_csv("# dont include this row
a, b, c
         1, 2, 3
         4, 5, 6", comment = '#')

#ignoring column headers
read_csv("1,2,3\n4,5,6", col_names = FALSE)

#add col names as separate vector
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

#specify value that represents missing
read_csv("a,b,c\n1,2,.", na = ".")
