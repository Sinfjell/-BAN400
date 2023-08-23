# assignment url: https://hotneim.github.io/ban400/assignment-04.html 
# notion url: https://fjellestad.notion.site/Learning-notes-assignment-4-f1b27ffd1c654512a3a383affed55383?pvs=4

# Task 1
# PROBLEM 1

# Generate 100 random numbers between 0 and 1
N <- runif(100)


stat <- function(n) {
  mu = sum(n) / length(n)
  sigma = 3
  return(mu)
}

stat(N)