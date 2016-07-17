# Liam Byrne
# R Bridge - Week 1 - Homework 1
# Problem 1

# Write a loop that calculates 12-factorial.

nFact <- function(n = 1) # No argument or any value < 2 will return 1, else will return n factorial
{
    nClone <- n
    fact <- 1

    while (nClone > 1)
    {
        fact <- nClone*fact
        nClone <- nClone-1
    }

    return (fact)
}

factVar <- 12

sprintf("%d! = %d", factVar, nFact(factVar))


#Problem 2

# Show how to create a numeric vector that contains the sequence from 20 to 50 by 5.

aSeq <- seq (from = 20,to = 50,by = 5)

cat(aSeq,"", sep = "\n")


#Problem 3

# Show how to take a trio of input numbers a, b, and c and implement the quadratic equation/

quadSolve <- function(a = 1, b = 0, c = 0) # Forces a=1, if Null, to avoid Inf
{

    # For two real roots
    if (b*b - 4*a*c > 0)
    {
        x1 <- (-b + sqrt(b*b - 4*a*c))/(2*a)
        x2 <- (-b - sqrt(b*b - 4*a*c))/(2*a)
        sol <- c(x1,x2)
    }

    # For one real root
    else if (b*b - 4*a*c == 0)
    {
        sol <- -b/(2*a)
    }

    # For two imaginary roots
    else
    {
        realN <- -b/(2*a)
        imagN <- sqrt(abs(b*b - 4*a*c))/(2*a)
        sol <- c(complex(real = realN, imaginary = imagN), complex(real = realN, imaginary = -imagN))
    }

    return (sol)
}

#Example Solution
quadSolve(1,-1,-1) # Two Real
quadSolve(1,6,9) # One Real
quadSolve(3,8,6) # Two Imaginary