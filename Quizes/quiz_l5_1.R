#Q2

likelihood_c <- function(y){
        choose(5,y)*0.8^y*0.2^(5-y)
}

likelihood_l <- function(y){
        choose(5,y)*0.3^y*0.7^(5-y)
}

likelihood_c(0)
likelihood_l(0)
#A: liberal

#Q4-5

b_lh_c <- function(y){
        choose(5,y)*0.8^y*0.2^(5-y)*0.5/(choose(5,y)*0.8^y*0.2^(5-y)*0.5+choose(5,y)*0.3*y+0.7^(5-y)*0.5)
}

b_lh_l <- function(y){
        choose(5,y)*0.3^y*0.7^(5-y)*0.5/(choose(5,y)*0.8^y*0.2^(5-y)*0.5+choose(5,y)*0.3^y*0.7^(5-y)*0.5)
}

b_lh_c(0) #Q4
b_lh_l(0) #Q5

#Q7

mle_fair <- function(h){
        choose(4,h)*0.5^h*0.5^(4-h)
}
mle_lh <- function(h){
        choose(4,h)*0.7^h*0.3^(4-h)
}
mle_lt <- function(h){
        choose(4,h)*0.3^h*0.7^(4-h)
}

mle_fair(2)
mle_lh(2)
mle_lt(2)

#A: fair

#Q8

b_fair <- function(h){
        choose(4,h)*0.5^h*0.5^(4-h)*0.4/(choose(4,h)*0.5^h*0.5^(4-h)*0.4+
                                        choose(4,h)*0.7^h*0.3^(4-h)*0.3+
                                        choose(4,h)*0.3^h*0.7^(4-h)*0.3)
}

b_fair(2)