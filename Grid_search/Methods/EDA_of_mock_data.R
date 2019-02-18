dataEDA <- function(pop){
    validateSample(pop$men, pop$women)
    plot(density(pop$men), col = 1)
    lines(density(pop$women), col = 2)
    abline(v = mean(pop$men), col = "green")
    abline(v = mean(pop$women), col = "green")
    return(list(
    men_stat = summary(pop$men),
    men_var = var(pop$men),
    women_stat = summary(pop$women),
    women_var = var(pop$women),
    pop_stat = summary(c(pop$men, pop$women)),
    var_pop = var(c(pop$men, pop$women))
    ))
}  
