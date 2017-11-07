library(CCA, GGally)

setwd("/Users/BlackHawk/Desktop/TheBigD/Mental_Health_in_Tech_survey")
file = read.csv("Mental_Health_survey.csv")


#--------------------------- Set 1: Policies on Mental Health -----------------------#
# Category 4: Organizational Policies on Mental Health
# benefits: Does your employer provide mental health benefits?
# care_options: Do you know the options for mental health care your employer provides?
# wellness_program: Has your employer ever discussed mental health as part of an employee wellness program?
# seek_help: Does your employer provide resources to learn more about mental health issues and how to seek help?
# anonymity: Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?
# leave: How easy is it for you to take medical leave for a mental health condition?

Policies_on_MH = file[,c("benefits", "care_options", "wellness_program", "seek_help", "anonymity", "leave")]
levels(Policies_on_MH[,1]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Policies_on_MH[,2]) = c(-1, 0, 1)    # -1 = "No"; 0 = "Don't know"; 1 = "Yes"
levels(Policies_on_MH[,3]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Policies_on_MH[,4]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Policies_on_MH[,5]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Policies_on_MH[,6]) = c(0, -1, 1, -2, 2)    #recode survey responses to likert scale


#------------------------- Set 2: Openness about Mental Health -----------------------#
# mental_health_consequence: Do you think that discussing a mental health issue with your employer would have negative consequences?
# phys_health_consequence: Do you think that discussing a physical health issue with your employer would have negative consequences?
# coworkers: Would you be willing to discuss a mental health issue with your coworkers?
# supervisor: Would you be willing to discuss a mental health issue with your direct supervisor(s)?
# mental_health_interview: Would you bring up a mental health issue with a potential employer in an interview?
# phys_health_interview: Would you bring up a physical health issue with a potential employer in an interview?
# mental_vs_physical: Do you feel that your employer takes mental health as seriously as physical health?
# obs_consequence: Have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?

Opennes_of_MH = file[,c("mental_health_consequence", "phys_health_consequence", "coworkers", 
                        "supervisor", "mental_health_interview", "phys_health_interview", 
                        "mental_vs_physical", "obs_consequence")]
levels(Opennes_of_MH[,1]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Opennes_of_MH[,2]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Opennes_of_MH[,3]) = c(-1, 0, 1)    #recode survey responses to likert scale
levels(Opennes_of_MH[,4]) = c(-1, 0, 1)    #recode survey responses to likert scale
levels(Opennes_of_MH[,5]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Opennes_of_MH[,6]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Opennes_of_MH[,7]) = c(0, -1, 1)    #recode survey responses to likert scale
levels(Opennes_of_MH[,8]) = c(-1, 1)    #recode survey responses to likert scale

library(varhandle)

Opennes_of_MH = unfactor(Opennes_of_MH)     #convert factors to numerics
Policies_on_MH = unfactor(Policies_on_MH)

correlation_matrix = matcor(Policies_on_MH, Opennes_of_MH)
cca = cc(Policies_on_MH, Opennes_of_MH)

#------------------------ tests of canonical dimensions -----------------------#
#------------------------ Code was copy and pasted from: ----------------------#
#---- https://stats.idre.ucla.edu/r/dae/canonical-correlation-analysis/  ------#

ev <- (1 - cca$cor^2)

n <- dim(Policies_on_MH)[1]
p <- length(Policies_on_MH)
q <- length(Opennes_of_MH)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

# initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1
    q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))




