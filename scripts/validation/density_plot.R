

km_cals <- read_csv("./data/km_cals.csv")

km_cals
hist(km_cals$users_needed, breaks = 15)


mean<- mean(km_cals$users_needed)
sd <- sd(km_cals$users_needed)
sd_low <- mean(km_cals$users_needed) - sd(km_cals$users_needed)
sd_hi <- mean(km_cals$users_needed) + sd(km_cals$users_needed)
meanx <- mean(km_cals$users_needed)
x.dens <- density(km_cals$users_needed)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)

cals_plot <- ggplot(data = km_cals, aes(users_needed)) +
  geom_density(size = 4) +
  xlim(0,20) +
  theme_bw()
cals_plot


cals_plot + geom_area(data = subset(df.dens, x >= sd_low & x <= sd_hi), 
              aes(x=x,y=y), fill = 'grey', alpha = 0.5) +
  geom_vline(xintercept = meanx, lty = 2, color = "red", lwd = 2)


#now playing with order here - this is the final graph

ggplot(data = km_cals, aes(users_needed)) +
  geom_area(data = subset(df.dens, x >= sd_low & x <= sd_hi), 
            aes(x=x,y=y), fill = 'grey', alpha = 0.65) +
  geom_density(size = 3) +
  xlim(0,20) +
  geom_vline(xintercept = mean, lty = 2, color = "red", lwd = 2) +
  theme_bw() +
  labs(x = "Number of Users Selecting a Pixel") +
  labs(y = "Density") +
  theme(text = element_text(size=30)) 
  





n = 2^10
df = data.frame(x = c(density(foo,n=n)$x, density(bar,n=n)$x),
                y = c(density(foo,n=n)$y, density(bar,n=n)$y),
                group=rep(c("foo","bar"), each=n))

## Mean and SD

y <- km_cals$users_needed

y[y < 6 | y > 8]


y[ y < (mean(km_cals$users_needed)) - (sd(km_cals$users_needed)) | y > (mean(km_cals$users_needed)) + (sd(km_cals$users_needed))] <- NA



funcShaded <- function(x) {
  x <- seq(0, 100, by = 1)
  x[ x < (mean(km_cals$users_needed)) - (sd(km_cals$users_needed)) | x > (mean(km_cals$users_needed)) + (sd(km_cals$users_needed))] <- NA
  return(y)
}


cals_plot +
  stat_function(fun=funcShaded, geom="area", fill="red", alpha=0.2)




df = data.frame(x = c(density(foo,n=n)$x, density(bar,n=n)$x),
                y = c(density(foo,n=n)$y, density(bar,n=n)$y),
                group=rep(c("foo","bar"), each=n))

## Mean and SD
p <- ggplot(data = d) + theme_bw() + 
  geom_density(aes(x=x, y = ..density..), color = 'black')
# new code is below
q5 <- quantile(x,.05)
q95 <- quantile(x,.95)
medx <- median(x)
x.dens <- density(x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
p + geom_area(data = subset(df.dens, x >= q5 & x <= q95), 
              aes(x=x,y=y), fill = 'blue') +
  geom_vline(xintercept = medx)
