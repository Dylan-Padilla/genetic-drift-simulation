###### BIO345 EVOLUTION: Genetic drift simulation ############

######### written by Dylan Padilla, August 2022 ########


alleles <- c("A", "B")

Ne <- c(85, 64, 48, 34, 22, 60, 46, 38, 24, 10)

matrix <- data.frame(gen = 1:10, pop = Ne, FreqA = NA, FreqB = NA)

for(row in 1:nrow(matrix)){
  a <- sample(alleles, size = matrix$pop[row], replace = T) # make random selection of alleles with replacement from a particular pop
  fa <- sum(a == "A") / length(a) # find the frequency of allele A
  fb <- 1 - fa # frequency of allele B
  matrix$FreqA[row] <- fa # assign it back to the data.frame
  matrix$FreqB[row] <- fb
  png(paste("img_", row, ".png"))
  plot(matrix$FreqA[row] ~ matrix$gen[row], type = "l", lwd = 3, col = "purple", ylim = c(0, 1), ylab = "Allele frequency", xlab = "Generation", xaxt = "n")
  axis(1, at = 1:10, labels = 1:10)
  legend("topleft", legend = c("A", "B"), col = c("purple", "skyblue"), lwd = 3, bty = "n")
  lines(matrix$FreqB[row] ~ matrix$gen[row], lwd = 3, col = "skyblue")
  dev.off()

}    


## list file names and read in
imgs <- list.files(path = "/Users/dylanpadilla/Dropbox (ASU)/BIO345/imgs", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "~/genetic-drift.gif")


knitr::include_graphics("genetic-drift.gif")
