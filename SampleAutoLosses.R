set.seed(354)
years <- seq(1966, 2015)
counts <- rnbinom(length(years), size = 3000, mu = 20000)
allCounts <- sum(counts)
smallCounts <- floor(0.7 * allCounts)
bigCounts <- allCounts - smallCounts
numPassBig <- sample(seq_len(6), bigCounts, replace = TRUE,
                     prob = c(0.4, 0.2, 0.15, 0.1, 0.08, 0.07))
numPassSmall <- sample(seq_len(3), smallCounts, replace = TRUE,
                       prob = c(0.7, 0.25, .05))
smallLosses <- rgamma(smallCounts, shape = 4, scale = 2500)
bigLosses <- rgamma(bigCounts, shape = 0.5, scale = 1e6)
sLosses_df <- data.frame(Loss = smallLosses, Pass = numPassSmall)
bLosses_df <- data.frame(Loss = bigLosses, Pass = numPassBig)
aLosses_df <- rbind(sLosses_df, bLosses_df)
losses_df <- aLosses_df[sample(nrow(aLosses_df)), ]
row.names(losses_df) <- NULL
ALAE_P = rlnorm(allCounts, meanlog = log(0.1) - log(5) / 2,
                sdlog = sqrt(log(5)))
ALAE = losses_df$Loss * ALAE_P
writeStates <- state.abb[state.division %in%
                           c('New England', 'Middle Atlantic', 'South Atlantic')]
losses_df <- data.frame(Year = rep(years, counts), Loss = round(losses_df$Loss, 0),
                        ALAE = round(ALAE, 0), Pass = losses_df$Pass,
                        State = sample(writeStates, allCounts, replace = TRUE))
rm(years, counts, allCounts, smallCounts, bigCounts, numPassBig, numPassSmall,
   smallLosses, bigLosses, sLosses_df, bLosses_df, aLosses_df, ALAE_P, ALAE,
   writeStates)
fwrite(losses_df, "SampleAutoLosses.csv", row.names = FALSE)