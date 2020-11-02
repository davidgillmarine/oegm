##Randomly select citations

library(dplyr)
library(tidyr)
library(RefManageR)
library(rcrossref)
library(revtools)

##Read in file
data <- ReadBib("10_OEGM.bib", check = FALSE)

##Convert to dataframe
df <- as.data.frame(data)
df$id <- rownames(df)

##Randomly select some - change number for different random amount
rand <- df[sample(nrow(df), 200), ]

##Remove random rows from rest of citations
remaining <- anti_join(df,rand, by="id")

##Convert format and write to an .RIS file
rand <- select(rand,-id)
remaining <- select(remaining,-id)

randombib <- as.bibliography(rand)
remainingbib <- as.bibliography(remaining)

write_bibliography(randombib, filename="random.ris",format="ris")
write_bibliography(remainingbib, filename="remaining.bib",format="bib") ##make sure you write the remaining to a bibtex file as it is faster for R to read this in than an RIS don't ask me why, for other purposes you can also write to .ris like importing to endnote, just change the file extension and the format type in this command

##Import into colandr directly OR into citation manager etc...
