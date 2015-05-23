## efficient randomization within blocks

library(pbapply)

block_sample <-
function(groups)
{
    groups <- as.integer(droplevels(as.factor(groups)))
    nl <- length(unique(groups))
    out <- numeric(length(groups))
    pb <- startpb(0, nl)
    on.exit(close(pb))
    for (k in seq_len(nl)) {
        l <- sum(groups == k)
        out[groups == k] <- sample.int(l)
        setpb(pb, k)
    }
    out
}
if (FALSE) {}
groups <- rep(1:4, each=3)
block_sample(groups)
table(groups, block_sample(groups))

groups <- rep(1:(10^3), each=100)
system.time(x <- block_sample(groups))
table(table(groups[x <= 10]))

}
