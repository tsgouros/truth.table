
## Imagine an input truth table:
##
## in1  in2  in3  out  prb
## ---  ---  ---  ---  ---
##  0    0    0    0   1.0
##  0    0    1    0   1.0
##  0    1    0    1   0.5
##  0    1    0    0   0.5
##

## We create a 'truth.table' class to hold this kind of thing.  It has a
## data frame filled with input values, and a list of output values that can
## have a probability attached to them, so that one input could produce
## multiple outputs, depending on circumstances.

## To begin with, there is an 'outcome' class that associates certain
## outcomes with probabilities.

## Returns an 'outcome' class object.
outcome <- function(out, prob) {
    if (sum(prob) != 1) {
        cat("Probabilities must sum to one.\n");
        stop();
    }
    return(structure(list(out=out, prob=prob), class="outcome"));
}

## A test object.
samp.out <- outcome(out=c(0,1),prob=c(0.5,0.5));
samp2.out <- outcome(out=c(1),prob=c(1));

## A print method for the class.
format.outcome <- function(p) {
    outstring <- ''
    for (i in 1:length(p$out)) {
        outstring <- paste(outstring, " ",
                           p$out[i], " (", p$prob[i], ") ", sep="");
    }
    return(outstring);
}

print.outcome <- function(p) {
    if (class(p) != "outcome") { stop(); }
    cat(format.outcome(p), "\n");
}

## Now we need a 'truth.table' class that consists of a bunch of
## inputs, and an 'outcome' object for each set of inputs.
truth.table <- function(inputs, outcomes) {
    return(structure(list(inputs=inputs, outcomes=outcomes), class="truth.table"));
}

## Here's an AND gate
AND.tt <- truth.table(inputs=data.frame(in1=c(0,0,1,1),in2=c(0,1,0,1)),
                       outcomes=list(outcome(out=c(0),prob=c(1)),
                                     outcome(out=c(0),prob=c(1)),
                                     outcome(out=c(0),prob=c(1)),
                                     outcome(out=c(1),prob=c(1))));

## Here's a sucky AND gate, that has trouble getting the right answer
## for a couple of combinations.
AND2.tt <- truth.table(inputs=data.frame(in1=c(0,0,1,1),in2=c(0,1,0,1)),
                        outcomes=list(outcome(out=c(0),prob=c(1)),
                                      outcome(out=c(0),prob=c(1)),
                                      outcome(out=c(0,1),prob=c(0.9,0.1)),
                                      outcome(out=c(0,1),prob=c(0.5,0.5))));

## This is sort of like Shuki Bruck's pswitch.  The output is a random choice
## among the inputs.
PSWITCH.tt <- truth.table(inputs=data.frame(in1=c(0,0,1,1),in2=c(0,1,0,1)),
                        outcomes=list(outcome(out=c(0),prob=c(1)),
                                      outcome(out=c(0,1),prob=c(0.5,0.5)),
                                      outcome(out=c(0,1),prob=c(0.5,0.5)),
                                      outcome(out=c(1),prob=c(1))));



print.truth.table <- function(tt) {
    if (class(tt) != "truth.table") { stop(); }

    for (j in 1:dim(tt$inputs)[2]) {
        cat(j, " ");
    }
    cat(" out\n");

    for (j in 1:dim(tt$inputs)[2]) {
        cat("-- ");
    }
    cat(" ---\n");

    for (i in 1:dim(tt$inputs)[1]) {
        for (j in 1:dim(tt$inputs)[2]) {
            cat(tt$inputs[i, j], " ");
        }
        cat(format.outcome(tt$outcomes[[i]]), "\n");
    }
}

## Given a truth table, and the probability distributions of its
## inputs, generate an output table with probabilities for each row.
calc.prob.table <- function(tt, input.probs) {

    ## Make a place to keep track of an output probability table.
    output.probs <- list();

    ## Calculate them.
    for (row in 1:dim(tt$inputs)[1]) {
        ## We assemble a string to identify the combination we're using.
        result.str <- "";
        ## And the resulting probability.
        result <- 1;

        for (col in 1:dim(tt$inputs)[2]) {
            result.str <- paste(result.str,
                                as.character(tt$inputs[row,col]), sep="");
            result <-
                result * as.numeric(input.probs[as.character(tt$inputs[row,col])]);
        }

        ## At this point we have a row of what will be a table of
        ## input probabilities.  Now we add the output, which might
        ## split it into multiple rows.
        for (k in 1:length(tt$outcomes[[row]]$out)) {
            output.probs[paste(result.str,
                               as.character(tt$outcomes[[row]]$out[k]),
                               sep="")] <- result * tt$outcomes[[row]]$prob[k];
        }
    }
    return(output.probs);
}

## A recursive function to generate all possible combinations of some
## symbols.  The COLS argument is a list of indices we're paying
## attention to.  The negative positions in that list will get a '.'.  So
## permute(1, c(-1,2,3)) ->
##      c(".000" ".100" ".010" ".110" ".001" ".101" ".011" ".111")
## Note that this always includes an extra column for the output, the last
## column on the right.
permute.recurse <- function(level, cols) {

    suffixes <- c("0", "1");

    output <- c();

    if (length(cols) == 0) {
        output <- suffixes;
    } else {
        temp <- permute.recurse(level + 1, cols[-1]);

        for (string in temp) {

            if (level == cols[1]) {

                for (suffix in suffixes) {
                    output <- c(output, paste(suffix, string, sep=""));

                }
            } else {
                    output <- c(output, paste(".", string, sep=""));
            }
        }
    }

    return(output);
}

permute <- function(cols) {
    return(permute.recurse(1, cols));
}

## With definitions of truth table in place, now we can create functions to
## calculate various definitions of mutual information.  TT is the input
## truth.table object, INPUT.PROBS is a list containing the input probablity
## distributions, and COLS refers to the columns we're looking at.  For now
## we're using the same input.probs for all the inputs, and the cols are
## specified with an index number.  Use zero to reference the output and
## negative numbers to ignore columns.  So cols=c(1,-2) will give the mutual
## information between the first input and the output, and cols=c(1,2) will
## give the three-way mutual information between two inputs and the output.
calc.probs <- function(tt, input.probs, cols) {

    ## Do some checking.
    if (length(cols) != dim(tt$inputs)[2]) {
        cat("Wrong number of column indicators for this truth table.\n");
        stop();
    }

    ## Get the probability table for this truth table, assuming the
    ## given input probabilities.
    cpt <- calc.prob.table(tt, input.probs);

    ## Now generate regular expressions to pull the relevant
    ## combinations out of the probability table.
    permutations <- permute(cols);

    out <- list();

    ## Generate all the possible values of the symbols identified by cols.
    for (p in permutations) {

        found <- grep(p, names(cpt));

        s <- 0;
        for (f in found) {
            s <- s + cpt[[names(cpt)[f]]];
        }

        out[[p]] <- s;
    }

    return(out);
}

mutual <- function(tt, input.probs, cols) {

    probs <- calc.probs(tt, input.probs, cols);

    ## Pick up the probability of the output symbols.  Multiplying the cols
    ## vector by zero ensures none of the result will match the level.
    output.prob <- calc.probs(tt, input.probs, cols*0)

    ## Change the names of output.prob to make them easier to use.
    for (i in 1:length(output.prob)) {
        nchars <- nchar(names(output.prob)[i]);
        names(output.prob)[i] <- substr(names(output.prob)[i], nchars, nchars);
    }

    ## We are using the KL divergence to stand for mutual information.  So we
    ## need the product of the input probabilities corresponding to the
    ## elements in the string for each probability.
    out <- 0;
    for (i in 1:length(probs)) {
        joint.prob <- probs[[ names(probs)[i] ]];

        ## Parse names(probs)[i] to find the probabilities of the components
        ## of the joint probability.
        chars <- strsplit(names(probs)[i], c())[[ 1 ]];

        prod.prob <- 1;
        for (j in 1:length(cols)) {
            if (cols[j] > 0) {
                prod.prob <- prod.prob * input.probs[[ chars[j] ]];
            }
        }

        ## Also get the output character, which is the next one.
        prod.prob <- prod.prob * output.prob[[ chars[length(cols)+1] ]];

        if (probs[[i]] != 0) {
            out <- out + probs[[i]] * log(probs[[i]] / prod.prob, base=2);
        }
    }
    return(out);
}



## The simple information content of an array of probabilities.
H <- function(ps) {
    sum <- 0;
    for (p in ps) {
        pnum <- as.numeric(p);
        if (pnum != 0) {
            sum <- sum - pnum * log(pnum, base=2);
        }
    }
    return(sum);
}

KL.s <- function(pq, ...) {
    if (pq == 0) {
        return(0);
    } else {
        prod <- 1;
        for (i in list(...)) {
            prod <- prod * i;
        }
        return(pq * log(pq/(prod),base=2));
    }
}

KL <- function(pq, p, q) {
    sum <- 0;
    for (i in 1:length(pq)) {
        sum <- sum + KL.s(pq[i], p[i], q[i]);
    }
    return(sum);
}

KL3 <- function(pq, p, q, r) {
    sum <- 0;
    for (i in 1:length(pq)) {
        sum <- sum + KL.s(pq[i], p[i], q[i], r[i]);
    }
    return(sum);
}

KL1 <- function(pq, p) {
    sum <- 0;
    for (i in 1:length(pq)) {
        sum <- sum + KL.s(pq[i], p[i]);
    }
    return(sum);
}







