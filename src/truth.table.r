
## Imagine an input truth table for an operation on discrete symbols:
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

## Returns an 'outcome' class object.  This is just a list of
## probabilities, keyed by the symbol values.  But the input to this
## constructor is a couple of vectors, of (one-character) strings and
## numbers that add to unity.
outcome <- function(out, prob) {
    if (sum(prob) != 1) {
        cat("Probabilities must sum to one.\n");
        stop();
    }

    if (class(out) != "character") {
        cat("'outcome' is a container for discrete symbols.\n");
        cat(" Use a string of characters for the 'out' argument.\n");
        stop();
    }

    output = list()
    for (i in 1:length(out)) {
        output[out[i]] = prob[i];
    }

    return(structure(output, class="outcome"));
}

## A test object.
samp.out <- outcome(out=c("0","1"),prob=c(0.5,0.5));
samp2.out <- outcome(out=c("1"),prob=c(1));

## A print method for the class.
format.outcome <- function(p) {
    outstring <- ''
    for (i in 1:length(p)) {
        outstring <- paste(outstring, " ",
                           names(p)[i], " (", p[[i]], ") ", sep="");
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

    ## A truth table is a list of inputs (as a data frame)...
    if (class(inputs) != "data.frame") {
        cat("inputs must be a data.frame.\n");
        stop();
    }

    ## And a list of corresponding outputs (as an outcome object for each
    ## input row.  The outcome object contains a list of output values and
    ## a probability corresponding to each.
    if (class(outcomes) != "list") {
        cat("outcomes must be a list (of outcomes).\n");
        stop();
    } else {
        if (class(outcomes[[1]]) != "outcome") {
            cat("outcomes must be a list of outcome class objects.\n");
            stop();
        }
    }

    ## Check that there is an outcome for each input row.
    if (length(outcomes) != dim(inputs)[1]) {
        cat("There must be one outcome object for each input row.\n");
        stop();
    }

    ## All checks out.  Assemble the structure and return it.
    return(structure(list(inputs=inputs,
                          outcomes=outcomes), class="truth.table"));
}

## Here's an AND gate
AND.tt <- truth.table(inputs=data.frame(in1=c("0","0","1","1"),
                                        in2=c("0","1","0","1")),
                      outcomes=list(outcome(out=c("0"),prob=c(1)),
                                    outcome(out=c("0"),prob=c(1)),
                                    outcome(out=c("0"),prob=c(1)),
                                    outcome(out=c("1"),prob=c(1))));

## Here's a sucky AND gate, that has trouble getting the right answer
## for a couple of combinations.
AND2.tt <- truth.table(inputs=data.frame(in1=c("0","0","1","1"),
                                         in2=c("0","1","0","1")),
                       outcomes=list(outcome(out=c("0"),prob=c(1)),
                                     outcome(out=c("0"),prob=c(1)),
                                     outcome(out=c("0","1"),prob=c(0.9,0.1)),
                                     outcome(out=c("0","1"),prob=c(0.5,0.5))));

## This is sort of like Shuki Bruck's pswitch.  The output is a random choice
## among the inputs.
PSWITCH.tt <- truth.table(inputs=data.frame(in1=c("0","0","1","1"),
                                            in2=c("0","1","0","1")),
                          outcomes=list(outcome(out=c("0"),prob=c(1)),
                                        outcome(out=c("0","1"),prob=c(0.5,0.5)),
                                        outcome(out=c("0","1"),prob=c(0.5,0.5)),
                                        outcome(out=c("1"),prob=c(1))));

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
            ## Note that R stores the inputs as factors, so we have to fix that.
            cat(as.character(tt$inputs[i, j]), " ");
        }
        cat(format.outcome(tt$outcomes[[i]]), "\n");
    }
}

## Given a truth table, and the probability distributions of its inputs,
## generate an output table with probabilities for each row.  The
## 'input.probs' argument is a list of 'outcome' objects, one for each of
## the input columns.
##
## Note that there must be an entry in input.probs for every possible
## input argument, even if it has probability zero.  We do not check for this.
calc.prob.table <- function(tt, input.probs) {

    ## Make a place to keep track of an output probability table.
    output.probs <- list();

    ## Calculate them.
    for (row in 1:dim(tt$inputs)[1]) {
        ## We assemble a string to identify the combination we're using.
        result.str <- "";
        ## And the resulting probability.
        result <- 1;

        ## Multiply the input probabilities together
        for (col in 1:dim(tt$inputs)[2]) {
            result.str <- paste(result.str,
                                as.character(tt$inputs[row,col]), sep="");
            result <-
                result * as.numeric(input.probs[[col]][[as.character(tt$inputs[row,col])]]);
        }

        ## At this point we have a row of what will be a table of input
        ## probabilities.  Now we add the output, which might split it
        ## into multiple rows.

        ## tt$outcomes[[row]] is the outcomes object, and its length is
        ## the number of rows we'll be adding to the output.
        for (k in 1:length(tt$outcomes[[row]])) {
            output.probs[paste(result.str,
                               names(tt$outcomes[[row]])[k],
                               sep="")] <- result * tt$outcomes[[row]][[k]];
        }
    }
    return(output.probs);
}

## A recursive function to generate all possible combinations of some set
## of symbols.  The input is a list of outcome class objects, a level, and
## a 'cols' argument to indicate which columns we are paying attentions
## to.  The negative positions in that list will get a '.'.  So
## permute(1, c(-1,2,3)) ->
##      c(".000" ".100" ".010" ".110" ".001" ".101" ".011" ".111")
##
## Don't use this directly, use the 'permute' function, below.
permute.recurse <- function(level, outcomes, cols) {

    output <- c();

    if (level >= length(cols)) {
        if (cols[level] > 0) {
            output <- names(outcomes[[level]]);
        } else {
            output <- c(".");
        }
    } else {
        temp <- permute.recurse(level + 1, outcomes, cols);

        for (string in temp) {

            if (cols[level] > 0) {

                for (name in names(outcomes[[level]])) {
                    output <- c(output, paste(name, string, sep=""));

                }
            } else {
                    output <- c(output, paste(".", string, sep=""));
            }
        }
    }

    return(output);
}

## This function accepts a bunch of input outcomes and one or more output
## outcomes, and a cols argument.  It returns a list of strings
## representing all the possible permutations of the input and output
## outcome objects, as indicated by the cols argument.
##
## The cols argument is either a list of the column numbers that are to be
## permuted, or a list of elements, one to each column, with positive
## values corresponding to elements to be permuted, and negative numbers
## to wildcards.
##
## Examples:
## > inp
## [[1]]
##  0 (0.5)  1 (0.5)
##
## [[2]]
##  0 (0.55)  1 (0.45)
##
## > permute(inp)
## [1] "00" "10" "01" "11"
## > permute(inp,cols=c(1,-1))
## [1] "0." "1."
## > permute(inp,cols=c(-1,-1))
## [1] ".."
## > permute(inp,cols=c(2))
## [1] ".0" ".1"
## > permute(inp,cols=c(1))
## [1] "0." "1."
## > permute(inp,out=outcome(c("0","1"),prob=c(0.3,0.7)),cols=c(1))
## [1] "0.." "1.."
## > permute(inp,out=outcome(c("0","1"),prob=c(0.3,0.7)),cols=c(1,3))
## [1] "0.0" "1.0" "0.1" "1.1"
## > permute(inp,out=outcome(c("0","1"),prob=c(0.3,0.7)))
## [1] "000" "100" "010" "110" "001" "101" "011" "111"
## > permute(inp,out=outcome(c("0","1"),prob=c(0.3,0.7)),cols=c(3,1))
## [1] "0.0" "1.0" "0.1" "1.1"
##
permute <- function(input.outcomes, output.outcomes=0, cols=0) {

    outcome.list <- input.outcomes;
    if (!missing(output.outcomes)) {
        if (class(output.outcomes) == "outcome") {
            outcome.list <- append(outcome.list, list(output.outcomes));
        } else if (class(output.outcomes) == "list") {
            outcome.list <- append(outcome.list, output.outcomes);
        }
    }

    ## If there is no cols argument, just return the permutation of all
    ## the outcomes.
    if (missing(cols)) {
        cols <- rep(1, length(outcome.list));
    }

    ## If cols is short, then presumably it is of the form c(2,3) which
    ## implies produce a permutation of columns 2 and 3, and use wildcards
    ## for the other columns.
    if (length(cols) != length(outcome.list)) {
        tmp <- rep(-1, length(outcome.list));

        for (k in cols) {
            tmp[k] <- 1;
        }

        cols <- tmp;
    }

    return(permute.recurse(1, outcome.list, cols));
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







