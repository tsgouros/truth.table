
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

## Here's an XOR gate
XOR.tt <- truth.table(inputs=data.frame(in1=c("0","0","1","1"),
                                        in2=c("0","1","0","1")),
                      outcomes=list(outcome(out=c("0"),prob=c(1)),
                                    outcome(out=c("1"),prob=c(1)),
                                    outcome(out=c("1"),prob=c(1)),
                                    outcome(out=c("0"),prob=c(1))));

## Here's an adder gate
ADD.tt <- truth.table(inputs=data.frame(in1=c("0","0","1","1"),
                                        in2=c("0","1","0","1")),
                      outcomes=list(outcome(out=c("0"),prob=c(1)),
                                    outcome(out=c("1"),prob=c(1)),
                                    outcome(out=c("2"),prob=c(1)),
                                    outcome(out=c("3"),prob=c(1))));

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

## A function to return a list of the possible output symbols generated by
## the input truth table.  The results are output as an 'outcome' object,
## but we didn't use the constructor, so the probabilities may not be valid.
output.symbols <- function(tt) {

    out <- list();

    for (outcome in tt$outcomes) {
        for (name in names(outcome)) {
            out[[name]] <- 1;
        }
    }

    class(out) <- "outcome";

    return(out);
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

## Calculate a set of probabilities, from a truth table, a list of
## probability distributions for the truth table inputs, and an indication
## of which columns are of interest.  So, if you have a truth table with
## two inputs, then the joint probability between the inputs and output
## uses cols=c(1,2,3).  If you only want the joint probability between the
## first input and the output, use cols=c(1,3).  You have to know how many
## inputs there are to get the index of the output, sorry about that, but
## this way accommodates multiple outputs, if we ever get to that.
calc.probs <- function(tt, input.probs, cols) {

    ## We need to include the output.  So get a list of the possible
    ## output symbols of the output and stick it in with the inputs.
    probs <- append(input.probs, list(output.symbols(tt)));

    ## Get the probability table for this truth table, assuming the given
    ## input probabilities.
    prob.table <- calc.prob.table(tt, input.probs);

    ## Now generate regular expressions to pull the relevant combinations
    ## out of the probability table.
    permutations <- permute(probs, cols=cols);

    ## Generate all the possible values of the symbols identified by cols.
    out <- list();
    for (p in permutations) {

        found <- grep(p, names(prob.table));

        s <- 0;
        for (f in found) {
            s <- s + prob.table[[names(prob.table)[f]]];
        }

        out[[p]] <- s;
    }

    return(out);
}

## Accepts a truth table and a bunch of input probabilities, and returns
## an outcome object with the output probabilities.
calc.output.probs <- function(tt, input.probs) {
    output.col <- length(input.probs) + 1;

    probs <- calc.probs(tt, input.probs, c(output.col));

    for (i in 1:length(probs)) {
        names(probs)[i] <- substr(names(probs)[i], output.col, output.col);
    }

    class(probs) <- "outcome";

    return(probs);
}

## Now we move to calculating the relative entropy between a columns of
## a truth table's input and its output.

mutual <- function(tt, input.probs, col, inspect=F) {

    output.probs <- calc.output.probs(tt, input.probs);

    output.col <- length(input.probs) + 1;

    joint.probs <- calc.probs(tt, input.probs, c(col, output.col));

    if (inspect) {
        cat("vals: prob\n");
        for (i in 1:length(joint.probs)) {
            cat(names(joint.probs)[i], ": (", joint.probs[[i]], ")\n", sep="");
        }
        for (i in 1:2) cat("---------");
        cat("  >> joint, product\n");
    }

    ## We are using the KL divergence (relative entropy) to calculate the
    ## mutual information.  So we need the product of the input
    ## probabilities corresponding to the elements in the string for each
    ## probability.
    out <- 0;
    for (i in 1:length(joint.probs)) {

        ## Parse names(joint.probs)[i] to find the relevant symbols for
        ## this component of the joint distribution.
        chars <- strsplit(names(joint.probs)[i], c())[[1]];
        input.char <- chars[col];
        output.char <- chars[output.col];

        prod.prob <- input.probs[[col]][[input.char]] * output.probs[[output.char]];

        if (inspect)
            cat(" ", input.char, " (", input.probs[[col]][[input.char]], ") ",
                " ", output.char, " (", output.probs[[output.char]], ") ",
                " >> ", joint.probs[[i]], " ,",  prod.prob,"\n", sep="");

        if (joint.probs[[i]] != 0) {
            out <- out + joint.probs[[i]] * log(joint.probs[[i]] / prod.prob, base=2);
        }
    }
    return(out);
}

## Gets us the total mutual information, from all the inputs and the output.
mutual.total <- function(tt, input.probs, inspect=F) {

    output.probs <- calc.output.probs(tt, input.probs);

    output.col <- length(input.probs) + 1;

    joint.probs <- calc.probs(tt, input.probs, 1:output.col);
    if (inspect) {
        cat("vals: prob\n");
        for (i in 1:length(joint.probs)) {
            cat(names(joint.probs)[i], ": (", joint.probs[[i]], ")\n", sep="");
        }
        for (i in 1:output.col) cat("---------");
        cat(" >> joint, product\n");
    }

    ## We are using the KL divergence to calculate the mutual information.
    ## So we need the product of the input probabilities corresponding to
    ## the elements in the string for each probability.
    out <- 0;
    for (i in 1:length(joint.probs)) {

        ## Parse names(joint.probs)[i] to find the relevant symbols for
        ## this component of the joint distribution.
        chars <- strsplit(names(joint.probs)[i], c())[[1]];
        prod.prob <- 1;

        for (col in 1:output.col) {

            char <- chars[col];

            if (col != output.col) {
                prod.prob <- prod.prob * input.probs[[col]][[char]];
                if (inspect) cat(" ", char, " (", input.probs[[col]][[char]], ") ",
                                 sep="");
            } else {
                prod.prob <- prod.prob * output.probs[[char]];
                if (inspect) cat(" ", char, " (", output.probs[[char]], ") ",
                                 sep="");
            }
        }
        if (inspect) cat(" >> ",joint.probs[[i]]," ,",  prod.prob,"\n", sep="");
        if (joint.probs[[i]] != 0) {
            out <- out + joint.probs[[i]] * log(joint.probs[[i]] / prod.prob, base=2);
        }
    }
    return(out);
}

try <- function(tt, input.probs) {

    HX1 <- H(input.probs[[1]]);
    HX2 <- H(input.probs[[2]]);
    cat(" Input 1 information H(X1): ", HX1, "\n");
    cat(" Input 2 information H(X2): ", HX2, "\n");
    HY <- H(calc.output.probs(tt, input.probs));
    cat(" Output information  H(Y):  ", HY, "\n");
    cat(" Mutual 1/O  I(X1;Y):       ", mutual(tt, input.probs, 1), "\n");
    cat(" Mutual 2/O  I(X2;Y):       ", mutual(tt, input.probs, 2), "\n");

    sum.mutual <- mutual(tt, input.probs, 1) + mutual(tt, input.probs, 2);
    cat(" Sum mutual I(X1;Y)+I(X2;Y):", sum.mutual, "\n");

    cat(" Sum mutual diff H(Y)-sum:  ",
        H(calc.output.probs(tt, input.probs)) - sum.mutual, "\n");
    cat(" Total Mutual I(X1;X2;Y):   ", mutual.total(tt, input.probs), "\n");
    cat(" Ratio 1 Isum/H(Y):         ", sum.mutual / HY, "\n");
    cat(" E(H(X1))                   ",
        ExH(input.probs[[1]], input.probs[[2]]), "\n");
    cat(" E(H(X2))                   ",
        ExH(input.probs[[2]], input.probs[[1]]), "\n");
    cat(" E(mutual X1;Y)             ",
        ExH(calc.output.probs(tt, input.probs), input.probs[[1]]), "\n");
    cat(" E(mutual X2;Y)             ",
        ExH(calc.output.probs(tt, input.probs), input.probs[[2]]), "\n");
    cat(" Sum H(X1) + I(X2;Y):        ",  HX1 + mutual(tt, input.probs, 1), "\n");
    cat(" Sum H(X2) + I(X1;Y):        ",  HX2 + mutual(tt, input.probs, 2), "\n");
    ## cat(" H(Y)-E(H(X1)):             ",
    ##     HY - ExH(input.probs[[1]],input.probs[[2]]), "\n");
    ## cat(" H(Y)-E(H(X2)):             ",
    ##     HY - ExH(input.probs[[2]],input.probs[[1]]), "\n");
    ## cat(" H(Y)-E(H(X1)) - I(X2;Y):   ",
    ##     HY - HX1 - mutual(tt, input.probs, 2), "\n");
    ## cat(" H(Y)-E(H(X2)) - I(X1;Y):   ",
    ##     HY - HX2 - mutual(tt, input.probs, 1), "\n");
##        HY - ExH(input.probs[[2]],input.probs[[1]]) - mutual(tt, input.probs, 1), "\n");
    cat(" H(Y)-E(H(X1))-E(H(X2)):    ",
        H(calc.output.probs(tt, input.probs)) - ExH(input.probs[[2]],input.probs[[1]]) - ExH(input.probs[[1]],input.probs[[2]]), "\n");



}

## The simple information content of an array of probabilities.
H <- function(ps, inspect=F) {
    sum <- 0;
    for (p in ps) {
        pnum <- as.numeric(p);
        if (inspect) {
            cat("sum:", sum, "- (", pnum, ")log(", pnum, ")\n");
        }
        if (pnum != 0) {
            sum <- sum - pnum * log(pnum, base=2);
        }
    }
    return(sum);
}

## Expected value of some information, averaged with a second set of
## probabilities (from an outcome class object).  Ps and pmod must be the
## same length.
ExH <- function(ps, pmod, inspect=F) {
    sum <- 0;
    if (class(pmod) != "outcome") {
        cat("need an outcome object as the second argument.\n");
        stop();
    }
    if (length(ps) != length(pmod)) {
        cat("Both arguments must be the same length.\n");
        stop();
    }

    for (i in 1:length(ps)) {
        pnum <- as.numeric(ps[i]);
        if (inspect) {
            cat("sum:", sum, "-", pmod[[i]], "* (", pnum,
                ")log(", pnum, ")\n");
        }
        if (pnum != 0) {
            sum <- sum - pmod[[i]] * pnum * log(pnum, base=2);
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


trial1 <- list();
trial1 <- append(trial1, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));
trial1 <- append(trial1, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));

trial2 <- list();
trial2 <- append(trial2, list(outcome(out=c("0","1"),prob=c(0.6,0.4))));
trial2 <- append(trial2, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));

trial3 <- list();
trial3 <- append(trial3, list(outcome(out=c("0","1"),prob=c(0.7,0.3))));
trial3 <- append(trial3, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));

trial4 <- list();
trial4 <- append(trial4, list(outcome(out=c("0","1"),prob=c(0.8,0.2))));
trial4 <- append(trial4, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));

trial5 <- list();
trial5 <- append(trial5, list(outcome(out=c("0","1"),prob=c(0.9,0.1))));
trial5 <- append(trial5, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));

trial6 <- list();
trial6 <- append(trial6, list(outcome(out=c("0","1"),prob=c(0.9,0.1))));
trial6 <- append(trial6, list(outcome(out=c("0","1"),prob=c(0.3,0.7))));

trial7 <- list();
trial7 <- append(trial7, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));
trial7 <- append(trial7, list(outcome(out=c("0","1"),prob=c(1.0,0.0))));

trial8 <- list();
trial8 <- append(trial8, list(outcome(out=c("0","1"),prob=c(0.5,0.5))));
trial8 <- append(trial8, list(outcome(out=c("0","1"),prob=c(0.0,1.0))));

trial9 <- list();
trial9 <- append(trial9, list(outcome(out=c("0","1"),prob=c(0.6,0.4))));
trial9 <- append(trial9, list(outcome(out=c("0","1"),prob=c(0.6,0.4))));

triala <- list();
triala <- append(triala, list(outcome(out=c("0","1"),prob=c(0.7,0.3))));
triala <- append(triala, list(outcome(out=c("0","1"),prob=c(0.7,0.3))));

trialb <- list();
trialb <- append(trialb, list(outcome(out=c("0","1"),prob=c(0.6,0.4))));
trialb <- append(trialb, list(outcome(out=c("0","1"),prob=c(0.7,0.3))));





