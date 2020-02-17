library(gtools)

## g.nerve <- gate(def=function(...) {
##     args <- list(...)[[1]];
##     return(sum(args)/length(args));
## },
## io=gateIO(i=list("in1"=gval(type=float), "in2"=gval(type=float)),
##           o=list("out"=gval(type=float))))

## g.comp <- gate(gateList=list("AND1"=g.and, "AND2"=g.and, "XOR1"=g.xor),
##                cnxn=cnxnList("in1"=cnxns("AND1:in1", "AND2:in2"),
##                              "in2"=cnxns("AND2:in1", "AND1:in2"),
##                              "AND1:out"="XOR1:in1",
##                              "AND2:out"="XOR1:in2",
##                              "XOR1:out"="out"),
##                io=gateIO(i=list("in1"=gval(type=binary),
##                                 "in2"=gval(type=binary)),
##                          o=list("out"=gval(type=binary))));


## Sets up the construction of a function of nInput float inputs and a
## single output.
makeNerve.fn <- function(name, fn, nInputs, x=1, y=2) {

    inputs <- c();
    for (i in 1:nInputs) {
        inputs <- c(inputs, sprintf("\"in%.2d\"=gval(type=float)", i));
    }

    gname <- deparse(name);

    return(paste0(name, " <- gate(def=",
                  paste0(deparse(fn),collapse="\n"),
                 ",io=gateIO(i=list(",
                 paste0(inputs, collapse=","),
                 "),o=list(\"out\"=gval(type=float)),",
                 "params=list(x=", x, ", y=", y, ", name=", gname, ")))"));
}

## A macro to actually execute makeNerve.fn and have the results
## appear in the current context.
makeNerve <- defmacro(name, fn, nInputs, expr={
    eval(parse(text=makeNerve.fn(name, fn, nInputs))); });

## Execute a function with the given args.
exec.fn <- function(name, ..., type="float") {
    args <- list(...);
    out <- c();
    for (i in 1:length(args)) {
        out <- c(out, sprintf("\"in%.2d\"=gval(%f, \"type\"=float)",
                              i, args[[i]]));
    }
    return(paste0("transform(", name, ",",
                  paste0(out, collapse=","), ", inspect=FALSE)"));
}

#transform(g.and,"in01"=gval(0.200000, "type"=float),"in02"=gval(0.300000, "type"=float),"in03"=gval(0.400000, "type"=float) inspect=TRUE)

## The output from this is a gateIOList, whose 'this' entry contains
## the inputs and outputs of the function that was executed.
exec <- defmacro(name, DOTS, expr={ eval(parse(text=exec.fn(name, ...))); })

## A simple test function to calculate the average of an arbitrary
## number of gval inputs.
testf <- function(...) {
    args <- list(...);
    gvArgs <- args[["inputs"]];
    if (length(args) > 1) {
        params <- args[["params"]];
    }
    sumArgs <- 0;
    for (gv in gvArgs) {
        sumArgs <- sumArgs + gv@numVal;
    }
    return(list("out"=gval(sumArgs/length(gvArgs), float)));
}

nerve <- function(...) {
    args <- list(...);
    gvArgs <- args[["inputs"]];
    weights <- rep(1/length(gvArgs), length(gvArgs));
    if (length(args) > 1) {
        params <- args[["params"]];
        weights <- params[["weights"]];
    }
    sumArgs <- 0;
    for (i in 1:length(gvArgs)) {
        sumArgs <- sumArgs + min(1.0, getVal(gvArgs[[i]])) * weights[i];
    }
    return(list("out"=gval(sumArgs, float)));
}

## Construct a layer with nOutputs nodes connected to each of nInputs
## inputs.  Each node has a single output, and the result is meant to
## be a layer in an neural net of some kind.  fName is the name of the
## function doing the real work.
makeLayer.fn <- function(name, fn, nInputs, nOutputs) {

    inputs <- c();
    for (i in 1:nInputs) {
        inputs <- c(inputs, sprintf("\"in%.2d\"=gval(type=float)", i));
    }

    ## Make an atomic gate to use.
    subName <- paste0(name, nInputs, "x", nOutputs);
    atomic <- paste0(subName, " <- gate(def=",
                     paste0(deparse(fn),collapse="\n"),
                     ",io=gateIO(i=list(",
                     paste0(inputs, collapse=","),
                     "),o=list(\"out\"=gval(type=float)),",
                     "params=list(weights=rep(1/", nOutputs, ",", nOutputs,")),",
                     "name=\"", subName, "\"));");

    gates <- c();
    for (i in 1:nOutputs) {
        gates <- c(gates, sprintf("\"nerve%.2d\"=%s", i, subName));
    }

    gateList <- sprintf("list(%s)",
                        paste0(gates, collapse=","));

    cnxns <- c();
    for (i in 1:nInputs) {
        subCnxns <- c();
        for (j in 1:nOutputs) {
            subCnxns <- c(subCnxns, sprintf("\"nerve%.2d:in%.2d\"", j, i));
        }

        cnxns <- c(cnxns, sprintf("\"in%.2d\"=cnxns(%s)", i,
                   paste0(subCnxns, collapse=",")));
    }

    for (j in 1:nOutputs) {
        cnxns <- c(cnxns, sprintf("\"nerve%.2d:out\"=\"out%.2d\"", j, j));
    }

    cnxnList <- sprintf("cnxnList(%s)",
                        paste0(cnxns, collapse=","));

    ins <- c();
    for (i in 1:nInputs) {
        ins <- c(ins, sprintf("\"in%.2d\"=gval(type=float)", i));
    }
    outs <- c();
    for (j in 1:nOutputs) {
        outs <- c(outs, sprintf("\"out%.2d\"=gval(type=float)", j));
    }

    gateIOList <- sprintf("gateIO(i=list(%s), o=list(%s))",
                          paste0(ins, collapse=","),
                          paste0(outs, collapse=","));

    compound <- paste0(name, " <- gate(gateList=", gateList,
                       ", cnxn=", cnxnList, ", io=", gateIOList, ")");

    return(paste(atomic, compound));
}

makeLayer <- defmacro(name, fn, nInputs, nOutputs, expr={
    ## Procure the definition.
    fstr <- makeLayer.fn(name, fn, nInputs, nOutputs);
    ## Execute it.
    eval(parse(text=fstr));
    ## Extract the name of the atomic gate on which the layer is built.
    aname <- strsplit(fstr, " <- ")[[1]][1];
    ## Make sure it's defined at the global level.
    eval(parse(text=paste0("assign(\"", aname, "\",", aname,
                           ", envir=.GlobalEnv);")));
    ## Extract the compound gate name that is the layer.
    fname <- strsplit(regmatches(a, regexpr("; .* <- gate", a)), " ")[[1]][2];
    ## Define it at the global level.
    eval(parse(text=paste0("assign(\"", fname, "\",", fname,
                           ", envir=.GlobalEnv);")));
    fname;});

##
## Make an entire, orderly, network, where the width of each layer is
## given by the input vector of widths, using the fn function.  Note
## that the number in the last entry of widths will be the number of
## outputs.
##
makeTidyNet.fn <- function(nInputs, widths, fn) {

    ## The grand and glorious output to be eval-ed.
    out <- "";

    ## As promised, the number of outputs is just the number of nodes
    ## in the last layer.
    nOutputs <- widths[length(widths)];

    ## Make a bunch of layers.
    layers <- list();
    layerNum <- 0;
    layerInputs <- nInputs;
    layerGNames <- c(); # Gate names
    layerFNames <- c(); # Function names

    superGates <- c();
    superCnxns <- c();
    superIO <- c();

    layerFRoot <- "glay";
    layerRootName <- "g.layer";
    for (width in widths) {
        layerNum <- layerNum + 1;
        layerGName <- sprintf("%s%.3d", layerRootName, layerNum);
        layerGNames <- c(layerGNames, layerGName);
        layerFName <- sprintf("%s%.3dd", layerFRoot, layerNum);
        out <- paste0(out, makeLayer.fn(layerFName,
                                        fn, layerInputs, width), ";\n");

        layerFNames <- c(layerFNames, layerFName);

        ## Lists we need to produce:
        ## list of subgates
        subGates <- c();
        ## connection list
        cnxnList <- c();
        ## input and output lists
        inputList <- c();
        outputList <- c();

        for (i in 1:width)
            subGates <- c(subGates, sprintf("\"%s%.3d\"=%s",
                                            layerFRoot, i, layerFName));

        inCnxns <- c();
        outCnxns <- c();
        inputs <- c();
        outputs <- c();
        for (i in 1:layerInputs) {
            subCnxns <- c();
            for (j in 1:width) {
                subCnxns <- c(subCnxns, sprintf("\"%s%.3d:in%.3d\"",
                                                layerFRoot, j, i));
            }
            inCnxns <- c(inCnxns,
                         paste0(sprintf("\"in%.3d\"=cnxns(", i),
                                paste0(subCnxns, collapse=","), ")"));

            inputs <- c(inputs, sprintf("\"in%.3d\"=gval(type=float)", i));
        }

        for (j in 1:width) {
            outCnxns <- c(outCnxns,
                          paste0(sprintf("\"%s%.3d:out\"", layerFRoot, layerNum),
                                 sprintf("=cnxns(\"out%.3d\")", j)));
            outputs <- c(outputs, sprintf("\"out%.3d\"=gval(type=float)", j));
        }

        out <- paste0(out,
                      sprintf("%s <- gate(gateList=list(", layerGName),
                      paste0(subGates, collapse=",\n"), "),\n",
                      "cnxn=cnxnList(",
                      paste0(inCnxns, collapse=",\n"), ",\n",
                      paste0(outCnxns, collapse=",\n"), "),\n",
                      "io=gateIO(i=list(",
                      paste0(inputs, collapse=","), "),\n",
                      "o=list(",
                      paste0(outputs, collapse=","), "))); \n");

        ## Record a little description of this layer.
        layers[[layerNum]] <- list("name"=sprintf("layer%.3d", layerNum),
                                   "gname"=layerGName,
                                   "numInputs"=length(inputs),
                                   "numOutputs"=length(outputs));

        ## For the next layer, the number of inputs for each node
        ## should be the number of outputs from the current layer.
        layerInputs <- width;
    }

    ## Now we have a list of gates to represent each layer.  We make
    ## one giant gate to hold them all, and connect them all up.
    ## out <- paste0("g.giant <- gate(gateList=list(",
    ##               paste0(gateList, collapse=",\n"),
    ##               "), cnxn=cnxnList(",
    ##               paste0(cnxns, collapse=",\n"),
    ##               "))");

    prevLayerOutputs <- c();
    for (i in 1:nInputs) {
        prevLayerOutputs <- c(prevLayerOutputs, sprintf("in%.3d", i));
    }

    subGates <- c();
    subCnxns <- c();
    inputs <- c();
    outputs <- c();
    for (i in 1:length(layers)) {
        subGates <- c(subGates, sprintf("\"%s\"=%s",
                                        layers[[i]]$name, layers[[i]]$gname));

        for (j in 1:length(prevLayerOutputs)) {
            ## First the inputs for this layer...
            subCnxns <- c(subCnxns, sprintf("\"%s\"=cnxns(\"%s:in%.3d\")",
                                            prevLayerOutputs[j],
                                            layers[[i]]$name, j));
        }
        ## ... now the outputs.
        for (j in 1:layers[[i]]$numOutputs) {
            if (i < length(layers)) {
                target <- sprintf("%s:in", layers[[i + 1]]$name);
            } else {
                target <- "out";
            }
            subCnxns <- c(subCnxns,
                          sprintf("\"%s:out%.3d\"=cnxns(\"%s%.3d\")",
                                  layers[[i]]$name, j, target, j));
        }

        for (j in 1:layers[[i]]$numOutputs) {
            prevLayerOutputs <- c(prevLayerOutputs,
                                  sprintf("%s:out%.3d", layers[[i]]$name, j));
        }
    }

    for (i in 1:layers[[1]]$numInputs) {
        inputs <- c(inputs, sprintf("\"in%.3d\"=gval(float)", i));
    }

    for (i in 1:layers[[length(layers)]]$numOutputs) {
        outputs <- c(outputs, sprintf("\"out%.3d\"=gval(float)", i));
    }

    out <- paste0(out,
                  sprintf("giant <- gate(gateList=list("),
                  paste0(subGates, collapse=",\n"), "),\n",
                  "cnxn=cnxnList(",
                  paste0(subCnxns, collapse=",\n"), "),\n",
                  "io=gateIO(i=list(",
                  paste0(inputs, collapse=","), "),\n",
                  "o=list(",
                  paste0(outputs, collapse=","), "))); \n");

    return(out);
}



makeNerve("g.nerve", fn=testf, 4)

g.nn <- gate(gateList=list("nn1-1"=g.nerve, "nn2-1"=g.nerve, "nn3-1"=g.nerve, "nn4-1"=g.nerve, "nn1-2"=g.nerve, "nn2-2"=g.nerve, "nn3-2"=g.nerve, "nn4-2"=g.nerve, "nn1-3"=g.nerve),
             cnxn=cnxnList("in01"=cnxns("nn1-1:in01","nn2-1:in01","nn3-1:in01","nn4-1:in01"),
                           "in02"=cnxns("nn1-1:in02","nn2-1:in02","nn3-1:in02","nn4-1:in02"),
                           "in03"=cnxns("nn1-1:in03","nn2-1:in03","nn3-1:in03","nn4-1:in03"),
                           "in04"=cnxns("nn1-1:in04","nn2-1:in04","nn3-1:in04","nn4-1:in04"),
                           "nn1-1:out"=cnxns("nn1-2:in01","nn2-2:in01","nn3-2:in01","nn4-2:in01"),
                           "nn2-1:out"=cnxns("nn1-2:in02","nn2-2:in02","nn3-2:in02","nn4-2:in02"),
                           "nn3-1:out"=cnxns("nn1-2:in03","nn2-2:in03","nn3-2:in03","nn4-2:in03"),
                           "nn4-1:out"=cnxns("nn1-2:in04","nn2-2:in04","nn3-2:in04","nn4-2:in04"),
                           "nn1-2:out"=cnxns("nn1-3:in01"),
                           "nn2-2:out"=cnxns("nn1-3:in02"),
                           "nn3-2:out"=cnxns("nn1-3:in03"),
                           "nn4-2:out"=cnxns("nn1-3:in04"),
                           "nn1-3:out"=cnxns("out")),
             io=gateIO(i=list("in01"=gval(type=float),
                              "in02"=gval(type=float),
                              "in03"=gval(type=float),
                              "in04"=gval(type=float)),
                       o=list("out"=gval(type=float)),
                       params=list(2,1)));

#makeCompound <- function(name,

makeNerve("g.ner", fn=testf, 2)

g.n <- gate(gateList=list("nn1-1"=g.ner, "nn2-1"=g.ner, "nn1-2"=g.ner, "nn2-2"=g.ner, "nn1-3"=g.ner),
             cnxn=cnxnList("in01"=cnxns("nn1-1:in01","nn2-1:in01"),
                           "in02"=cnxns("nn1-1:in02","nn2-1:in02"),
                           "nn1-1:out"=cnxns("nn1-2:in01","nn2-2:in01"),
                           "nn2-1:out"=cnxns("nn1-2:in02","nn2-2:in02"),
                           "nn1-2:out"=cnxns("nn1-3:in01"),
                           "nn2-2:out"=cnxns("nn1-3:in02"),
                           "nn1-3:out"=cnxns("out")),
             io=gateIO(i=list("in01"=gval(type=float),
                              "in02"=gval(type=float)),
                       o=list("out"=gval(type=float)),
                       params=list(2,1)));



