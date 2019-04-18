### What we need is an outcome object, to contain symbols and
### associated probabilities, and a gate object, to contain a list of
### alphabets, and a function to transform inputs to an output.

gate.id.counter <- 0;
connection.id.counter <- 0;

## A class to hold a type and range for some gate input.
inputType <- function(baseType="symbol", range=c("0","1")) {
    out <- list();

    if (baseType == "symbol") {
        out[["baseType"]] <- baseType;
        if (class(range) == "character") {
            out[["range"]] <- range;
        } else {
            cat(range, "is not a good range for type 'symbol'.\n");
            stop();
        }
    } else if (baseType == "integer") {
        out [["baseType"]] <- baseType;
        if ((class(range) == "numeric") &&
            (max(range) > min(range))) {
            out[["range"]] <- range;
        } else {
            cat(range, "is not a good range for type 'integer'.\n");
            stop();
        }
    } else if (baseType == "numeric") {
        out [["baseType"]] <- baseType;
        if ((class(range) == "numeric") &&
            (max(range) > min(range))) {
            out[["range"]] <- range;
        } else {
            cat(range, "is not a good range for type 'numeric'.\n");
            stop();
        }
    } else {
        cat(baseType, "is not an available type for gate inputs.\n");
        cat(" Use 'symbol' (character), 'integer' (positive), or 'numeric'.\n");
        stop();
    }

    ## types checked, all fine, create a type-checking function to
    ## return a boolean indicating whether the argument is within the valid
    ## bounds of this type.
    out[["check"]] <- function(arg) {
        if (baseType == "symbol") {
            return(arg %in% range);
        } else if (baseType == "integer") {
            return((arg <= max(range)) && (arg >= min(range)) &&
                   (round(arg) == arg));
        } else {
            return((arg <= max(range)) && (arg >= min(range)));
        }
    }

    return(structure(out, class="inputType"));
}

print.inputType <- function(x) {
    if (x$baseType == "symbol") {
        cat(x$baseType, ": ", sep="");
        for (sym in x$range) cat("\"", sym, "\" ", sep="");
        cat("\n");
    } else {
        cat(x$baseType, ": min: ", min(x$range),
            ", max: ", max(x$range), "\n", sep="");
    }
}

gate.default.typeCatalog <- list(
    binary=inputType("symbol", c("0","1")),
    integer=inputType("integer", c(0,100)),
    number=inputType("numeric", c(-1.0,1.0)));

## A 'type list' is a list of names and type objects to go with them The
## inputTypes used in the gate() constructor is a typeList.
format.typeList <- function(x) {
    out <- "";
    for (name in names(x)) out <- paste(out, name, "(", x[[name]], ") ", sep="");
    return(out);
}

print.typeList <- function(x) {
    cat(format.typeList(x), "\n");
}

## A connectionElement is a connection between some source and a sink.  The
## source is in the key of the connectionList.  We are using an object here
## so that we can eventually use it to hold graphic information about the
## connection.
connectionElement <- function(sink) {
    ## How many entries are in sink? (gregexpr has complicated output)
    numEntries <- sum(gregexpr(",", sink)[[1]] > 0);

    ## If there are multiple entries in sink, we need an id for each one.
    idString <- paste(connection.id.counter:(connection.id.counter+numEntries),
                      collapse=",");

    ## Update the counter for all the ids we've just used.
    connection.id.counter <<- connection.id.counter + numEntries + 1;
    return(structure(list(sink=sink,id=idString),
                     class="connectionElement"));
}

format.connectionElement <- function(cel) {
    return(paste(cel$sink, " (", cel$id, ")", sep=""));
}

print.connectionElement <- function(cel) {
    cat(format.connectionElement(cel), "\n");
}

## A connectionList is a list of connections between outputs and inputs.
## The input specifications look like this: "<gate>.<name>" where <gate> is
## the name of a gate in the gate list and <name> is one of the inputs that
## gate expects.  If the <gate> is missing, this is an input for the whole
## gate list.  The output specification is the same, except that one can
## link a collection of them in a comma-separated list.
print.connectionList <- function(clist) {
    for (name in names(clist))
        cat(name, "-->", format.connectionElement(clist[[name]]), "\n");
}

append.connectionList <- function(clist, src, sink) {
    if (class(clist) != "connectionList") {
        cat("Can only append to a connectionList.\n");
        stop();
    }
    if (length(src) != length(sink)) {
        cat("Length of source array must match sink array.\n");
        stop();
    }

    for (i in 1:length(src)) {
        clist[[ src[i] ]] <- connectionElement(sink[i]);
    }

    return(clist);
}

connectionList <- function(src, sink) {
    if (length(src) != length(sink)) {
        cat("Length of source array must match sink array.\n");
        stop();
    }

    out <- list();
    for (i in 1:length(src)) {
        out[[ src[i] ]] <- connectionElement(sink[i]);
    }

    return(structure(out, class="connectionList"));
}

## Define a type-checking function for an input list.  The argument list is
## a list or argument names and values.  Typelist is a list of types, and
## type objects that contain a checking function ("check") and inputTypes is
## a list of names and types expected by some function.  If the argList is
## not valid, we bomb out, but otherwise just return T or F depending on
## whether it looks kosher or not.
gate.checkTypes <- function(argList, typeCatalog, inputTypes) {
    ## The argList list is assumed to be a collection of names and
    ## values.  For each item in that list, we run the appropriate
    ## checker function.
    if (class(argList) != "list") {
        cat("The input to a gate is a list of input names and values.\n");
        print(argList);
        cat("... does not qualify.\n");
        stop();
    }
    ## There should be exactly as many name,value pairs as name,type pairs.
    if (length(argList) != length(inputTypes)) {
        cat("There are", length(argList), "arguments to this transformation, but",
            length(inputTypes), "are expected.\n");
        stop();
    }

    for (arg in names(argList)) {

        ## Run the check function for each type on
        if (!typeCatalog[[inputTypes[[arg]]]]$check(argList[[arg]])) {
            cat(arg, "is not a valid", inputTypes[[arg]],
                "type object.\n");
            return(FALSE);
        }
    }
    return(TRUE);
}

## Takes a collection of inputs (inputList), a collection of gates
## (gateList), and a list of connections between them all, and tries to come
## up with a list of outputs.
##
## This function has a concept of time, in that it executes all the
## functions in its list for which there are inputs, and then ticks the
## clock forward and executes all the next functions for which there are
## inputs, and keeps going until there is an output value for each slot in
## the output list.  On input, the output list is a list of names and empty
## values.
gate.execute <- function(inputList, connectionList, gateList, outList,
                         inspect=FALSE, itermax=100) {

    ## Create an empty list of values and populate it with values from the
    ## inputList, according to the connections in the connectionList.  The
    ## connectionlist has entries like this: cL[["in1"]]$sink = "AND1.in1".
    ## Multiple connections can be listed in a comma-separated list like
    ## this: "AND1.in1,AND2.in2". No spaces at the commas.
    valueList <- list();
    for (inputName in names(inputList)) {
        if (inputName %in% names(connectionList)) {
            for (entry in strsplit(connectionList[[inputName]]$sink, ",")[[1]]) {
                valueList[[entry]] <- inputList[[inputName]];
            }
        } else {
            cat("Missing value in connectionList:", inputName, "\n");
            cat("connectionList names: ");
            for (name in names(connectionList)) cat(name, " ");
            cat ("\n");
            stop();
        }
    }

    ## At this point, the valueList is primed with entries like
    ## vL[["AND1.in1"]]="0" We will now loop through all the functions in
    ## our gateList and execute the ones where there is sufficient data.
    ## Note that this means that some gate transformations will be executed
    ## multiple times, but this is ok, so we can have circuits with feedback.
    masterOutput <- list();

    showValueList <- function(v, prefix="") {
        for (name in names(v)) cat(prefix, name, ": ", v[[name]], "\n", sep="");
    }
    showMasterOutput <- function(m, prefix="") {
        for (gname in names(m)) {
            cat(prefix, gname," output:\n", sep="");
            showValueList(m[[gname]], prefix=paste(prefix," "));
        }
    }

    if (inspect) {
        cat("input list:\n");
        showValueList(inputList, prefix="  ");
        cat("connections:\n");
        for (name in names(connectionList))
            cat(" ", name, "-->",
                format.connectionElement(connectionList[[name]]), "\n");
    }


    ## We're going to repeat until the output list is filled up.  The test
    ## is below, just after the values are transferred from the
    ## masterOutput list to the value list, and we check the value list for
    ## names identified in the output list.
    watchCount <- 1;
    repeat {

        if (inspect) {
            cat("value list (iteration ", watchCount, "):\n", sep="");
            showValueList(valueList, prefix="  ");
            cat("-------------------------\n");
        }

        for (gateName in names(gateList)) {
            ## Do we have all the inputs this gate needs?
            gateInputs <- list();

            ## Copy the inputs from the value list to the input list.
            for (inputName in names(gateList[[gateName]]$inputTypes)) {
                gateInputs[[inputName]] <-  # <- name
                    valueList[[paste(gateName, inputName, sep=".")]]; # <- value
            }

            ## If we have all the inputs the gate needs, execute its transform.
            if (length(gateInputs) == length(gateList[[gateName]]$inputTypes)) {
                if (inspect) cat("processing:", gateName, "\n");
                masterOutput[[gateName]] <-
                    gateList[[gateName]]$transform(gateInputs);
            } else {
                if (inspect) cat("skipping:", gateName, "\n");
            }
        }

        if (inspect) {
            cat("master output:\n");
            showMasterOutput(masterOutput, prefix="  ");
            cat("------------------------\n");
        }

        ## We have a masterOutput list here that has an output string from all
        ## the gates that were run just now.  We need to use the connectionList
        ## to copy the outputs into the valueList, so they can be inputs to the
        ## next round.
        for (gateName in names(masterOutput)) {
            for (outputName in names(masterOutput[[gateName]])) {
                outputQualifiedName <- paste(gateName, outputName, sep=".");
                valueList[[connectionList[[outputQualifiedName]]$sink]] <-
                    masterOutput[[gateName]][[outputName]];
            }
        }

        ## Check to see if all the values anticipated in the outList exist
        ## in the valueList.  If they do, copy them to the outList, which
        ## will be the return value.
        missing <- FALSE;
        for (outputName in names(outList)) {
            if (outputName %in% names(valueList)) {
                outList[[outputName]] <- valueList[[outputName]];
            } else {
                ## We're missing some values, so repeat.
                missing <- TRUE;
            }
        }
        ## Note the watchdog counter, just in case.
        watchCount <- watchCount + 1;
        if ((!missing) || (watchCount > itermax)) break;
    }

    if (missing && (watchCount > itermax))
        cat("The gate did not produce all the necessary outputs.\n");
    return(outList);
}

## A 'gate' object has a list of input names and types and a 'transform'
## function to transform inputs into a list of one or more output values.
## The transformation function is either an R function or it is a
## collection of other gate objects, with a table indicating how the the
## input list feeds into those gates, and how they are hooked up to one
## another.
##
## A gate object also has a graphical representation, either as a single
## node with edges in and out, or a collection of nodes and edges.
gate <- function(inputTypes,
                 transform,
                 connectionList=list(),
                 outList=list(out=""),
                 color=1,
                 shape=1,
                 typeCatalog=gate.default.typeCatalog) {
    out <- list();
    gate.id.counter <<- gate.id.counter + 1; ## global
    out[["id"]] <- gate.id.counter;

    ## The input list should be a bunch of names and a valid name from the
    ## type list for each one.
    for (name in names(inputTypes)) {
        if (!(inputTypes[[name]] %in% names(typeCatalog))) {
            cat(name, " has an invalid input type description ('",
                inputTypes[[name]], "')  Use one from this list:\n", sep="");
            print(typeCatalog);
            stop();
        }
    }

    ## Input list checks out.
    out[["inputTypes"]] <- structure(inputTypes, class="typeList");
    out[["outList"]] <- outList;

    ## The input transform can take two possible forms.  It is either a
    ## function (class='function'), that accepts an input list of
    ## name,value pairs and outputs a similar list at the other end, or it
    ## is an assembly of other gate objects (class='list').
    if (class(transform) == "function") {
        out[["type"]] <- "atomic";
        ## We want to create the function with an arglist specified by
        ## inputTypes and do a certain amount of type checking before
        ## passing it to the input transformation.  The output value of a
        ## transform is a list of one or more labeled values.
        out[["transform"]] <-
            function(argList, inspect=FALSE) {
                if (!gate.checkTypes(argList=argList,
                                     typeCatalog=typeCatalog,
                                     inputTypes=inputTypes)) stop();
                return(transform(argList));
            };
    } else if (class(transform) == "list") {
        out[["type"]] <- "composite";
        ## The transform is a list of other gates, hopefully accompanied by
        ## a connection list.
        out[["gateList"]] <- transform;
        out[["connectionList"]] <- connectionList;
        out[["transform"]] <-
            function(argList, inspect=FALSE, itermax=100) {
                if (!gate.checkTypes(argList=argList,
                                     typeCatalog=typeCatalog,
                                     inputTypes=inputTypes)) stop();
                return(gate.execute(inputList=argList,
                                    gateList=out[["gateList"]],
                                    connectionList=connectionList,
                                    outList=outList,
                                    inspect, itermax));
            };
    } else {
        cat("transform must be a function or a list.\n");
        stop();
    }
    out[["color"]] <- color;
    out[["shape"]] <- shape;

    class(out) <- "gate";
    return(out);
}

print.gate <- function(g) {

    cat("type:", g$type, "\n");
    cat("node ID:", g$id, "\n");
    cat("inputTypes:", format.typeList(g$inputTypes), "\n");
    cat("expected outputs: ");
    for (oname in names(g$outList)) cat(oname, " ");
    cat("\n");

    if (g$type == "composite") {
        cat("nodes: ");
        for (name in names(g$gateList)) cat(name, " ");
        cat("\n");

        cat("connections:\n");
        for (name in names(g$connectionList))
            cat("  ", name, "-->",
                format.connectionElement(g$connectionList[[name]]), "\n");
        cat("\n");
    }

    cat("color:", g$color, "\n");
    cat("shape:", g$shape, "\n");
}


## compiles a table of nodes for a gate.  If the node is atomic, this is
## pretty simple.  If it is composite, the function is called recursively
## to work out the whole structure.
gate.nodeList <- function(g, prefix="", uid=1) {

    ## Check gate type
    if (g$type == "atomic") {
        return(data.frame(gid=g$id, id=uid, label=prefix,
                          stringsAsFactors=FALSE));
    } else {
        ## This is a composite gate, and g$gateList is a list of other gates.
        out.nodeList <- data.frame();

        ## Sort through the subsidiary gates...
        for (name in names(g$gateList)) {

            ## ... making a nodeList for each of them
            d <- gate.nodeList(g$gateList[[name]], prefix=name, uid=uid);

            ## The gate ID is arbitrary, just needs to be unique in this table.
            uid <- uid + dim(d)[1];

            if (prefix != "") {
                for (i in 1:length(d$label)) {
                    d$label[i] <- paste(prefix, d$label[i], sep=".");
                }
            }
            out.nodeList <- rbind(out.nodeList, d);
        }
        return(out.nodeList);
    }
}

## Compile a table of edges for a gate, with 'from' and 'to' columns.
gate.edgeList <- function(g, prefix="", inspect=FALSE) {

    ##  If the node is atomic, there aren't any edges, so we return an
    ## empty data frame to rbind to the earlier work.
    if (g$type == "atomic") return(data.frame());

    ## Prepare an empty data frame to accumulate the connections.
    out.edgeList <- data.frame();

    ## Get the connections for any child nodes.
    for (gateName in names(g$gateList)) {
        if (inspect) cat("processing:", gateName, "\n");
        out.edgeList <- rbind(out.edgeList,
                              gate.edgeList(g$gateList[[gateName]],
                                            prefix=gateName));
    }

    for (connectName in names(g$connectionList)) {

        ## There might be multiple sinks for any connection.  We trust that
        ## the ids were generated correctly so there is an id number for
        ## each connection sink.
        sinkNames <- strsplit(g$connectionList[[connectName]]$sink, ",")[[1]];
        sinkIds <- strsplit(g$connectionList[[connectName]]$id, ",")[[1]];

        if (length(sinkNames) != length(sinkIds)) {
            cat("The connectionList has inconsistent connection IDs\n");
            stop();
        }

        if (prefix == "") {
            d <- data.frame(id=as.numeric(sinkIds),
                            from=connectName,
                            to=sinkNames);
        } else {
            d <- data.frame(id=as.numeric(sinkIds),
                            from=paste(prefix, connectName, sep="."),
                            to=paste(prefix, sinkNames, sep="."));
        }

        out.edgeList <- rbind(out.edgeList, d);
    }
    return(out.edgeList);
}

## TBD:
## Need treatment of final inputs and outputs
## Need treatment of multiple sinks.
## Node ids are not unique like labels. Maybe just generate on fly?
## Should add color and shape info to node and edge lists.
## (Also add color to edge defs.)



test.inlist <- list(in1="0",in2="1", in3="1");
test.intlist <- list(in1="binary", in2="binary", in3="binary");

test.clist <- connectionList("in1","AND.in1");
test.clist <- append.connectionList(test.clist, "in1", "AND1.in1");
test.clist <- append.connectionList(test.clist, "in2", "AND1.in2,AND2.in1");
test.clist <- append.connectionList(test.clist, "in3", "AND2.in2");
test.clist <- append.connectionList(test.clist, "AND1.out", "OR3.in1");
test.clist <- append.connectionList(test.clist, "AND2.out", "OR3.in2");
test.clist <- append.connectionList(test.clist, "OR3.out", "out");

test.ANDfun <- function(inlist) {
    out <- TRUE;
    for (l in inlist) out <- out && (l == "1");
    return(list(out=(if (out) "1" else "0")));
}
test.ORfun <- function(inlist) {
    out <- FALSE;
    for (l in inlist) out <- out || (l == "1");
    return(list(out=(if (out) "1" else "0")));
}
test.ORgate <- gate(list(in1="binary",in2="binary"), test.ORfun);
test.ANDgate <- gate(list(in1="binary",in2="binary"), test.ANDfun);
test.glist <- list(AND1=test.ANDgate, AND2=test.ANDgate, OR3=test.ORgate)
test.olist <- list(out="")

test.COMPgate <- gate(input=test.intlist,
                      conn=test.clist,
                      transform=test.glist,
                      out=test.olist)

test.glist2 <- list(AND1=test.ANDgate, C1=test.COMPgate, OR1=test.ORgate)
test.clist2 <- connectionList("in1", "AND1.in1,C1.in1");
test.clist2 <- append.connectionList(test.clist2, "in2", "AND1.in2,C1.in2");
test.clist2 <- append.connectionList(test.clist2, "in3","C1.in3");
test.clist2 <- append.connectionList(test.clist2, "C1.out","OR1.in1");
test.clist2 <- append.connectionList(test.clist2, "AND1.out","OR1.in2");
test.clist2 <- append.connectionList(test.clist2, "OR1.out","out");

test.COMP2gate <- gate(input=test.intlist,
                       conn=test.clist2,
                       transform=test.glist2,
                       out=test.olist)

test.glist3 <- list(AND1=test.ANDgate, C2=test.COMP2gate);
test.clist3 <- connectionList("in1", "AND1.in1,C2.in1");
test.clist3 <- append.connectionList(test.clist3, "in2", "AND1.in2,C2.in2,C2.in3")
test.clist3 <- append.connectionList(test.clist3, "C2.out", "out1")
test.clist3 <- append.connectionList(test.clist3, "AND1.out", "out2")

test.COMP3gate <- gate(input=test.intlist[1:2],
                       conn=test.clist3,
                       transform=test.glist3,
                       out=list("out1"="","out2"=""))

#print.gate

#plot.gate

## Creates a virtual function from an assemblage of gate objects.
##gateFromGates <- function(gateList, connectionTable
