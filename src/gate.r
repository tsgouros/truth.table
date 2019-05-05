## The idea is that we want to assert a time base for everything equally, so
## that we can 'tick' the gates together, but we also want a hierarchical
## arrangement of gates, so we can define functional groups of gates as a
## unit.  But we want the functional groups to use the same 'ticks' as their
## parent.
##
## This means that a transformation might have a contingent nature, returning
## not a value, but a state of all the inputs to each of its operations, that
## can be absorbed into the state of the parent.  So our record of the state
## (the 'value list') has to be hierarchical in some fashion.  We will do
## that by creating a hierarchy of the names of the inputs and outputs.  So
## the root list might begin with a list of three inputs, but those get
## mapped to the various inputs to this or that gate, and "in1" is translated
## to "AND1.in1" or "C2.in2" or whatever.  Then the AND1 and C2 gates are
## executed with whatever inputs are available.  Upon execution of AND1, its
## inputs are renamed from "AND1.in1" to simply "in1", which is how the AND1
## gate thinks of it.  Its output value list has names like "out" or "out1"
## or whatever, and when reincorporating those values into the parent list,
## they are prefixed with "AND1".  So:
##
##  "AND1.in1" -> "in1" -> execute AND1 gate -> "out" -> "AND1.out"


## TO DO list
##
##  - A function to abstract a truth table from an arbitrary gate object.
##    This would require going through all the possible input combinations
##    some set number of times to estimate the probabilities of the
##    different outputs.
##
##  - Better control of the graphics? Don't really understand how they work
##    at this point.
##
##  - Some more testing of different gate geometries.  I think the export
##    works and gets all the connections right, but do I really know?  Not
##    so much.
##
##  This is all in service of experimenting with geometries that might
##  require more or less error correction.  The point of doing this with a
##  graph-theoretic analysis thingy is that there might be some kind of
##  graph-theoretic approach to the problem of determining when a transform
##  or an input is unimportant enough not to require much in the way of
##  error checking.
##
##  4/18/19 ts

library(plyr)
library(DiagrammeR)

## A couple of global variables for generating unique IDs for nodes and
## edges.  Only really used on export.  See the gate.nodeList() function,
## for example.
gate.id.counter <- 0;
connection.id.counter <- 0;

## A class to hold a type and range for some gate input.  We have a type
## system, where you can establish a data type and range of acceptable
## values for some gate input.  The available types are built on top of
## 'symbol' (character string), integer, and floats.
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

## A handful of default types to use.
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
gate.checkTypes <- function(argList, typeCatalog, inputTypes,
                            inspect=FALSE, prefix="") {

    ## If the arglist is a gate.io object, just get the inputs list.
    if (class(argList) == "gate.io") argList <- argList$inputs;

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
        if (inspect) cat("There are", length(argList),
                         "arguments to this transformation, but",
                         length(inputTypes), "are expected.\n");
        return(FALSE);
    }

    for (arg in names(argList)) {

        ## If there's a name mismatch, that's not good.
        if (!(arg %in% names(inputTypes))) {
            if (inspect) cat(arg, "does not appear in the list of expected",
                             "inputs:", paste(names(inputTypes), collapse=" "),
                             "\n");
            return(FALSE);
        }

        ## Run the check function for each type on
        if (!typeCatalog[[inputTypes[[arg]]]]$check(argList[[arg]])) {
            if (inspect) cat(arg, "is not a valid", inputTypes[[arg]],
                             "type object.\n");
            return(FALSE);
        }
    }
    return(TRUE);
}

## Sets up inputs and outputs for a pair of name-value lists.
gate.io <- function(inputs, outputs=list("out"=NULL)) {
    return(structure(list(inputs=inputs, outputs=outputs), class="gate.io"));
}

## This does double duty for printing gate.io objects, and also for lists of
## such objects.  Though of course it is only summoned automatically for the
## former.
print.gate.io <- function(gio, prefix="") {

    if (class(gio) == "gate.io") {

        cat(prefix, "inputs:\n", prefix, "  ", sep="");
        if (class(gio$inputs) == "list") {
            for (name in names(gio$inputs))
                cat(name, "=", gio$inputs[[name]], " ", sep="");
            cat("\n");
        } else {
            cat(prefix, "Malformed gate.io object, input list seems to be:");
            print(gio$inputs);
            stop();
        }
        cat(prefix, "outputs:\n", prefix, "  ", sep="");
        if (class(gio$outputs) == "list") {
            for (name in names(gio$outputs))
                cat(name, "=", gio$outputs[[name]], " ", sep="");
            cat("\n");
        } else {
            cat(prefix, "Malformed gate.io object, output list seems to be:");
            print(gio$outputs);
            stop();
            print.gate.io(gio$outputs, prefix=paste("|", prefix));
        }
    } else if (class(gio) == "list") {

        for (gname in names(gio)) {
            cat(prefix, gname, ": \n", sep="");
            print.gate.io(gio[[gname]], prefix=paste(prefix, "| ", sep=""));
        }

    } else {
        cat(prefix, "Looking for gate.io.",
            "Don't know how to print object of class", class(gio), "\n");
        print(gio);
        stop();
    }
}

## A named list of gate.io objects could be nested.  This function accepts a
## dot-separated name like "C2.C1.AND1.in1" and a list and returns the value
## indicated.
gate.io.get <- function(gioList, name, inspect=FALSE, prefix="") {
    ## gioList is either a list of gate.io objects, or a single gate.io object.
    ## name is either (1) a name like "C2.C1.AND1.in1" or (2) an already
    ## split name like ["C2","C1","AND1","in1"] or (3) an atomic name like
    ## "in1".
    if (inspect) {
        cat(prefix, " gate.io.get searching for ", paste(name, collapse="."),
            " (", length(name),  ") in\n", sep="");
        print.gate.io(gioList, prefix=paste("|", prefix));
    }

    if (length(name) == 1) {
        ## Case 1 or 3
        if (grepl("\\.", name)) {
            ## Case 1.
            if (inspect) cat(prefix, "gate.io.get: Case 1 (splitting).\n");
            names <- strsplit(name, "\\.")[[1]];
            return(gate.io.get(gioList, names,
                               inspect=inspect, prefix=prefix));
        } else {
            ## Case 3.  Note that we might have got here either because we
            ## are looking for something in the "this" element, or because
            ## we've narrowed it down to the last element.  So this messy if
            ## statement covers both cases.
            if (inspect) cat(prefix, "gate.io.get: Case 3 (this).\n");
            if (class(gioList) == "gate.io") {
                ## First case (gioList is a gate.io object)
                gioList <- gioList;
                ## Is it among the inputs?
                if (name %in% names(gioList$inputs)) {
                    if (inspect) cat(prefix, "found it among the inputs.\n");
                    return(gioList$inputs[[name]]);

                } else if (name %in% names(gioList$outputs)) {
                    ## Or the outputs?
                    if (inspect) cat(prefix, "found it among the outputs.\n");
                    return(gioList$outputs[[name]]);

                } else {
                    if (inspect)
                        cat(prefix, "gate.io.get:", name,
                            "does not appear to be in this IO list.\n");
                    return(NULL);
                }

            } else {
                ## Second case (we're looking in the this element).
                gioList[["this"]] <- gioList[["this"]];
                if (name %in% names(gioList[["this"]]$inputs)) {
                    if (inspect) cat(prefix, "it was an input.\n");
                    return(gioList[["this"]]$inputs[[name]]);

                } else if (name %in% names(gioList[["this"]]$outputs)) {
                    if(inspect) cat(prefix, "it was an output.\n");
                    return(gioList[["this"]]$outputs[[name]]);
                } else {
                    if (inspect)
                        cat(prefix, "gate.io.get:", name,
                            "does not appear to be in this IO list.\n");
                    return(NULL);
                }
            }
        }
    } else {
        ## Case 2.  We have a separated name list.  If the next name is not
        ## among the inputs and we have a newVal, we'll have to create it.
        if (inspect) cat(prefix, "gate.io.get: Case 2 (split).\n");
        if (name[1] %in% names(gioList)) {

            return(gate.io.get(gioList[[name[1]]], name[-1],
                               inspect=inspect, prefix=paste("|", prefix)));

        } else {
            if(inspect) cat(prefix, "gate.io.get can't find:",
                            paste(name, collapse="."), "\n");
            return(NULL);
        }
    }
}


## Sets a value within a potentially-nested value list and returns the
## modified result.  If you try to set something that does not exist, it is
## created for you.
##
## There's a certain amount of guessing done on the names you're using, so if
## your outputs are not named "out*" or "result" or something like that, it
## might not work.  Or if you name an input "out", you're asking for trouble.
gate.io.set <- function(gioList, name, newVal=NULL,
                        inspect=FALSE, prefix="") {
    ## gioList is either a list of gate.io objects, or a single gate.io object.
    ## name is either (1) a name like "C2.C1.AND1.in1" or (2) an already
    ## split name like ["C2","C1","AND1","in1"] or (3) an atomic name like
    ## "in1".
    if (inspect) {
        cat(prefix, "gate.io.set searching for ", paste(name, collapse="."),
            " (", length(name),  ") ", sep="");
        if (!is.null(newVal)) {
            cat(" to set it to ->", newVal, "\n");
        }
        print.gate.io(gioList, prefix=paste("|", prefix));
    }

    if (length(name) == 1) {
        ## Case 1 or 3
        if (grepl("\\.", name)) {
            ## Case 1.
            if (inspect) cat(prefix, "gate.io.set: Case 1 (splitting).\n");
            names <- strsplit(name, "\\.")[[1]];
            return(gate.io.set(gioList, names, newVal=newVal,
                               inspect=inspect, prefix=prefix));
        } else {
            ## Case 3.  Note that we might have got here either because we
            ## are looking for something in the "this" element, or because
            ## we've narrowed it down to the last element.  So this messy if
            ## statement covers both cases.
            if (inspect) cat(prefix, "gate.io.set: Case 3 (this).\n");
            if (class(gioList) == "gate.io") {
                ## First case (gioList is a gate.io object)
                gioList <- gioList;
                if (name %in% names(gioList$inputs)) { ## Is it among the inputs?
                    ## If we have a newVal, this is a set command.
                    if (!is.null(newVal)) {
                        gioList$inputs[[name]] <- newVal;
                        return(gioList);
                    }

                } else if (name %in% names(gioList$outputs)) { ## Or the outputs?
                    if (!is.null(newVal)) {
                        gioList$outputs[[name]] <- newVal;
                        return(gioList);
                    }
                } else {
                    if (!is.null(newVal)) {

                        ## If we are here, we need to add the name, so we can
                        ## add the value.  We are totally guessing whether
                        ## it's for the input or output.
                        if (grepl("out|res", name)) {
                            gioList$outputs[[name]] <- newVal;
                        } else {
                            gioList$inputs[[name]] <- newVal;
                        }
                        return(gioList);
                    }
                }
            } else {
                ## Second case (we're looking in the this element).
                gioList[["this"]] <- gioList[["this"]];
                if (name %in% names(gioList[["this"]]$inputs)) {
                    if (!is.null(newVal)) {

                        gioList[["this"]]$inputs[[name]] <- newVal;
                        return(gioList);
                    }

                } else if (name %in% names(gioList[["this"]]$outputs)) {
                    if (!is.null(newVal)) {

                        gioList[["this"]]$outputs[[name]] <- newVal;
                        return(gioList);
                    }
                } else {
                    if (!is.null(newVal)) {

                        ## If we are here, we need to add the name, so we can
                        ## add the value.  We are totally guessing whether
                        ## it's for the input or output.
                        if (grepl("out|res", name)) {
                            gioList[["this"]]$outputs[[name]] <- newVal;
                        } else {
                            gioList[["this"]]$inputs[[name]] <- newVal;
                        }
                        return(gioList);
                    }
                }
            }
        }
    } else {
        ## Case 2.  We have a separated name list.  If the next name is not
        ## among the inputs and we have a newVal, we'll have to create it.
        if (inspect) cat(prefix, "gate.io.set: Case 2 (split).\n");
        if (name[1] %in% names(gioList)) {

            if (!is.null(newVal)) {

                gioList[[name[1]]] <- gate.io.set(gioList[[name[1]]], name[-1],
                                                  newVal=newVal,
                                                  inspect=inspect,
                                                  prefix=paste("|", prefix));
                return(gioList);
            }
        } else if (!is.null(newVal)) {

            if (length(name) == 2) {
                gioList[[name[1]]] <- gate.io(inputs=list());
            } else {
                gioList[[name[1]]] <- list();
            }
            gioList[[name[1]]] <- gate.io.set(gioList[[name[1]]], name[-1],
                                              newVal=newVal,
                                              inspect=inspect,
                                              prefix=paste("|", prefix));
            return(gioList);

        } else {
            if(inspect) cat("gate.io.set: can't find:",
                            paste(name, collapse="."), "\n");
            return(NULL);
        }
    }
}

## Work through the valueList and use the connectionList to copy values
## where they belong.  The connection name is the source, the value the
## sink, which may have multiple entries.  The connectionlist has entries
## like this: cL[["in1"]]$sink = "AND1.in1".  Multiple connections can be
## listed in a comma-separated list like this: "AND1.in1,AND2.in2". No
## spaces at the commas.
gate.makeConnections <-
    function(valueList, connectionList, inspect=FALSE, prefix="") {

    for (cname in names(connectionList)) {

        if (inspect) {
            cat(prefix, "Checking ", cname, " against \n");
            print.gate.io(valueList, paste("|", prefix));
            cat(prefix, "----------------------------\n");
        }

        ## Need to move this value:
        newInput <- gate.io.get(valueList, cname, inspect);

        if (!is.null(newInput)) {

            for (entry in strsplit(connectionList[[cname]]$sink, ",")[[1]]) {
                ## To here:
                valueList <- gate.io.set(valueList, entry, newVal=newInput,
                                         inspect=inspect, prefix=prefix);

            }
        } else {
            ## There is an entry in the connection list that does not appear
            ## in the input list.
            if (inspect) {
                cat(prefix, "Value in connectionList does not appear",
                    "in valueList:", cname, "\n");
            }
        }
    }
    return(valueList);
}


## This function accepts a list of inputs and a gate.  It copies the items in
## the input list to wherever the gate's connection list dictates, then
## executes as many components of the gate have a complete set of inputs.
##
## The input is a gate.io object or a list of gate.io objects with one of
## them identified as 'this'.  The output is exactly the same thing, with
## values added and updated as necessary, so that this function can be run
## multiple times to 'tick' the values through the gate.  See
## gate.execute.iter, below.
gate.execute <- function(inputList, gate, inspect=FALSE, prefix="") {
    ## The inputList is either a single gate.io object, or a list of them.
    ## If it's a list, the special keyword "this" is used to indicate the
    ## inputs and outputs to this particular gate.
    if (class(inputList) == "gate.io") {
        valueList <- list(this=inputList);
    } else {
        valueList <- inputList;
    }

    if (TRUE) {
        cat(prefix, "Entering gate.execute with \n");
        print.gate(gate, prefix=prefix);
    }

    if (gate$type == "atomic") {
        cat("*************************** atomic\n");
        print(valueList);
        valueList$this$outputs <- gate$transform(valueList$this$inputs,
                                                 inspect=inspect,
                                                 prefix=prefix);
    } else {
        ## This is a composite gate.  First, copy all the input values to
        ## wherever they need to be copied to, according to the connections
        ## recorded in this gate's connection list.
        valueList <- gate.makeConnections(valueList, gate$connectionList,
                                          inspect=inspect, prefix=prefix);


        ## The valueList is prepared, so we will now loop through all gates
        ## in our composite gate, and execute whichever we can.  The
        ## 'checkTypes() function (wrapped inside the $transform method) will
        ## prevent execution of the gates for which there aren't enough
        ## inputs.
        for (subGateName in names(gate$gateList)) {
            if (inspect) cat(prefix, "processing", subGateName, "\n");

            ## Gather all the relevant entries from the valueList
            gateRegex <- paste("^", subGateName, "\\.*", sep="")
            subGateEntries <- grep(gateRegex, names(valueList));

            ## It's possible this subGate has no presence in the valueList at
            ## this point.
            if (length(subGateEntries) == 0) next;

            ## Trim the names from valueList to work for this subGate.
            valueSubList <- list();
            for (i in subGateEntries) {

                subName <- sub(gateRegex, "", names(valueList)[i]);
                if (nchar(subName) == 0) subName <- "this";
                valueSubList[[subName]] <- valueList[[i]];
            }

            ## Execute the gate with the resulting list.
            subGate <- gate$gateList[[subGateName]];
            if (inspect) {
                cat(prefix, "********** executing:", subGateName, "with:\n");
                print.gate.io(valueSubList, prefix);
            }
            valueSubList <- subGate$execute(valueSubList,
                                            inspect=inspect,
                                            prefix=paste("|", prefix));

            ## Now add those values back to the original valueList.
            for (valueName in names(valueSubList)) {
                parentGateName <- "";
                if (valueName == "this") {
                    parentGateName <- subGateName;
                } else {
                    parentGateName <- paste(subGateName, valueName, sep=".");
                }

                valueList[[parentGateName]] <- valueSubList[[valueName]];
            }
        }

########### Note that there is a "completed" function that checks to see if
########### there is a value for each of the expected outputs, and which
########### could be deployed here.

    }

    return(valueList);
}

## Accepts a set of inputs and a gate, and returns a set of outputs
## processed by the gate.

## Takes a list of inputs (inputList), a collection of gates (gateList), and
## a list of connections between them all, and tries to come up with a list
## of outputs.
##
## This function has a concept of time, in that it executes all the
## functions in its list for which there are inputs, and then ticks the
## clock forward and executes all the next functions for which there are
## inputs, and keeps going until there is an output value for each slot in
## the output list.  On input, the output list is a list of names and empty
## values.
##
## On input you have an inputList with names and values of the various
## inputs.  These should all be represented in the "source" side of the
## connectionList.  The inputList becomes our "valueList" and its values are
## copied to the respective "sink" of the connections.  The corresponding
## gates are then executed, and the output values inserted in the value list,
## which is returned.  We repeat this tickMax times, or until the output
## value list contains all the values named in the outList, whichever comes
## first.
##
## The "values" of the input list can themselves be a list of names and
## values, if the gate indicated is a composite.


gate.execute.iter <- function(inputList, gate, tickMax=100,
                              inspect=FALSE, prefix="") {

    tick <- 1;

    ## We want to repeat the execution of the gate until there is a complete
    ## set of outputs, or until we have exceeded the tickMax value.
    repeat {
        valueList <- gate.execute(inputList, gate,
                                  inspect=inspect, prefix=prefix);

        ## This might also be a test to see if values have stabilized.
        if (gate$complete(valueList$this$output)) break;

        tick <- tick + 1;
        if (tick > tickMax) break;

    }

    return(valueList$this$output);
}





##     masterOutput <- list();

##     showValueList <- function(v, prefix="") {
##         for (name in names(v)) cat(prefix, name, ": ", v[[name]], "\n", sep="");
##     }
##     showMasterOutput <- function(m, prefix="") {
##         for (gname in names(m)) {
##             cat(prefix, gname," output:\n", sep="");
##             showValueList(m[[gname]], prefix=paste(prefix," "));
##         }
##     }

##     if (inspect) {
##         cat(prefix, "input list:\n");
##         showValueList(inputList, prefix=prefix);
##         cat(prefix, "connections:\n");
##         for (name in names(gate$connectionList))
##             cat(" ", name, "-->",
##                 format.connectionElement(gate$connectionList[[name]]), "\n");
##     }

##     for (gateName in names(gate$gateList)) {
##         ## Do we have all the inputs this gate needs?
##         gateInputs <- list();

##         cat("checking:", gateName, "\n");

##         ## Copy the inputs from the value list to the input list.
##         for (inputName in names(gate$gateList[[gateName]]$inputTypes)) {
##             gateInputs[[inputName]] <-  # <- name
##                 valueList[[paste(gateName, inputName, sep=".")]]; # <- value
##         }

##         showValueList(valueList);

##         ## If we have all the inputs the gate needs, execute its transform.
##         if (length(gateInputs) == length(gate$gateList[[gateName]]$inputTypes)) {
##             if (inspect) cat(prefix, "processing:", gateName, "\n");
##             masterOutput[[gateName]] <-
##                 gate$gateList[[gateName]]$transform(gateInputs, inspect=inspect);
##         } else {
##             if (inspect) cat(prefix, "skipping:", gateName, "\n");
##         }
##     }

##     if (inspect) {
##         cat(prefix, "master output:\n");
##         showMasterOutput(masterOutput, prefix="  ");
##         cat("------------------------\n");
##     }
##     return(masterOutput);
## }






##     ## We're going to repeat until the output list is filled up.  The test
##     ## is below, just after the values are transferred from the
##     ## masterOutput list to the value list, and we check the value list for
##     ## names identified in the output list.
##     watchCount <- 1;
##     repeat {

##         if (inspect) {
##             cat(thisGate, ": value list (iteration ", watchCount, "):\n", sep="");
##             showValueList(valueList, prefix="  ");
##             cat("-------------------------\n");
##         }


##         ## We have a masterOutput list here that has an output string from all
##         ## the gates that were run just now.  We need to use the connectionList
##         ## to copy the outputs into the valueList, so they can be inputs to the
##         ## next round.
##         for (gateName in names(masterOutput)) {
##             cat(thisGate, "copying gateName", gateName, "to: ");
##             for (outputName in names(masterOutput[[gateName]])) {
##                 outputQualifiedName <- paste(gateName, outputName, sep=".");
##                 cat("(", outputQualifiedName, ") ");
##                 valueList[[connectionList[[outputQualifiedName]]$sink]] <-
##                     masterOutput[[gateName]][[outputName]];
##             }
##             cat("\n");
##         }
##         cat(thisGate, "NEXT\n");

##         ## Check to see if all the values anticipated in the outList exist
##         ## in the valueList.  If they do, copy them to the outList, which
##         ## will be the return value.
##         missing <- FALSE;
##         for (outputName in names(outList)) {
##             if (outputName %in% names(valueList)) {
##                 outList[[outputName]] <- valueList[[outputName]];
##             } else {
##                 ## We're missing some values, so repeat.
##                 cat(thisGate, "can't find", outputName,"\n");
##                 missing <- TRUE;
##             }
##         }
##         ## Note the watchdog counter, just in case.
##         watchCount <- watchCount + 1;
##         if ((!missing) || (watchCount > tickMax)) break;
##     }

##     if (missing && (watchCount > tickMax))
##         cat("The gate did not produce all the necessary outputs.\n");
##     return(outList);
## }

## A 'gate' object has a list of input names and types and a 'definition'
## function to transform inputs into a list of one or more output values.
## The transformation function is either an R function or it is a
## collection of other gate objects, with a table indicating how the the
## input list feeds into those gates, and how they are hooked up to one
## another.
##
## A gate object also has a graphical representation, either as a single
## node with edges in and out, or a collection of nodes and edges.
gate <- function(gateName,
                 inputTypes,
                 definition,
                 connectionList=list(),
                 outList=c("out"),
                 color=1,
                 shape=1,
                 typeCatalog=gate.default.typeCatalog) {
    newGate <- list();
    gate.id.counter <<- gate.id.counter + 1; ## global
    newGate[["thisGate"]] <- gateName;
    newGate[["id"]] <- gate.id.counter;

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
    newGate[["inputTypes"]] <- structure(inputTypes, class="typeList");

    ## Define the output list.
    newGate[["outList"]] <- outList;

    ## This is a convenience function to say whether a list of output names
    ## is the complete list.
    newGate[["complete"]] <- function(outNames) {
        cat("*********************", gateName, "\n"); print(newGate[["outList"]]);
        print(outNames);
        complete <- TRUE;
        if (length(outNames) < 1) return(FALSE);
        for (neededName in names(newGate[["outList"]])) {
            if (!(neededName %in% names(outList))) {
                complete <- FALSE; ## We don't even have a needed name.
            } else if (is.null(newGate[["outList"]][[neededName]])) {
                complete <- FALSE; ## We don't have a needed value.
            }
        }
        return(complete);
    }

    newGate[["color"]] <- color;
    newGate[["shape"]] <- shape;

    class(newGate) <- "gate";

    ## The input definition can take two possible forms.  It is either a
    ## function (class='function'), that accepts an input list of
    ## name,value pairs and outputs a similar list at the other end, or it
    ## is an assembly of other gate objects (class='list').
    if (class(definition) == "function") {
        newGate[["type"]] <- "atomic";
        newGate[["gateList"]] <- NULL;
        newGate[["connectionList"]] <- NULL;
        ## We want to create the function with an arglist specified by
        ## inputTypes and do a certain amount of type checking before
        ## passing it to the input transformation.  The output value of a
        ## transform is a list of one or more labeled values.
        newGate[["transform"]] <-
            function(argList, inspect=FALSE, prefix="", tickMax=100) {
                if (!gate.checkTypes(argList=argList,
                                     typeCatalog=typeCatalog,
                                     inputTypes=inputTypes,
                                     inspect=inspect)) {
                    return(NULL);
                }
                return(definition(argList, newGate[["outList"]]));
            };
    } else if (class(definition) == "list") {
        newGate[["type"]] <- "composite";
        ## The transform is a list of other gates, hopefully accompanied by
        ## a connection list.
        newGate[["gateList"]] <- definition;
        newGate[["connectionList"]] <- connectionList;
        newGate[["transform"]] <-
            function(argList, inspect=FALSE, prefix="", tickMax=100) {
                if (!gate.checkTypes(argList=argList,
                                     typeCatalog=typeCatalog,
                                     inputTypes=inputTypes,
                                     inspect=inspect)) {
                    return(NULL);
                }
                return(gate.execute.iter(inputList=argList,
                                         gate=newGate,
                                         tickMax=4,
                                         inspect=inspect,
                                         prefix=prefix));
            };
    } else {
        cat("definition must be a function or a list.\n");
        stop();
    }

    ## The 'execute' method accepts a list of inputs that may not be
    ## complete, and returns a list of outputs that also may not be complete.
    ## This is what you get for one 'tick' of the clock.
    if (class(definition) == "function") {
        newGate[["execute"]] <- function(valueList, inspect=FALSE, prefix="") {
            ## We hope that the value list consists of only one item, a list
            ## of inputs and outputs for this particular transformation.
            valueList[[1]]$outputs <-
                newGate[["transform"]](valueList[[1]]$inputs,
                    inspect, prefix);
            return(valueList);
        };
    } else {
        ## This is a composite definition.  We need to find the 'this' data
        newGate[["execute"]] <- function(valueList, inspect=FALSE, prefix="") {
            return(gate.execute(valueList, newGate,
                                inspect=inspect, prefix=prefix))
        }
    }

    return(newGate);
}

print.gate <- function(g, prefix="") {

    cat(prefix, "type:", g$type, "\n");
    cat(prefix, "node ID:", g$id, "\n");
    cat(prefix, "inputTypes:", format.typeList(g$inputTypes), "\n");
    cat(prefix, "expected outputs: ");
    for (oname in g$outList) cat(oname, " ");
    cat("\n");

    if (g$type == "composite") {
        cat(prefix, "nodes: ");
        for (name in names(g$gateList)) cat(name, " ");
        cat("\n");

        cat(prefix, "connections:\n");
        for (name in names(g$connectionList))
            cat(prefix, "  ", name, "-->",
                format.connectionElement(g$connectionList[[name]]), "\n");
        cat(prefix, "\n");
    }

    cat(prefix, "color:", g$color, "\n");
    cat(prefix, "shape:", g$shape, "\n");
}


## The next few functions are meant for exporting the graph nature of the
## connection to a graph display and analysis package in R.  Using the
## "DiagrammeR" package, tre these:
##
## > igr <- create_graph()
## > nl <- gate.nodeList(test.COMP2gate)
## > el <- gate.edgeList(test.COMP2gate)
## > tl <- gate.simplify.edgeList(el, nl)
## > igr <- create_graph()
## > igr2 <- igr %>% add_nodes_from_table(table=nl, label_col=label)
## > igr3 <- igr2 %>% add_edges_from_table(table=tl, from_col=from, to_col=to, from_to_map=id_external)
## > render_graph(igr3, layout="tree")
##
## compiles a table of nodes for a gate for drawing them.  If the node is
## atomic, this is pretty simple.  If it is composite, the function is
## called recursively to work out the whole structure.
gate.nodeList <- function(g, prefix="", uid=1) {

    ## Start with an empty node list.
    out.nodeList <- data.frame(stringsAsFactors=FALSE);

    ## If we're at the top level, include the inputs as nodes, because
    ## we'll probably want to draw them, too.
    if (prefix == "") {
        for (iname in names(g$inputTypes)) {
            out.nodeList <- rbind(out.nodeList,
                                  data.frame(label=iname,id=uid,
                                             gid=g$id, type="IO",
                                             stringsAsFactors=FALSE));
            uid <- uid + 1;
        }
    }

    ## Check gate type
    if (g$type == "atomic") {
        ## This is an atomic gate, just return a single line with its data.
        out.nodeList <- rbind(out.nodeList,
                              data.frame(label=prefix, id=uid, gid=g$id, type="G",
                                         stringsAsFactors=FALSE));
        uid <- uid + 1;
    } else {
        ## This is a composite gate, and g$gateList is a list of other gates.

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
    }

    ## If we're at the top level, include the outputs as nodes, because
    ## we'll probably want to draw them, too.
    if (prefix == "") {

        for (oname in g$outList) {
            out.nodeList <- rbind(out.nodeList,
                                  data.frame(label=oname, id=uid,
                                             gid=g$id, type="IO",
                                         stringsAsFactors=FALSE));
            uid <- uid + 1;
        }
    }

    return(out.nodeList);
}

## Compile a table of edges for a gate, with 'from' and 'to' columns.  If a
## nodeList is included as an arg, use its 'id' column to reference the
## 'to' and 'from' columns created here.
gate.edgeList <- function(g, inspect=FALSE, prefix="") {

    ## Prepare an empty data frame to accumulate the connections.
    out.edgeList <- data.frame(stringsAsFactors=FALSE);

    showList <- function(l) {
        for (i in 1:dim(l)[1])
            cat(">>", i, l[i,"id"], l[i,"fromLabel"], l[i,"toLabel"], "\n");
    }

    ## Get the connections for any child nodes.
    for (gateName in names(g$gateList)) {
        if (inspect) cat("processing:", gateName, "\n");

        if (prefix == "") {
            out.edgeList <- rbind(out.edgeList,
                                  gate.edgeList(g$gateList[[gateName]],
                                                prefix=gateName));
        } else {
            out.edgeList <- rbind(out.edgeList,
                                  gate.edgeList(g$gateList[[gateName]],
                                                prefix=paste(prefix, gateName, sep=".")));
        }

    }

    if (inspect) showList(out.edgeList);

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
                            fromLabel=connectName,
                            toLabel=sinkNames,
                            stringsAsFactors=FALSE);
        } else {
            d <- data.frame(id=as.numeric(sinkIds),
                            fromLabel=paste(prefix, connectName, sep="."),
                            toLabel=paste(prefix, sinkNames, sep="."),
                            stringsAsFactors=FALSE);
        }

        out.edgeList <- rbind(out.edgeList, d);
        if (inspect) showList(out.edgeList);

    }
    return(out.edgeList);
}

## Sorts through the edge list and outputs an edge list that references the
## id field of the node list.
gate.simplify.edgeList <- function(edges, nodes) {

    ## A little function to get rid of the suffix, if this is a compound
    ## string, like 'a.b.c'.  Strings without a '.' are untouched.
    dropLast <- function(label) {
        tmpArray <- strsplit(label, "\\.")[[1]];
        if (length(tmpArray) > 1) {
            return(paste(tmpArray[1:(length(tmpArray) - 1)], collapse="."));
        } else {
            return(label);
        }
    }

    ## Create a couple of columns with the suffix removed from the labels.
    toShort <- edges$toLabel;
    fromShort <- edges$fromLabel;

    for (i in 1:length(edges$fromLabel)) {
        fromShort[i] <- dropLast(edges$fromLabel[i]);
        toShort[i] <- dropLast(edges$toLabel[i]);

    }
    edges <- cbind(edges, fromShort, toShort);



    ## Get the easy matches out of the way with mdply.
    findId <- function(id, fromLabel, toLabel, fromShort, toShort) {

        fromId <- 0;
        toId <- 0;

        for (i in 1:length(nodes$label)) {
            if (grepl(paste("^", nodes$label[i], "$", sep=""), fromShort)) {
                fromId <- nodes$id[i];
            }

            if (grepl(paste("^", nodes$label[i], "$", sep=""), toShort)) {
                toId <- nodes$id[i];
            }
        }
        return(data.frame(from=fromId, to=toId));
    }

    edges <- mdply(edges, findId);


    ## By this point, we've got most of them, and the ones we don't have
    ## are marked with a zero.

    ## A little recursive function to follow the 'from' nodes backwards.
    ## We go backwards because there's no forking in that direction.  Once
    ## these are found, I believe the forwards links left over are
    ## redundant.
    findOriginId <- function(fromString, edgeList, nodeList) {

        if (length(fromString) == 0) return(0);

        ## grepl returns an array of T/F. "sum" makes an ok "or".
        if (sum(grepl(paste("^", fromString, "$", sep=""), nodeList$label))) {

            ## If we're here, we've found the string in the node list...
            fromLoc <- grep(paste("^", fromString, sep=""), nodeList$label);

            ## So return the corresponding index.
            return(nodeList$id[fromLoc]);

            ## If we haven't found it, try looking without the suffix.
        } else if (sum(grepl(paste("^", dropLast(fromString), "$", sep=""),
                             nodeList$label))) {

            fromLoc <- grep(paste("^", dropLast(fromString), sep=""),
                            nodeList$label);
            return(nodeList$id[fromLoc]);
        } else {
            ## We didn't find it in the nodelist, so grab the corresponding
            ## "to" and try again, recursively.
            toLoc <- grep(paste("^", fromString, sep=""), edgeList$toLabel);

            if (length(toLoc) == 1) {

                return(findOriginId(edgeList$fromLabel[toLoc],
                                    edgeList, nodeList));

            } else if (length(toLoc > 1)) {
                cat("Your edge list is pathological.  An output can connect to\n",
                    "multiple inputs, but an input cannot connect to multiple\n",
                    "outputs.\n");
                stop();
            }
        }
    }

    ## We have all the matches, except that lots of the "from" indications
    ## have to be traced back to their origin.
    for (i in 1:length(edges$from)) {
        ## If it's a plain number, no need.
        if (edges$from[i] == 0) {

            ## Find this in the to list and fix the from.
            edges$from[i] <- findOriginId(edges$fromLabel[i], edges, nodes);

        }
    }

    return(edges[(edges$to!=0),c("id","from","to")]);
}



## TBD:
## Should add color and shape info to node and edge lists.
## (Also add color to edge defs.)



test.inlist <- list(in1="0",in2="1", in3="1");
test.intlist <- list(in1="binary", in2="binary", in3="binary",
                     in4="binary", in5="binary", in6="binary");

test.clist <- connectionList("in1","AND.in1");
test.clist <- append.connectionList(test.clist, "in1", "AND1.in1");
test.clist <- append.connectionList(test.clist, "in2", "AND1.in2,AND2.in1");
test.clist <- append.connectionList(test.clist, "in3", "AND2.in2");
test.clist <- append.connectionList(test.clist, "AND1.out", "OR3.in1");
test.clist <- append.connectionList(test.clist, "AND2.out", "OR3.in2");
test.clist <- append.connectionList(test.clist, "OR3.out", "out");

test.ANDfun <- function(inlist, outlist) {
    out <- TRUE;
    for (l in inlist) out <- out && (l == "1");
    return(list(out=(if (out) "1" else "0")));
}
test.ORfun <- function(inlist, outlist) {
    out <- FALSE;
    for (l in inlist) out <- out || (l == "1");
    return(list(out=(if (out) "1" else "0")));
}
test.ORgate <- gate("OR", list(in1="binary",in2="binary"), test.ORfun);
test.ANDgate <- gate("AND", list(in1="binary",in2="binary"), test.ANDfun);
test.glist <- list(AND1=test.ANDgate, AND2=test.ANDgate, OR3=test.ORgate);
test.olist <- c("out");

test.COMPgate <- gate("C1", input=test.intlist[1:3],
                      conn=test.clist,
                      definition=test.glist,
                      out=test.olist)

test.glist2 <- list(AND1=test.ANDgate, C1=test.COMPgate, OR1=test.ORgate)
test.clist2 <- connectionList("in1", "AND1.in1,C1.in1");
test.clist2 <- append.connectionList(test.clist2, "in2", "AND1.in2,C1.in2");
test.clist2 <- append.connectionList(test.clist2, "in3","C1.in3");
test.clist2 <- append.connectionList(test.clist2, "C1.out","OR1.in1");
test.clist2 <- append.connectionList(test.clist2, "AND1.out","OR1.in2");
test.clist2 <- append.connectionList(test.clist2, "OR1.out","out");

test.COMP2gate <- gate("C2", input=test.intlist[1:3],
                       conn=test.clist2,
                       definition=test.glist2,
                       out=test.olist)

test.glist3 <- list(AND1=test.ANDgate, OR4=test.ORgate, OR5=test.ORgate, C2=test.COMP2gate, C1=test.COMPgate, AND2=test.ANDgate, AND3=test.ANDgate);
test.clist3 <- connectionList("in1", "AND1.in1,C2.in1");
test.clist3 <- append.connectionList(test.clist3, "in2", "AND1.in2,C2.in2")
test.clist3 <- append.connectionList(test.clist3, "in3", "OR4.in1,C2.in3")
test.clist3 <- append.connectionList(test.clist3, "in4", "OR4.in2")
test.clist3 <- append.connectionList(test.clist3, "OR4.out", "OR5.in1")
test.clist3 <- append.connectionList(test.clist3, "AND1.out", "AND2.in1,OR5.in2,C1.in1")
test.clist3 <- append.connectionList(test.clist3, "in5", "C1.in2")
test.clist3 <- append.connectionList(test.clist3, "C2.out", "C1.in3")
test.clist3 <- append.connectionList(test.clist3, "C1.out", "AND3.in1")
test.clist3 <- append.connectionList(test.clist3, "OR5.out", "AND2.in2")
test.clist3 <- append.connectionList(test.clist3, "AND2.out", "AND3.in2")
test.clist3 <- append.connectionList(test.clist3, "AND3.out", "out1")

test.COMP3gate <- gate("C3", input=test.intlist[1:5],
                       conn=test.clist3,
                       definition=test.glist3,
                       out=c("out1"));


#test.glist4 <- list(AND1=test.ANDgate, C3=test.COMP3gate

test.ilist <- list(this=gate.io(inputs=list("in1"="0","in2"="1")),
                   "C1"=gate.io(inputs=list("i1"="a","i2"="b", "i3"="c"),
                                outputs=list("out1"="","out2"="")),
                   "C2"=gate.io(inputs=list("in1"="1")),
                   "C2.C3"=gate.io(inputs=list("in1"="f","in2"="g")));

ts <- gate.io(inputs=list("in1"="0"))
ts <- gate.io.set(list(this=ts), "in2", "1")
ts <- gate.io.set(ts, "in3", "1")
ts <- gate.io.set(ts, "in4", "0")
