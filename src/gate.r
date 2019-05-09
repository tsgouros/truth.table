## A system for simulating an arbitrary arrangement of arbitrary 'gate'
## functions that accept inputs and produce outputs.

## Some generic functions that will be used by our objects.

## A 'format' function to be just a print function that works well with
## others.  Usually just the same as 'print' without the final \n.
setGeneric(name="formatVal",
           def=function(object, ...) {
               standardGeneric("formatVal");
           });

## For types, checks to see if a value is a type.
setGeneric(name="check",
           def=function(object,testVal) {
               standardGeneric("check");
           });

## Adds values ot the various objects.
setGeneric(name="add",
           def=function(object, ...) {
               standardGeneric("add");
           });

## A generic accessor.
setGeneric(name="getVal",
           def=function(object, ...) {
               standardGeneric("getVal");
           });

## A generic setter.
setGeneric(name="setVal",
           def=function(object, ...) {
               standardGeneric("setVal");
           });

## We begin by establishing a system of 'types' with which to characterize
## the inputs and outputs of the gates.  There are 'symbol' types, which can
## assume one of several possible values, 'integer' types that can assume
## integer values within a range, and 'float' types that can assume floating
## point values within a range.
type <- setClass(
    "type",
    slots = c(baseType="character",
              range="character",
              min="numeric",
              max="numeric"),
    prototype=c(baseType="symbol",
                range=c("0", "1"),
                min=0, max=1),
    validity = function(object) {

        if (!(object@baseType %in% c("symbol", "integer", "float")))
            return("Not a valid baseType.");

        if (object@baseType == "symbol") {
            if (length(object@range) < 1) return("Need a range of values.");
        } else {
            if (object@max <= object@min) return("Min and max not correct.");
        }
        return(TRUE);
    })

setMethod("formatVal",
          signature="type",
          definition=function(object) {
              if (object@baseType == "symbol") {
                  return(paste(object@baseType, ": ",
                               paste(object@range, collapse="/"), sep=""));
              } else {
                  return(paste(object@baseType, ": min: ", object@min,
                               ", max: ", object@max, sep=""));
              }
          })

setMethod("show",
          signature="type",
          definition=function(object) {
              cat(formatVal(object), "\n");
          })

## Use 'check(binary, "0")' to see whether a value is an acceptable member of
## the given type.  We use NULL as a placeholder, so it counts as a valid
## member of any type.
setMethod("check",
          signature="type",
          definition=function(object, testVal) {
              if (is.null(testVal)) return(TRUE);
              if (object@baseType == "symbol") {
                  if (class(testVal) != "character") return(FALSE);
                  if (!(testVal %in% object@range)) return(FALSE);
              } else if (object@baseType == "integer") {
                  if (class(testVal) != "numeric") return(FALSE);
                  if ((testVal < object@min) || (testVal > object@max))
                      return(FALSE);
                  if (testVal != round(testVal)) return(FALSE);
              } else if (object@baseType == "float") {
                  if (class(testVal) != "numeric") return(FALSE);
                  if ((testVal < object@min) || (testVal > object@max))
                      return(FALSE);
                  if (testVal != round(testVal)) return(FALSE);
              }
              return(TRUE);
          })

binary <- type(baseType="symbol", range=c("0","1"));
integer <- type(baseType="integer", min=0, max=100);
float <- type(baseType="float", min=0.0, max=1.0);

## Now we define a class that is just a list of types.  This is not a
## terribly useful class, mostly exists only so that we can use the validity
## check to assess whether such a list is valid.
typeList <- setClass(
    "typeList",
    slots = c(data="list"),
    prototype=c(data=list()),
    validity = function(object) {

        if (class(object@data) != "list") return("Not a valid list.");

        if (length(object@data) > 0) {

            for (i in 1:length(object@data)) {
                if (names(object@data)[i] == "")
                    return("All entries in the type list need a name.");

                if (class(object@data[[i]]) != "type")
                    return("Object in list is not a type.");

                if (!validObject(object@data[[i]]))
                    return("Invalid type object in list.");
            }
        }

        return(TRUE);
    })

setMethod("initialize",
          signature="typeList",
          definition=function(.Object, ...) {

              args <- list(...);
              for (name in names(args)) {
                  .Object@data[[name]] <- args[[name]];
              }
              validObject(.Object);
              return(.Object);
          });

setMethod("add",
          signature="typeList",
          definition=function(object, ...) {
              args <- list(...);
              print(args);
              for (name in names(args)) {
                  object@data[[name]] <- args[[name]];
              }
              return(object);
          });

setMethod("formatVal",
          signature="typeList",
          definition=function(object) {
              out <- "";
              for (name in names(object))
                  out <- paste(out, name, "(",
                               formatVal(object[[name]]), ") ", sep="");
              return(out);
          })

setMethod("show",
          signature="typeList",
          definition=function(object) {
              for (name in names(object))
                  cat(name, "(",
                      formatVal(object[[name]]), ") ", sep="", "\n");
          })

## A couple of things to make a typeList act more like a list.
setMethod("[[",
          signature="typeList",
          definition=function(x, i, j, ...) { return(x@data[[i]]); });

setMethod("length",
          signature="typeList",
          definition=function(x) { return(length(x@data)); });

setMethod("names",
          signature="typeList",
          definition=function(x) { return(names(x@data)); });

## We define connections among the gates with a cnxnList of cnxnElements
## specifying a set of "sinks" for each "source".  Connections can be
## one-source-to-one-sink or one-source-to-many-sinks, but not
## many-sources-to-one-sink.
##
## A cnxnElement is a connection between some source and one or more sink.
## The cnxnElement really just has the sinks, since the source is in the key
## of the cnxnList.  We are using an object here so that we can eventually
## use it to hold graphic and other information about the connection.
cnxn.id.counter <- 0;
cnxnElement <- setClass(
    "cnxnElement",
    slots = c(sink="list", color="vector", weight="numeric"),
    prototype = c(sink=list(), color=vector("numeric",3), weight=1.1),
    validity = function(object) {

        print(object@color);
        ## Empty sink lists are ok.
        if (length(object@sink) < 1) return(TRUE);

        for (i in 1:length(object@sink)) {
            if (names(object@sink)[i] == "")
                return("Need a name for each entry.");
            if ((class(object@sink[[i]]) != "numeric") ||
                (object@sink[[i]] != round(object@sink[[i]])))
                return(paste("Bad ID for connection", names(object@sink)[i]));
        }
        return(TRUE);
    })


## A cnxnElement is initialized like this: cnxnElement("C2:in1", "C1:in2") or
## cnxnElement(c("C2:in1", "C2:in2")).  Either will work.
setMethod("initialize",
          signature="cnxnElement",
          definition=function(.Object, ...) {
              for (item in list(...)) {
                  for (subitem in item) {
                      cnxn.id.counter <<- cnxn.id.counter + 1;
                      .Object@sink[[subitem]] <- cnxn.id.counter;
                  }
              }
              validObject(.Object);
              ## This is magic for adjusting the global variable.
              assign("cnxn.id.counter", cnxn.id.counter, envir = .GlobalEnv)
              return(.Object);
          });

setMethod("formatVal",
          signature="cnxnElement",
          definition=function(object) {
              out <- c();
              if (length(object@sink) < 1) return("");
              for (i in 1:length(object@sink)) {
                  out <- c(out, paste(names(object@sink)[i],
                                      "(", object@sink[[i]], ")", sep=""));
              }
              return(paste(out, collapse=", "));
          });

setMethod("show",
          signature="cnxnElement",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("add",
          signature="cnxnElement",
          definition=function(object, ...) {
              args <- list(...);
              for (i in 1:length(args)) {

                  cnxn.id.counter <<- cnxn.id.counter + 1;
                  object@sink[[args[[i]] ]] <- cnxn.id.counter;
              }
              assign("cnxn.id.counter", cnxn.id.counter, envir = .GlobalEnv)
              return(object);
          });

## A cnxnList is a list of connections between sources and sinks.  The input
## specifications look like this: "<gate>:<name>" where <gate> is the name of
## a gate in the gate list and <name> is one of the inputs or outputs
## belonging to that gate.  e.g. "AND1:in1" and "OR3:in2" and "OR4:out".
cnxnList <- setClass(
    "cnxnList",
    slots=c(data="list"),
    prototype = c(data=list()),
    validity = function(object) {
        if (class(object@data) != "list")
            return("cnxnList must begin with a list.");
        if (length(object@data) == 0) return(TRUE);
        if (sum(sapply(object@data, function(x) {class(x) == "cnxnElement"})) !=
            length(object@data)) return("All elements must be cnxnElement");
        return(TRUE);
    });

## Initialize a cnxnList like this: cnxnList(source1, source2, ...)
setMethod("initialize",
          signature="cnxnList",
          definition=function(.Object, ...) {
              args <- list(...);
              for (i in 1:length(args)) {
                  ## If the current source isn't represented, add it.
                  if (!(names(args)[i] %in% names(.Object@data))) {
                      .Object@data[[names(args)[i]]] <- cnxnElement();
                  }

                  ## Separate the sinks.
                  sinks <- strsplit(args[[i]], split=",")[[1]];

                  for (s in sinks) {
                      .Object@data[[names(args)[i]]] <-
                          add(.Object@data[[ names(args)[i] ]], s);
                  }
              }
              validObject(.Object);
              return(.Object);

          });

setMethod("formatVal",
          signature="cnxnList",
          definition=function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              out <- c();
              for (i in 1:length(object@data)) {
                  out <- c(out, paste(names(object@data)[i],
                                      formatVal(object@data[[i]]),
                                      sep=" --> "));
              }
              return(paste(prefix, out, sep="", collapse="\n"));
          });

setMethod("show",
          signature="cnxnList",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

## A couple of things to make a cnxnList act more like a list.
setMethod("[[",
          signature="cnxnList",
          definition=function(x, i, j, ...) { return(x@data[[i]]); });

setMethod("length",
          signature="cnxnList",
          definition=function(x) { return(length(x@data)); });

setMethod("names",
          signature="cnxnList",
          definition=function(x) { return(names(x@data)); });

## For cnxnList, this is an 'add' method, that can accept stuff like
## "in1"="AND1.in1,AND2.in2"...  We refer to these pairs as sources and
## sinks.  The sinks can be multiple, separated by commas.
setMethod("add",
          signature="cnxnList",
          definition=function(object, ...) {
              args <- list(...);
              for (i in 1:length(args)) {
                  ## If the current source isn't represented, add it.
                  if (!(names(args)[i] %in% names(object@data))) {
                      object@data[[names(args)[i]]] <- cnxnElement();
                  }

                  ## Separate the sinks.
                  sinks <- strsplit(args[[i]], split=",")[[1]];

                  for (s in sinks) {
                      object@data[[names(args)[i]]] <-
                          add(object@data[[ names(args)[i] ]], s);
                  }
              }
              return(object);
          });


##   gateIO (holds inputs and outputs of a gate)
gateIO <- setClass(
    "gateIO",
    slots=c(inputs="list", outputs="list",
            inputTypes="typeList", outputTypes="typeList"),
    prototype = c(inputs=list(), outputs=list(),
                  inputTypes=typeList(), outputTypes=typeList()),
    validity = function(object) {
        if (class(object@inputs) != "list") return("inputs must be a list");
        if (class(object@outputs) != "list") return("outputs must be a list");
        if ("replace" %in% names(object@inputs))
            return("Please don't use 'replace' for an input name.");
        if ("replace" %in% names(object@outputs))
            return("Please don't use 'replace' for an output name.");

        if (class(object@inputTypes) != "typeList")
            return("inputTypes should be a typeList object.");
        if (length(object@inputs) != length(object@inputTypes@data))
            return("Must have a type for each input.");
        if (class(object@outputTypes) != "typeList")
            return("outputTypes should be a typeList object.");
        if (length(object@outputs) != length(object@outputTypes@data))
            return("Must have a type for each output.");

        ## Check all the values against the types.
        if (length(object@inputs) > 0) {
            for (i in 1:length(object@inputs)) {
                if (!check(object@inputTypes@data[[i]], object@inputs[[i]]))
                    return(paste("Bad type for argument",
                                 names(object@inputs)[i]));
            }
        }
        if (length(object@outputs) > 0) {
            for (i in 1:length(object@outputs)) {
                if (!check(object@outputTypes@data[[i]], object@outputs[[i]]))
                    return(paste("Bad type for argument",
                                 names(object@outputs)[i]));
            }
        }
        return(TRUE);
    });

setMethod("initialize",
          signature="gateIO",
          definition=function(.Object, ...) {
              args <- list(...);
              if ("inputs" %in% names(args)) {
                  .Object@inputs <- args$inputs;
              }
              if ("outputs" %in% names(args)) {
                  .Object@outputs <- args$outputs;
              }
              if ("inputTypes" %in% names(args)) {
                  .Object@inputTypes <- args$inputTypes;
              } else {
                  ## We are creating a default input binary type.
                  .Object@inputTypes <- typeList();
                  for (name in names(.Object@inputs)) {
                      .Object@inputTypes@data[[name]] <-
                          type(baseType="symbol", range=c("0","1"));
                  }
              }
              if ("outputTypes" %in% names(args)) {
                  .Object@outputTypes <- args$outputTypes;
              } else {
                  ## Output defaults to binary type, too.
                  .Object@outputTypes <- typeList();
                  for (name in names(.Object@outputs)) {
                      .Object@outputTypes@data[[name]] <-
                          type(baseType="symbol", range=c("0","1"));
                  }
              }

              validObject(.Object);
              return(.Object);
          });


setMethod("formatVal",
          signature="gateIO",
          definition=function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              outstr <- "";
              out <- c();
              if (length(object@inputs) > 0) {
                  for (i in 1:length(object@inputs)) {
                      out <- c(out, paste(names(object@inputs)[i], "=",
                                          object@inputs[[i]], " (",
                                          formatVal(object@inputTypes[[i]]),
                                          ")", sep=""));
                  }
                  outstr <- paste(prefix, "I: ",
                                  paste(out, collapse=", "), sep="");
              }

              if (length(object@outputs) == 0) return(outstr);

              out <- c();
              for (i in 1:length(object@outputs)) {
                  out <- c(out, paste(names(object@outputs)[i], "=",
                                      object@outputs[[i]], " (",
                                      formatVal(object@outputTypes[[i]]),
                                      ")", sep=""));
              }
              outstr <- paste(outstr, "\n", prefix, "O: ",
                              paste(out, collapse=", "), sep="");
              return(outstr);
          });

setMethod("show",
          signature="gateIO",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("getVal",
          signature="gateIO",
          definition=function(object, ...) {
              args <- list(...);
              valName <- args[[1]];

              ## The valName could refer to an item among the inputs or the
              ## outputs, so check both before giving up.
              if (valName %in% names(object@inputs)) {
                  return(object@inputs[[valName]]);
              } else if (valName %in% names(object@outputs)) {
                  return(object@outputs[[valName]]);
              } else {
                  return(NULL);
              }
          });

## A convenience function.
isOutputName <- function(vname) {
    return(grepl("out|res", vname));
}

## Call like this: g <- setVal(g, "in1"="1", type=binary, replace=FALSE).
## The 'replace' arg FALSE means if you don't find the name, add it to the
## object.  If TRUE, the setter will fail if the name doesn't already exist.
## You can set multiple values, but please don't name any of the inputs or
## outputs 'replace' or 'type'.
setMethod("setVal",
          signature="gateIO",
          definition=function(object, ...) {
              args <- list(...);

              replace <- FALSE;
              if ("replace" %in% names(args)) replace <- args[["replace"]];

              ## Default type is binary.
              if ("type" %in% names(args)) {
                  thisType <- args[["type"]];
              } else {
                  thisType <- type(baseType="symbol", range=c("0", "1"));
              }

              for (name in names(args)) {
                  if (name == "replace") next;
                  if (name == "type") next;

                  if (replace &&
                      (!(name %in% names(object@inputs))) &&
                      (!(name %in% names(object@outputs)))) {
                      cat(name, "is not already in object.\n");
                      stop();
                  } else {
                      if (isOutputName(name)) {
                          if (name %in% names(object@inputs)) {
                              if (check(object@outputTypes[[name]],
                                        args[[name]])) {
                                  object@outputs[[name]] <- args[[name]];
                              } else {
                                  cat(name, "has a bad value.\n");
                                  stop();
                              }
                          } else {
                              if (check(thisType, args[[name]])) {
                                  object@inputTypes@data[[name]] <- thisType;
                                  object@inputs[[name]] <- args[[name]];
                              } else {
                                  cat(name, "is not a good value.\n");
                                  stop();
                              }
                          }
                      } else {
                          if (name %in% names(object@inputs)) {
                              if (check(object@inputTypes[[name]],
                                        args[[name]])) {
                                  object@inputs[[name]] <- args[[name]];
                              } else {
                                  cat(name, "has a bad value.\n");
                                  stop();
                              }
                          } else {
                              if (check(thisType, args[[name]])) {
                                  object@inputTypes@data[[name]] <- thisType;
                                  object@inputs[[name]] <- args[[name]];
                              } else {
                                  cat(name, "is not a good value.\n");
                                  stop();
                              }
                          }
                      }
                  }
              }
              return(object);
          });

## You can use 'add' to add an input or output to a gate without a value.
## Try 'g <- add(g, "out", type=binary)'. Still needs a type, and the default
## is binary.
setMethod("add",
          signature = "gateIO",
          definition= function(object, ...) {
              args <- list(...);

              ## Default type is binary.
              if ("type" %in% names(args)) {
                  thisType <- args[["type"]];
              } else {
                  thisType <- type(baseType="symbol", range=c("0", "1"));
              }

              if (isOutputName(args[[1]])) {
                  object@outputs[[ args[[1]] ]] <- list(NULL);
                  object@outputTypes@data[[ args[[1]] ]] <- thisType;
              } else {
                  object@inputs[[ args[[1]] ]] <- list(NULL);
                  object@inputTypes@data[[ args[[1]] ]] <- thisType;
              }
              return(object);
          });

## A gateIOList records the inputs and outputs of a gate, as well as its
## status quo.
gateIOList <- setClass(
    "gateIOList",
    slots = c(data="list"),
    prototype = c(data=list()),
    validity = function(object) {
        if (class(object@data) != "list")
            return("gateIOList must begin with a list.");
        if (sum(sapply(object@data, function(x) {class(x) == "gateIO"})) !=
            length(object@data)) return("All elements must be gateIO.");
        return(TRUE);
    });

setMethod("initialize",
          signature = "gateIOList",
          definition= function(.Object, ...) {
              args <- list(...);

              ## Create a 'this' object.
              .Object@data[["this"]] <- gateIO();

              for (i in 1:length(args)) {
                  if (names(args)[i] == "") {
                      .Object@data[["this"]] <- args[[i]];
                  } else {
                      .Object@data[[names(args)[i]]] <- args[[i]];
                  }
              }

              validObject(.Object);
              return(.Object);
          });

setMethod("formatVal",
          signature = "gateIOList",
          definition= function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              outstr <- "";
              out <- c();

              ## We want to get the 'this' element first.
              outstr <- paste(prefix, "this:\n", sep="");
              outstr <- paste(outstr,
                              formatVal(object@data[["this"]],
                                        prefix=paste(prefix, " ", sep="")),
                              sep="");

              for (i in 1:length(object@data)) {
                  if (names(object@data)[i] == "this") next;

                  outstr <- paste(outstr, "\n\n",
                                  prefix, names(object@data)[i],
                                  ":\n", sep="");
                  outstr <-
                      paste(outstr,
                            formatVal(object@data[[names(object@data)[i] ]],
                                      prefix=paste(prefix, " ", sep="")),
                            sep="");
              }

              return(outstr);
          });

setMethod("show",
          signature="gateIOList",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

## The things to make a gateIOList act more like a list.
setMethod("[[",
          signature="gateIOList",
          definition=function(x, i, j, ...) { return(x@data[[i]]); });

setMethod("[[<-",
          signature="gateIOList",
          definition=function(x, i, j, ..., value) {
              if (class(value) == "gateIO") {
                  x@data[[i]] <- value;
              } else {
                  cat("value must be a gateIO object.\n");
                  stop();
              }
              return(x);
          });

setMethod("length",
          signature="gateIOList",
          definition=function(x) { return(length(x@data)); });

setMethod("names",
          signature="gateIOList",
          definition=function(x) { return(names(x@data)); });


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
## to "AND1:in1" or "C2:in2" or whatever.  Then the AND1 and C2 gates are
## executed with whatever inputs are available.  Upon execution of AND1, its
## inputs are renamed from "AND1:in1" to simply "in1", which is how the AND1
## gate thinks of it.  Its output value list has names like "out" or "out1"
## or whatever, and when reincorporating those values into the parent list,
## they are prefixed with "AND1".  So:
##
##  "AND1:in1" -> "in1" -> execute AND1 gate -> "out" -> "AND1:out"


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

## A 'type list' is a list of names and type objects to go with them.  The
## inputTypes used in the gate() constructor is a typeList.
## typeList <- function(...) {}

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

## This would be a better constructor if we could do it like this:
##   connectionList("in1"="AND1.in1", "in2"="AND1.in2",...)
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

    ## If the arglist is a gateIOorig object, just get the inputs list.
    if (class(argList) == "gateIOorig") argList <- argList$inputs;

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
gateIOorig <- function(inputs=list(), outputs=list("out"=NULL)) {

    if (class(inputs) != "list") {
        cat("inputs must be a list of input names and values.\n");
        stop();
    }

    if (class(outputs) != "list") {
        cat("outputs should be a list of output names and NULL values.\n");
        stop();
    }

    out <- list(inputs=inputs, outputs=outputs);

    ## A couple of convenience function.
    out[["is.output"]] <- function(outStr) {
        return(grepl("out|res", outStr));
    }

    out[["add"]] <- function(...) {
        ## We assume an argument list like "in1"="0","in2"="1",...
        argList <- list(...);

        for (i in 1:length(argList)) {
            if (out$is.output(names(argList)[i])) {
                out$outputs[[names(argList)[i] ]] <- argList[[i]];
            } else {
                out$inputs[[names(argList)[i] ]] <- argList[[i]];
            }
        }

        return(structure(out, class="gateIOorig"));
    }

    return(structure(out, class="gateIOorig"));
}

## This does double duty for printing gateIOorig objects, and also for lists of
## such objects.  Though of course it is only summoned automatically for the
## former.
print.gateIOorig <- function(gio, prefix="") {

    if (class(gio) == "gateIOorig") {

        cat(prefix, "inputs:\n", prefix, "  ", sep="");
        if (class(gio$inputs) == "list") {
            for (name in names(gio$inputs))
                cat(name, "=", gio$inputs[[name]], " ", sep="");
            cat("\n");
        } else {
            cat(prefix, "Malformed gateIOorig object, input list seems to be:");
            print(gio$inputs);
            stop();
        }
        cat(prefix, "outputs:\n", prefix, "  ", sep="");
        if (class(gio$outputs) == "list") {
            for (name in names(gio$outputs))
                cat(name, "=", gio$outputs[[name]], " ", sep="");
            cat("\n");
        } else {
            cat(prefix, "Malformed gateIOorig object, output list seems to be:");
            print(gio$outputs);
            stop();
            print.gateIOorig(gio$outputs, prefix=paste("|", prefix));
        }
    } else if (class(gio) == "list") {

        for (gname in names(gio)) {
            cat(prefix, gname, ": \n", sep="");
            print.gateIOorig(gio[[gname]], prefix=paste(prefix, "| ", sep=""));
        }

    } else {
        cat(prefix, "Looking for gateIOorig.",
            "Don't know how to print object of class", class(gio), "\n");
        print(gio);
        stop();
    }
}

## An object to contain a list of gateIOorig objects.  This is used to hold the
## "state" of a complicated gate or a system of gates.  The structure of a
## complicated gate's gioList is flat, but there is a hierarchy to the names.
## There is a get and set method used to return or add entries.  Entries look
## like this: "C1.C3:in3" which would point to one of the inputs for the
## C1.C3 gate.
##
## There is a special "this" entry in the list which is used to interpret
## unqualified entries like "in3" and "out2".
##
## Note that though the class provides a "set()" method and "append()", R
## doesn't really do that kind of thing, so you have to use this syntax:
##    gl <- gl$append("C1.C2"=gateIOorig(inputs=list("in1"="0","in2"="1")))
##    gl <- gl$set("C1.C2:in2"="0")
gioList <- function(...) {
    ## The input to this constructor should be one or more gateIOorig objects, or
    ## a list of such objects.  You can also use a collection of name-value
    ## pairs, in which case they are coerced to be input and output entries
    ## to the 'this' entry.
    argList <- list(...);
    out <- list();
    out[["data"]] <- list();

    ## If the first arg is a gioList, this is an append operation.
    if (class(argList[[1]]) == "gioList") {
        out$data <- argList[[1]];
    }

    ## Sort through each item in the argument list and put it into place in
    ## the output list.
    for (i in start:length(argList)) {

        cat(">>>>->", names(argList)[i], "->", class(argList[[i]]), "<<\n", sep="");
        if ((names(argList)[i])=="") cat("NULL value\n");

        ## If this is a gateIOorig object, just add it.
        if (class(argList[[i]]) == "gateIOorig") {

            if (names(argList)[i] == "") {
                ## There is no name for this object, so make it 'this'.
                out$data[["this"]] <- argList[[i]];
            } else {
                out$data[[names(argList)[i] ]] <- argList[[i]];
            }

        } else {
            ## Either this is an argument like "in2" or like "in2"="0".

            ## If there isn't a 'this' entry, create one.
            if (is.null(out$data$this))
                out$data[["this"]] <- gateIOorig();

            if (names(argList)[i] == "") {
                ## Like "in2"
                if (class(argList[[i]]) == "character") {
                    ## Construct an argument list.
                    al <- list();
                    al[[argList[[i]]]] <- NULL;

                    out$data$this <- do.call(out$data$this$add, al);
                } else {
                    cat("I don't know how to parse :", argList[[i]]);
                    stop();
                }
            } else {
                ## Construct an argument list.
                al <- list();
                al[[names(argList)[i]]] <- argList[[i]];

                out$data$this <- do.call(out$data$this$add, al);
            }
        }
    }

    return(out);
}

## This function accepts a dot-separated name like "C2.C1.AND1:in1" and a
## list and returns the value indicated.  The hierarchy is in the name, so we
## only need to separate at the suffix, match the gate name, and find the
## suffix value among the inputs or outputs of the gateIOorig object.
gateIOorig.get <- function(gioList, name, inspect=FALSE, prefix="") {
    ## gioList is either a list of gateIOorig objects, or a single gateIOorig
    ## object.  name is either (1) a name like "C2.C1.AND1:in1" or (2) an
    ## atomic name like "in1".
    if (inspect) {
        cat(prefix, "gateIOorig.get searching for", name, "in\n");
        print.gateIOorig(gioList, prefix=paste("|", prefix));
    }

    ## Check for suffix
    if (grepl(":", name)) {
        nameSplit <- strsplit(name, ":")[[1]];
        targetGate <- nameSplit[1];
        targetFeature <- nameSplit[2];

        if (targetGate %in% names(gioList)) {
            return(gateIOorig.get(gioList[[targetGate]], targetFeature,
                               inspect=inspect, prefix=paste("|", prefix)));
        } else {
            if (inspect) cat(prefix, "gateIOorig.get: Can't find",
                             targetGate, "\n");
            return(NULL);
        }
    } else {
        ## No suffix, so hopefully the name is something like "in1" and the
        ## gioList is either a single gateIOorig object or a list, in which case
        ## we need to look in the 'this' object.  So this messy if statement
        ## covers both cases.
        if (class(gioList) == "gateIOorig") {
            ## First case (gioList is a gateIOorig object)

            if (name %in% names(gioList$inputs)) {
                ## Is it among the inputs?
                if (inspect) cat(prefix, "found it among the inputs.\n");
                return(gioList$inputs[[name]]);

            } else if (name %in% names(gioList$outputs)) {
                ## Or the outputs?
                if (inspect) cat(prefix, "found it among the outputs.\n");
                return(gioList$outputs[[name]]);

            } else {
                if (inspect)
                    cat(prefix, "gateIOorig.get:", name,
                        "does not appear to be in this IO list.\n");
                return(NULL);
            }

        } else {
            ## Second case (we're looking in the this element).
            if (name %in% names(gioList[["this"]]$inputs)) {
                if (inspect) cat(prefix, "it was an input.\n");
                return(gioList[["this"]]$inputs[[name]]);

            } else if (name %in% names(gioList[["this"]]$outputs)) {
                if(inspect) cat(prefix, "it was an output.\n");
                return(gioList[["this"]]$outputs[[name]]);

            } else {
                if (inspect)
                    cat(prefix, "gateIOorig.get:", name,
                        "does not appear to be in this IO list.\n");
                return(NULL);
            }
        }
    }
}

## Like gateIOorig.get, but this sets a value within a list of gateIOorig objects
## and returns the modified gateIOorig list.  If you try to set something that
## does not exist, it is created for you.  If you don't want that behavior,
## set the 'modify' arg to FALSE.
##
## There's a certain amount of guessing done on the names you're using, so if
## your outputs are not named "out*" or "result" or something like that, it
## might not work.  Or if you name an input "out", you're asking for trouble.
gateIOorig.set <- function(gioList, name, newVal, modify=TRUE,
                        inspect=FALSE, prefix="") {
    ## gioList is either a list of gateIOorig objects, or a single gateIOorig
    ## object.  name is either (1) a name like "C2.C1.AND1:in1" or (2) an
    ## atomic name like "in1".
    if (inspect) {
        cat(prefix, "gateIOorig.set searching for", name,
            " to set it to ->", newVal, ". List:\n");
        print.gateIOorig(gioList, prefix=paste("|", prefix));
    }

    if (grepl(":", name)) {
        nameSplit <- strsplit(name, ":")[[1]];
        targetGate <- nameSplit[1];
        targetFeature <- nameSplit[2];

        if (targetGate %in% names(gioList)) {
            gioList[[targetGate]] <-
                gateIOorig.set(gioList[[targetGate]], targetFeature, newVal,
                            inspect=inspect, prefix=paste("|", prefix));
            return(gioList);

        } else {
            if (inspect) cat(prefix, "gateIOorig.set: Can't find",
                             targetGate, "\n");
            if (!modify) {
                cat(prefix, "gateIOorig.set: Can't find", targetGate,
                    "stopping.\n");
                stop();
            }

            target <- list();
            target[[targetFeature]] <- newVal;
            ## We assume that a never-before-noticed gate has its inputs
            ## established first.  We may reassess if this error is hit.
            if (grepl("out|res", targetFeature)) {
                cat(prefix, "Surprise! new gate", targetGate,
                    "wants to start with", targetFeature, "\n");
                stop();
            }
            gioList[[targetGate]] <- gateIOorig(inputs=target);
            return(gioList);
        }
    } else {
        ## No suffix, so hopefully the name is something like "in1" and the
        ## gioList is either a single gateIOorig object or a list, in which case
        ## we need to look in the 'this' object.  So this messy if statement
        ## covers both cases.
        if (class(gioList) == "gateIOorig") {
            ## First case (gioList is a gateIOorig object)

            ## Is this an output sort of a name?
            if (grepl("out|res", name)) {

                if (!modify) {
                    if (!(name %in% names(gioList$outputs))) {
                        if (inspect) {
                            cat(prefix, name, "not present in ");
                            print.gateIOorig(gioList, prefix=prefix);
                        }
                        return(gioList);
                    }
                }
                gioList$outputs[[name]] <- newVal;

            } else {
                ## If not, we assume it's an input sort of a name.
                if (!modify) {
                    if (!(name %in% names(gioList$inputs))) {
                        if (inspect) {
                            cat(prefix, name, "not present in ");
                            print.gateIOorig(gioList, prefix=prefix);
                        }
                        return(gioList);
                    }
                }
                gioList$inputs[[name]] <- newVal;
            }

            return(gioList);
        } else {
            ## Second case (gioList is a list, so we need the 'this' entry.

            ## Is this an output sort of a name?
            if (grepl("out|res", name)) {

                if (!modify) {
                    if (!(name %in% names(gioList$this$outputs))) {
                        if (inspect) {
                            cat(prefix, name, "not present in ");
                            print.gateIOorig(gioList$this, prefix=prefix);
                        }
                        return(gioList);
                    }
                }
                gioList$this$outputs[[name]] <- newVal;

            } else {
                ## If not, we assume it's an input sort of a name.
                if (!modify) {
                    if (!(name %in% names(gioList$this$inputs))) {
                        if (inspect) {
                            cat(prefix, name, "not present in ");
                            print.gateIOorig(gioList$this, prefix=prefix);
                        }
                        return(gioList);
                    }
                }
                gioList$this$inputs[[name]] <- newVal;
            }

            return(gioList);
        }
    }
}

## Work through the valueList and use the connectionList to copy values
## where they belong.  The connection name is the source, the value the
## sink, which may have multiple entries.  The connectionlist has entries
## like this: cL[["in1"]]$sink = "AND1:in1".  Multiple connections can be
## listed in a comma-separated list like this: "AND1:in1,AND2:in2". No
## spaces at the commas.
gate.makeConnections <-
    function(valueList, connectionList, inspect=FALSE, prefix="") {

    for (cname in names(connectionList)) {

        if (inspect) {
            cat(prefix, "Checking ", cname, " against \n");
            print.gateIOorig(valueList, paste("|", prefix));
            cat(prefix, "----------------------------\n");
        }

        ## Need to move this value:
        newInput <- gateIOorig.get(valueList, cname, inspect);

        if (!is.null(newInput)) {

            for (entry in strsplit(connectionList[[cname]]$sink, ",")[[1]]) {
                ## To here:
                valueList <- gateIOorig.set(valueList, entry, newVal=newInput,
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
## The input is a gateIOorig object or a list of gateIOorig objects with one of
## them identified as 'this'.  The output is exactly the same thing, with
## values added and updated as necessary, so that this function can be run
## multiple times to 'tick' the values through the gate.  See
## gate.execute.iter, below.
gate.execute <- function(inputList, gate, inspect=FALSE, prefix="") {
    ## The inputList is either a single gateIOorig object, or a list of them.
    ## If it's a list, the special keyword "this" is used to indicate the
    ## inputs and outputs to this particular gate.
    if (class(inputList) == "gateIOorig") {
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
                print.gateIOorig(valueSubList, prefix);
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

test.clist <- connectionList("in1","AND:in1");
test.clist <- append.connectionList(test.clist, "in1", "AND1:in1");
test.clist <- append.connectionList(test.clist, "in2", "AND1:in2,AND2:in1");
test.clist <- append.connectionList(test.clist, "in3", "AND2:in2");
test.clist <- append.connectionList(test.clist, "AND1:out", "OR3:in1");
test.clist <- append.connectionList(test.clist, "AND2:out", "OR3:in2");
test.clist <- append.connectionList(test.clist, "OR3:out", "out");

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
test.clist2 <- connectionList("in1", "AND1:in1,C1:in1");
test.clist2 <- append.connectionList(test.clist2, "in2", "AND1:in2,C1:in2");
test.clist2 <- append.connectionList(test.clist2, "in3","C1:in3");
test.clist2 <- append.connectionList(test.clist2, "C1:out","OR1:in1");
test.clist2 <- append.connectionList(test.clist2, "AND1:out","OR1:in2");
test.clist2 <- append.connectionList(test.clist2, "OR1:out","out");

test.COMP2gate <- gate("C2", input=test.intlist[1:3],
                       conn=test.clist2,
                       definition=test.glist2,
                       out=test.olist)

test.glist3 <- list(AND1=test.ANDgate, OR4=test.ORgate, OR5=test.ORgate, C2=test.COMP2gate, C1=test.COMPgate, AND2=test.ANDgate, AND3=test.ANDgate);
test.clist3 <- connectionList("in1", "AND1:in1,C2:in1");
test.clist3 <- append.connectionList(test.clist3, "in2", "AND1:in2,C2:in2")
test.clist3 <- append.connectionList(test.clist3, "in3", "OR4:in1,C2:in3")
test.clist3 <- append.connectionList(test.clist3, "in4", "OR4:in2")
test.clist3 <- append.connectionList(test.clist3, "OR4:out", "OR5:in1")
test.clist3 <- append.connectionList(test.clist3, "AND1:out", "AND2:in1,OR5:in2,C1:in1")
test.clist3 <- append.connectionList(test.clist3, "in5", "C1:in2")
test.clist3 <- append.connectionList(test.clist3, "C2:out", "C1:in3")
test.clist3 <- append.connectionList(test.clist3, "C1:out", "AND3:in1")
test.clist3 <- append.connectionList(test.clist3, "OR5:out", "AND2:in2")
test.clist3 <- append.connectionList(test.clist3, "AND2:out", "AND3:in2")
test.clist3 <- append.connectionList(test.clist3, "AND3:out", "out1")

test.COMP3gate <- gate("C3", input=test.intlist[1:5],
                       conn=test.clist3,
                       definition=test.glist3,
                       out=c("out1"));


#test.glist4 <- list(AND1=test.ANDgate, C3=test.COMP3gate

test.ilist <- list(this=gateIOorig(inputs=list("in1"="0","in2"="1")),
                   "C1"=gateIOorig(inputs=list("i1"="a","i2"="b", "i3"="c"),
                                outputs=list("out1"="","out2"="")),
                   "C2"=gateIOorig(inputs=list("in1"="1")),
                   "C2.C3"=gateIOorig(inputs=list("in1"="f","in2"="g")));

ts <- gateIOorig(inputs=list("in1"="0"))
ts <- gateIOorig.set(list(this=ts), "in2", "1")
ts <- gateIOorig.set(ts, "in3", "1")
ts <- gateIOorig.set(ts, "in4", "0")
