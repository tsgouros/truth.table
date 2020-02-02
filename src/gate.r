## A system for simulating an arbitrary arrangement of arbitrary 'gate'
## functions that accept inputs and produce outputs.

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


## Some generic functions that will be used by our objects.

## This is for testing.  There are tests embedded in this file, and this
## variable controls whether the test variables are deleted before this
## script ends.
deleteTestVariables <- FALSE;

## A 'format' function to be just a print function that works well with
## others.  Usually just the same as 'print' without the final \n.
setGeneric(name="formatVal",
           def=function(object, style="short", ...) {
               standardGeneric("formatVal");
           });
setGeneric(name="describe",
           def=function(object) {
               standardGeneric("describe");
           });

## For types, checks to see if a value is a type.
setGeneric(name="check",
           def=function(object,testVal) {
               standardGeneric("check");
           });

## Adds values to the various objects.
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

## A generic subset-getter.
setGeneric(name="subset",
           def=function(object, ...) {
               standardGeneric("subset");
           });

setGeneric(name="is.empty",
           def=function(object, ...) {
               standardGeneric("is.empty");
           });

setGeneric(name="showCount",
           def=function(object) {
               standardGeneric("showCount");
           });

setGeneric(name="showShape",
           def=function(object) {
               standardGeneric("showShape");
           });

setGeneric(name="probs",
           def=function(object, ...) {
               standardGeneric("probs");
           });

setGeneric("info",
           def=function(object, ...) {
               standardGeneric("info");
           });

setGeneric(name="record",
           def=function(object, inVal, inspect=FALSE) {
               standardGeneric("record");
           });

setGeneric(name="export",
           def=function(object) {
               standardGeneric("export");
           });

############################################################################
## TYPES
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
          definition=function(object, style) {
              if (object@baseType == "symbol") {
                  if (style == "verbose") {
                      return(paste0("baseType=", object@baseType, ": range=",
                                   paste0(object@range, collapse="/")));
                  } else {
                      return(paste0(object@baseType, ": ",
                                    paste0(object@range, collapse="/")));
                  }
              } else {
                  if (style == "verbose") {
                      return(paste0("baseType=", object@baseType,
                                    ": min: ", object@min,
                                    ", max: ", object@max));
                  } else {
                      return(paste0(object@baseType, ": min: ", object@min,
                                    ", max: ", object@max));
                  }
              }
          })

setMethod("show",
          signature="type",
          definition=function(object) {
              cat(formatVal(object), "\n");
          })

setMethod("describe",
          signature="type",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          })

## Use 'check(binary, "0")' to see whether a value is an acceptable member of
## the given type.  Note that you can have an empty gval, with a type
## declared. (should this be optional?)
setMethod("check",
          signature="type",
          definition=function(object, testVal) {
              if (is.null(testVal)) return(TRUE);
              if (object@baseType == "symbol") {
                  if (class(testVal) != "character") return(FALSE);
                  if (testVal == "") return(TRUE);
                  if (!(testVal %in% object@range)) return(FALSE);
              } else if (object@baseType == "integer") {
                  if (class(testVal) != "numeric") return(FALSE);
                  if (is.nan(testVal)) return(TRUE);
                  if ((testVal < object@min) || (testVal > object@max))
                      return(FALSE);
                  if (testVal != round(testVal)) return(FALSE);
              } else if (object@baseType == "float") {
                  if (class(testVal) != "numeric") return(FALSE);
                  if (is.nan(testVal)) return(TRUE);
                  if ((testVal < object@min) || (testVal > object@max))
                      return(FALSE);
              }
              return(TRUE);
          })

## Some basic classes to have around by default.
binary <- type(baseType="symbol", range=c("0","1"));
integer <- type(baseType="integer", min=0, max=100);
float <- type(baseType="float", min=0.0, max=1.0);

if (formatVal(binary) != "symbol: 0/1") stop("binary no good");
if (formatVal(integer) != "integer: min: 0, max: 100") stop("integer oops");
if (formatVal(float) != "float: min: 0, max: 1") stop("float bad");

############################################################################
## TYPE LIST
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

              for (name in names(args)) {
                  object@data[[name]] <- args[[name]];
              }
              return(object);
          });

setMethod("formatVal",
          signature="typeList",
          definition=function(object, style) {
              out <- c();
              if (style == "verbose") {
                  out <- c("data:");
                  for (item in 1:length(object)) {
                      out <- c(out,
                               paste0("[[", item, "]] \"",
                                     names(object)[item], "\" (",
                                     formatVal(object[[item]], style="short"),
                                     ")"));
                  }
              } else {
                  for (name in names(object)) {
                      out <- c(out,
                               paste0(name, " (",
                                      formatVal(object[[name]]), ")"));
                  }
              }
              return(paste0(out, collapse="\n"));
          })

setMethod("show",
          signature="typeList",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="typeList",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

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


## A few typeList tests.
tl <- typeList("binary"=binary, "integer"=integer);
tl <- add(tl, "float"=float);

if (formatVal(tl) != "binary (symbol: 0/1)\ninteger (integer: min: 0, max: 100)\nfloat (float: min: 0, max: 1)") stop("typeList problem");
if (formatVal(tl[["integer"]]) != "integer: min: 0, max: 100")
    stop("typeList problem with subsetting");


############################################################################
## CONNECTIONS
##
## We define connections among the gates with a cnxnList of cnxnElements
## specifying a set of "sinks" for each "source".  Connections can be
## one-source-to-one-sink or one-source-to-many-sinks, but not
## many-sources-to-one-sink.  (If you need to do that, you need to define a
## gate that can undertake the combination of sources into one output and
## then send that somewhere.)
##
## To do this we need a 'cnxn' object to hold an id, color, and weight, and
## a cnxns to be a list of such objects that connects a sink name to
## each id, color, and weight.  Those lists will then be part of a cnxnList
## object to connect sources and sinks.
cnxnIDCounter <- 0;
cnxn <- setClass(
    "cnxn",
    slots = c(id="numeric", color="character", weight="numeric"),
    validity = function(object) {
        if ((class(object@id) != "numeric") || (object@id != round(object@id)))
            return(paste("Bad ID for connection", object@sink));

        if ((class(object@weight) != "numeric") ||
            (object@weight < 0))
            return(paste("Bad weight for connection", object@sink));

        if (class(object@color) != "character")
            return(paste("Bad color for connection", object@sink));

        return(TRUE);
    })

setMethod("initialize",
          signature="cnxn",
          definition=function(.Object, ...) {
              args <- list(...);

              cnxnIDCounter <<- cnxnIDCounter + 1;
              .Object@id <- cnxnIDCounter;

              if ("color" %in% names(args)) {
                  .Object@color = args[["color"]];
              } else {
                  .Object@color = "gray";
              }

              if ("weight" %in% names(args)) {
                  .Object@weight = args[["weight"]];
              } else {
                  .Object@weight = 1.0;
              }

              validObject(.Object);

              ## This is magic for adjusting the global variable.
              assign("cnxnIDCounter", cnxnIDCounter, envir = .GlobalEnv)
              return(.Object);
          });

setMethod("formatVal",
          signature="cnxn",
          definition=function(object, style) {
              out <- "";
              if (style == "verbose") {
                  out <- paste0("id: ", object@id,
                                ", color: ", object@color,
                                ", weight: ", object@weight);
              } else {
                  out <- paste0(object@id,
                                " (", object@color,
                                ",", object@weight, ")");
              }
              return(out);
          });

setMethod("show",
          signature="cnxn",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="cnxn",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });


## A cnxns object is just one or a bunch of names to attach to cnxn nodes.
## Think of it as a list of 'sinks' to which a source will be connected.
cnxns <- setClass(
    "cnxns",
    slots = c(sink="list"),
    prototype = c(sink=list()),
    validity = function(object) {

        ## Empty sink lists are ok.
        if (length(object@sink) < 1) return(TRUE);

        for (i in 1:length(object@sink)) {
            if (names(object@sink)[i] == "")
                return("Need a name for each entry.");
            if (class(object@sink[[i]]) != "cnxn")
                return(paste("Bad connection:", names(object@sink)[i]));
        }
        return(TRUE);
    })

##
## This is how to initialize a cnxns object:
##
##  $ cnxns("C2:in1"=cnxn(color="gray", weight=2),...)
##
##  $ cnxns("C2:in1", "C1:in2", ...)  This will produce a cnxn object with
##  the default color and width values for each sink name.
setMethod("initialize",
          signature="cnxns",
          definition=function(.Object, ...) {
              args <- list(...);

              if (length(args) > 0) {
                  for (i in 1:length(args)) {
                      if (is.null(names(args)) || names(args)[i] == "") {
                          if (class(args[[i]]) == "character") {
                              .Object@sink[[args[[i]]]] <- cnxn();
                          } else {
                              stop("Bad spec for source.");
                          };
                      } else {
                          name <- names(args)[i];
                          .Object@sink[[name]] <- args[[name]];
                      }
                  }
              }

              validObject(.Object);
              return(.Object);
          });

setMethod("formatVal",
          signature="cnxns",
          definition=function(object, style) {
              if (style == "verbose") {
                  out <- c();
                  if (length(object@sink) > 0) {
                      for (i in 1:length(object@sink)) {
                          out <- c(out,
                                   paste0("[[", i, "]] ",
                                          names(object@sink)[i],
                                          "(", formatVal(object@sink[[i]],
                                                         style="verbose"),
                                          ")"));
                      }
                  }
                  out <- paste0(out, collapse="\n");
                  out <- paste0("sink:\n", out);
              } else {
                  out <- c();
                  if (length(object@sink) > 0) {
                      for (name in names(object@sink)) {
                          out <- c(out,
                                   paste0(name, "(", object@sink[[name]]@id,
                                          ")"));
                      }
                  }
                  out <- paste0(out, collapse=", ");
              }
              return(out);
          });

setMethod("show",
          signature="cnxns",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="cnxns",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });


setMethod("add",
          signature="cnxns",
          definition=function(object, ...) {
              args <- list(...);
              for (i in 1:length(args)) {

                  if (class(args[[i]]) == "cnxn") {
                      if (is.null(names(args)) || (names(args)[i] == "")) {
                          stop("Need a name for each cnxn object.");
                      } else {
                          object@sink[[names(args)[i]]] <- args[[i]];
                      }
                  } else if (class(args[[i]]) == "cnxns") {
                      sublist <- args[[i]];
                      for (cnxn in names(sublist)) {
                          ## This will overwrite previous connections if the
                          ## names are the same.  I don't think this is a
                          ## problem, but the ID will change.
                          object@sink[[cnxn]] <-
                              ## Create a new cnxn object so they all are
                              ## unique IDs.
                              cnxn(color=sublist[[cnxn]]@color,
                                   weight=sublist[[cnxn]]@weight);
                      }
                  } else {
                      stop("Problem with adding cnxns.  Bad class:",
                           class(args[[i]]));
                  }
              }
              assign("cnxnIDCounter", cnxnIDCounter, envir = .GlobalEnv)
              return(object);
          });

setMethod("names",
          signature="cnxns",
          definition=function(x) { return(names(x@sink)); });

setMethod("[[",
          signature="cnxns",
          definition=function(x, i, j, ...) { return(x@sink[[i]]); });

setMethod("length",
          signature="cnxns",
          definition=function(x) { return(length(x@sink)); });


## Testing
ce <- cnxns("C1:in1"=cnxn(), "C2:in2"=cnxn());
ce <- add(ce, "C3:in3"=cnxn());
ce2 <- cnxns("C1:in2", "C2:in1");
ce3 <- cnxns();
ce3 <- add(ce3, ce, ce2);
if (formatVal(ce2) != "C1:in2(4), C2:in1(5)") stop("ce2 problem");
if (formatVal(ce) != "C1:in1(1), C2:in2(2), C3:in3(3)")
    stop("ce problem");
if (formatVal(ce3) != "C1:in1(6), C2:in2(7), C3:in3(8), C1:in2(9), C2:in1(10)")
    stop("ce3 problem");


############################################################################
## CONNECTION LISTS
##
## A cnxnList is a list of connections between 'sources' (identified with a
## character string) and 'sinks' (identified with a cnxns object).  The
## source specifications look like this: "<gate>:<name>" where <gate> is the
## name of a gate in the gate list and <name> is one of the inputs or outputs
## belonging to that gate.  e.g. "AND1:in1" and "OR3:in2" and "OR4:out".
##
## Examples:
##  cl <- cnxnList("AND2:out"="AND3:in1", "AND3.out"="OR1:in1,OR2:in2") or
##  ce1 <- cnxns("AND3:in1")
##  ce2 <- cnxns("OR1:in1,OR2:in2")
##  cl <- cnxnList("AND2:out"=ce1, "AND3.out"=ce2) or
##
cnxnList <- setClass(
    "cnxnList",
    slots=c(data="list"),
    prototype = c(data=list()),
    validity = function(object) {
        if (class(object@data) != "list")
            return("cnxnList must begin with a list.");
        if (length(object@data) == 0) return(TRUE);
        if (sum(sapply(object@data, function(x) {class(x) == "cnxns"})) !=
            length(object@data)) return("All elements must be cnxns");
        return(TRUE);
    });

## Initialize a cnxnList like this: cnxnList(source1, source2, ...)
setMethod("initialize",
          signature="cnxnList",
          definition=function(.Object, ...) {
              args <- list(...);
              if (length(args) > 0) {

                  args <- list(...);
                  for (i in 1:length(args)) {
                      if (class(args[[i]]) == "character") {
                          newSink <- cnxns(args[[i]]);
                      } else if (class(args[[i]]) == "cnxns") {
                          newSink <- args[[i]];
                      } else {
                          stop("Bad value for cnxnList sink.");
                      }

                      ## If the current source isn't represented, add it.
                      if ((is.null(.Object@data)) ||
                          (!(names(args)[i] %in% names(.Object@data)))) {
                          .Object@data[[names(args)[i]]] <- cnxns();
                      }
                      ## Source already represented, add it.
                      .Object@data[[names(args)[i]]] <-
                          add(.Object@data[[names(args)[i]]], newSink);

                  }
              }
              validObject(.Object);
              return(.Object);

          });

## Doesn't seem to be much to do with 'verbose' flag for this one.
setMethod("formatVal",
          signature="cnxnList",
          definition=function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              out <- c();
              if (length(object@data) > 0) {
                  for (i in 1:length(object@data)) {
                      out <- c(out, paste(names(object@data)[i],
                                          formatVal(object@data[[i]]),
                                          sep=" --> "));
                  }
              }
              out <- paste0(prefix, out, collapse="\n");
              if (style == "verbose") {
                  return(paste0("data:\n", out));
              } else {
                  return(out);
              }
          });

setMethod("show",
          signature="cnxnList",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="cnxnList",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

setMethod("is.empty",
          signature="cnxnList",
          definition=function(object, ...) {
              return(length(object@data) == 0);
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
## "in1"=cnxns("AND1.in1","AND2.in2"...)  We refer to these pairs as sources
## and sinks.
##
## The funny logic in here is so we can add things to cnxns objects already
## in place.  So if there's an "in1 --> C2:in2" in a list called a, we can do
## add(a, "in1"="C3:in2") and have "C3:in2" added to the already existing set
## of sinks for the "in1" source.
setMethod("add",
          signature="cnxnList",
          definition=function(object, ...) {
              args <- list(...);
              for (i in 1:length(args)) {
                  ## If the current source isn't represented, add it.
                  if (!(names(args)[i] %in% names(object@data))) {
                      object@data[[names(args)[i]]] <- cnxns();
                  }

                  if (class(args[[i]]) == "character") {
                      ## If the argument is just a string, we assume it to be
                      ## a node name.  Turn it into a cnxn and put it in a
                      ## cnxns object.
                      argToAdd <- cnxns(args[[i]]);
                  } else {
                      argToAdd <- args[[i]];
                  }

                  object@data[[names(args)[i]]] <-
                      add(object@data[[ names(args)[i]]], argToAdd);
              }
              return(object);
          });

## Testing cnxnList
cl <- cnxnList("AND2:out"="AND3:in1", "AND3:out"=cnxns("OR1:in1","OR2:in2"));
cl <- add(cl, "AND3:out"="AND4:in2");
cl <- add(cl, "in1"="AND1:in1");

cl2 <- cnxnList("in1"=ce, "in2"=ce2);
cl2 <- add(cl2, "in3"=ce3);

if (formatVal(cl) != "AND2:out --> AND3:in1(14)\nAND3:out --> OR1:in1(15), OR2:in2(16), AND4:in2(18)\nin1 --> AND1:in1(20)") stop("cl problem");
if (formatVal(cl2) != "in1 --> C1:in1(21), C2:in2(22), C3:in3(23)\nin2 --> C1:in2(24), C2:in1(25)\nin3 --> C1:in1(26), C2:in2(27), C3:in3(28), C1:in2(29), C2:in1(30)") stop("cl2 problem");
#
############################################################################
## VALUES, LINKED WITH TYPES
##
## A gval is a container that links a value and a type.  It provides its own
## checking facility for assignments.  Also note that a gval can have a type
## but remain empty, with a value of "" or NaN, depending on whether it's
## secretly a character or numeric data value.
gval <- setClass(
    "gval",
    slots=c(numVal="numeric", symVal="character", type="type"),
    validity= function(object) {
        if (class(object@type) != "type") return("Need a type for type.");
        return(TRUE);
    });

## You can do a constructor with numVal=, symVal=, or value=, but also need a
## "type=".  Easiest is just 'gval("0", binary)'  You can also define only
## the type in which case the value is NaN.
setMethod("initialize",
          signature="gval",
          definition=function(.Object, ...) {
              args <- list(...);

              if (is.null(names(args))) {
                  ## This is either gval("0", typeName) or gval("0") and gets
                  ## the default type.
                  if (length(args) == 2) {
                      .Object@type <- args[[2]];
                      if (check(.Object@type, args[[1]])) {
                          if (.Object@type@baseType == "symbol") {
                              .Object@symVal <- args[[1]];
                          } else {
                              .Object@numVal <- args[[1]];
                          }
                      } else {
                          stop("Given value and type do not match.");
                      }
                  } else {
                      ## No type is specified, so we will try to guess.
                      if (is.character(args[[1]])) {
                          .Object@type <- binary;
                          .Object@symVal <- args[[1]];
                      } else {
                          .Object@numVal <- args[[1]];
                          if (round(args[[1]]) == args[[1]]) {
                              .Object@type <- integer;
                          } else {
                              .Object@type <- float;
                          }
                      }
                  }
              } else {
                  if ("type" %in% names(args)) {
                      if ("numVal" %in% names(args)) {
                          .Object@type <- args[["type"]];
                          .Object@numVal <- args[["numVal"]];
                      } else if ("symVal" %in% names(args)) {
                          .Object@type <- args[["type"]];
                          .Object@numVal <- args[["symVal"]];
                      } else if ("value" %in% names(args)) {
                          .Object@type <- args[["type"]];
                          .Object@numVal <- args[["value"]];
                      } else {
                          if (length(args) == 1) {
                              ## This is (we hope) a type with no value, so
                              ## will get a value of NaN or "".
                              .Object@type <- args[["type"]];
                              if (.Object@type@baseType == "symbol") {
                                  .Object@symVal <- "";
                              } else {
                                  .Object@numVal <- NaN;
                              }
                          } else {
                              ## gval("0", type=binary)
                              .Object@type <- args[["type"]];
                              if (.Object@type@baseType == "symbol") {
                                  .Object@symVal <- args[[1]];
                              } else {
                                  .Object@numVal <- args[[1]];
                              }
                          }
                      }
                  }
              }
              validObject(.Object);
              return(.Object);
          });

setMethod("getVal",
          signature="gval",
          definition=function(object, ...) {
              args <- list(...);
              if (object@type@baseType == "symbol") {
                  return(object@symVal);
              } else {
                  return(object@numVal);
              }
          });

setMethod("setVal",
          signature="gval",
          definition=function(object, ...) {
              args <- list(...);

              if (length(args) != 1) {
                  stop("setVal requires one argument and only one.");
              }

              if (!check(object@type, args[[1]])) {
                  stop("value is the wrong type.");
              }

              if (object@type@baseType == "symbol") {
                  object@symVal <- args[[1]];
              } else {
                  object@numVal <- args[[1]];
              }

              return(object);
          });

setMethod("is.empty",
          signature="gval",
          definition=function(object, ...) {
              if (object@type@baseType == "symbol") {
                  return(object@symVal == "");
              } else {
                  return(is.nan(object@numVal));
              }
          });

setMethod("formatVal",
          signature="gval",
          definition=function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              if (style == "verbose") {

                  if (object@type@baseType == "symbol") {
                      if (object@symVal == "") {
                          out <- "symVal= [empty]";
                      } else {
                          out <- paste0(prefix, "symVal= ",
                                        object@symVal);
                      }
                  } else {
                      if (is.nan(object@numVal)) {
                          out <- "numVal= [empty]";
                      } else {
                          out <- paste0(prefix, "numVal= ",
                                        object@numVal);
                      }
                  }
                  return(paste0(out, "\n", prefix,
                                formatVal(object@type, style="verbose")));
              } else {

                  if (object@type@baseType == "symbol") {
                      if (object@symVal == "") {
                          out <- "[empty]";
                      } else {
                          out <- object@symVal;
                      }
                  } else {
                      if (is.nan(object@numVal)) {
                          out <- "[empty]";
                      } else {
                          out <- paste(object@numVal);
                      }
                  }
                  return(paste0(out, " (", formatVal(object@type), ")"));
              }
          });

setMethod("show",
          signature="gval",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="gval",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

setMethod("check",
          signature="gval",
          definition=function(object, testVal) {
              return(check(object@type, testVal));
          });

## Testing gval

gv <- gval("0", type=binary);
if (formatVal(gv) != "0 (symbol: 0/1)") stop("gv problem");
gv2 <- gval(0.5, float);
if (formatVal(gv2) != "0.5 (float: min: 0, max: 1)")
    stop("gv float problem");
tryCatch({gv <- gval(0.5, integer);},
         error=function(cond) {
             if (format(cond)[1] != "Given value and type do not match.")
                 stop("gval error issue");
         });
tryCatch({gv <- gval(1, binary);},
         error=function(cond) {
             if (format(cond)[1] != "Given value and type do not match.")
                 stop("second gval error issue");
         });

gv <- gval(type=binary)
if (formatVal(gv) != "[empty] (symbol: 0/1)")
    stop("another gv problem");
gv <- setVal(gv, "0")
if (formatVal(gv) != "0 (symbol: 0/1)")
    stop("third gv problem");
tryCatch({gv <- setVal(gv, "2")},
         error=function(cond) {
             if (format(cond)[1] != "value is the wrong type.")
                 stop("gv setVal problem");
         });
if (getVal(gv) != "0") stop("second gv setVal problem");

if (is.empty(gv)) stop("gv is not empty");
if (!is.empty(gval(type=binary))) stop("empty gval reads as full.");


############################################################################
## INPUTS AND OUTPUTS
##
## A gateIO object holds a set of inputs and outputs for some gate.  This
## is two lists of gvals, one for the inputs to some gate and the other for
## the output.  Don't use 'replace' or 'type' for an input or output label.
gateIO <- setClass(
    "gateIO",
    slots=c(inputs="list", outputs="list"),
    validity = function(object) {
        if (class(object@inputs) != "list") return("inputs must be a list");
        if (class(object@outputs) != "list") return("outputs must be a list");
        if ("replace" %in% names(object@inputs))
            return("Please don't use 'replace' for an input name.");
        if ("replace" %in% names(object@outputs))
            return("Please don't use 'replace' for an output name.");

        ## We don't need to check all the values against the types because
        ## that is done in the gval constructor.

        return(TRUE);
    });

## gateIO(input=list("in1"=gval("0",binary), "in2"=gval(1, binary)),
##        output=list(...))
##
setMethod("initialize",
          signature="gateIO",
          definition=function(.Object, ...) {
              args <- list(...);
              input <- grepl("^i", names(args));
              if (sum(input) == 1) { ## There's one arg that starts with 'i'.
                  inputIndex <- which(input);
                  if (length(inputIndex) > 1) {
                      stop("Ambiguous argument to constructor.");
                  }

                  ## A modest amount of type checking, though more is done
                  ## in the gval constructor.
                  if (class(args[[inputIndex]]) != "list")
                      stop("gateIO inputs must be a list.");
                  if (class(args[[inputIndex]][[1]]) != "gval")
                      stop("gateIO inputs must be a list of gvals.");

                  .Object@inputs <- args[[inputIndex]];
              } else if (sum(input) > 1) {
                  stop("gateIO objects forbid multiple input lists.");
              }

              output <- grepl("^o", names(args));
              if (sum(output) == 1) { ## There's one arg that starts with 'o'.
                  outputIndex <- which(output)
                  if (length(outputIndex) > 1) {
                      stop("Ambiguous argument to constructor.");
                  }

                  if (class(args[[outputIndex]]) != "list")
                      stop("gateIO outputs must be a list.");
                  if (class(args[[outputIndex]][[1]]) != "gval")
                      stop("gateIO outputs must be a list of gvals.");

                  .Object@outputs <- args[[outputIndex]];
              } else if (sum(output) > 1) {
                  stop("gateIO objects forbid multiple output lists.");
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

              if (style == "verbose") {

                  outstr <- "";
                  out <- c();
                  if (length(object@inputs) > 0) {
                      for (i in 1:length(object@inputs)) {
                          out <- c(out, paste0(prefix, "  ",
                                               names(object@inputs)[i], ": ",
                                               formatVal(object@inputs[[i]])));
                      }
                      outstr <- paste0(prefix, "inputs:\n",
                                      paste0(out, collapse="\n"));
                  }

                  if (length(object@outputs) == 0) return(outstr);
                  if (outstr != "") outstr <- paste0(outstr, "\n");

                  out <- c();
                  for (i in 1:length(object@outputs)) {
                      out <- c(out, paste0(prefix, "  ",
                                           names(object@outputs)[i], ": ",
                                           formatVal(object@outputs[[i]])));
                  }
                  outstr <- paste0(outstr, prefix, "outputs:\n",
                                   paste0(out, collapse="\n"));
                  return(outstr);

              } else {

                  ## If either the input or output list is too long, use
                  ## 'verbose' which formats the gvals on separate lines.
                  if ((length(object@inputs) > 5) ||
                      (length(object@inputs) > 5))
                      return(formatVal(object, style="verbose"));

                  outstr <- "";
                  out <- c();
                  if (length(object@inputs) > 0) {
                      for (i in 1:length(object@inputs)) {
                          out <- c(out, paste0(names(object@inputs)[i], "=",
                                               formatVal(object@inputs[[i]])));
                      }
                      outstr <- paste0(prefix, "I: ",
                                       paste0(out, collapse=", "));
                  }

                  if (length(object@outputs) == 0) return(outstr);
                  if (outstr != "") outstr <- paste0(outstr, "\n");

                  out <- c();
                  for (i in 1:length(object@outputs)) {
                      out <- c(out, paste0(names(object@outputs)[i], "=",
                                           formatVal(object@outputs[[i]])));
                  }
                  outstr <- paste0(outstr, prefix, "O: ",
                                   paste0(out, collapse=", "));
                  return(outstr);
              }
          });

setMethod("show",
          signature="gateIO",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="gateIO",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

setMethod("getVal",
          signature="gateIO",
          definition=function(object, ...) {
              args <- list(...);
              valName <- args[[1]];

              if (args[[1]] == "inputs") return(object@inputs);
              if (args[[1]] == "outputs") return(object@outputs);

              ## The valName could refer to an item among the inputs or the
              ## outputs, so check both before giving up.
              if (valName %in% names(object@inputs)) {
                  return(getVal(object@inputs[[valName]]));
              } else if (valName %in% names(object@outputs)) {
                  return(getVal(object@outputs[[valName]]));
              } else {
                  return(NULL);
              }
          });

setMethod("is.empty",
          signature="gateIO",
          definition=function(object, ...) {
              args <- list(...);

              if (length(args) < 1) {
                  return((length(object@inputs) == 0) &&
                         (length(object@outputs) == 0));
              } else {
                  if (grepl("^in", args[[1]])) {
                      ## If any input is empty, return TRUE.
                      if (length(object@inputs) > 0) {
                          for (gv in object@inputs) {
                              if (is.empty(gv)) return(TRUE);
                          }
                          return(FALSE);
                      } else {
                          return(TRUE);
                      }
                  } else if (grepl("^out", args[[1]])) {
                      ## If any output is empty, return TRUE.
                      if (length(object@outputs) > 0) {
                          for (gv in object@outputs) {
                              if (is.empty(gv)) return(TRUE);
                          }
                          return(FALSE);
                      } else {
                          return(TRUE);
                      }
                  } else {
                      stop("Don't know that argument:", args[[1]]);
                  }
              }
          });

## Clear the output list values.
setGeneric(name="clearOutputs",
           def=function(object) {
               standardGeneric("clearOutputs");
           });

setMethod("clearOutputs",
          signature="gateIO",
          definition=function(object) {
              blankOutputs <- list();
              for (name in names(object@outputs)) {
                  blankOutputs[[name]] <-
                      gval(type=object@outputs[[name]]@type);
              }
              return(object);
          });

## A convenience function.  We do some guessing of a node's purpose by
## looking at its name.  If it's not an output, we assume it's an input.  So
## outputs start with 'o' or 'r', or 'out' or 'res'.  This is only for
## convenience in the shortened version of setVal().  You can always specify
## which is output and which is input explicitly.  The only real restrictions
## are don't name anything 'replace' or 'type' or some flag's name.  If you
## do, you'll be sorry, though I can't predict how, exactly.
isOutputName <- function(vname) {
    return(grepl("out|res|^o|^r", vname));
}

## Call like this: g <- setVal(g, "in1"="1").
## or g <- setVal(g, "in1"=gval("1", binary), replace=FALSE)
## also g <- setVal(g, outputs=list("out"=gval(5, integer))), etc
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

              for (name in names(args)) {
                  if (name == "replace") next;
                  if (name == "type") next;
                  if (name == "value") next;

                  if (name == "inputs") {
                      object@inputs <- args[["inputs"]];
                      next;
                  }

                  if (name == "outputs") {
                      object@outputs <- args[["outputs"]];
                      next;
                  }

                  if (name == "name") {
                      newName <- args[["name"]];
                      newVal <- args[["value"]]
                  } else {
                      newName <- name;
                      newVal <- args[[name]];
                  }

                  if (replace &&
                      (!(newName %in% names(object@inputs))) &&
                      (!(newName %in% names(object@outputs)))) {
                      cat(newName, "is not already in object.\n");
                      stop();
                  } else {
                      if (isOutputName(newName)) {
                          if (class(newVal) == "gval") {
                              object@outputs[[newName]] <- newVal;
                          } else {
                              object@outputs[[newName]] <-
                                  setVal(object@outputs[[newName]], newVal);
                          }
                      } else {
                          if (class(newVal) == "gval") {
                              object@inputs[[newName]] <- newVal;
                          } else {
                              object@inputs[[newName]] <-
                                  setVal(object@inputs[[newName]], newVal);
                          }
                      }
                  }
              }
              return(object);
          });

setMethod("names",
          signature="gateIO",
          definition=function(x) { return(c(names(x@inputs),
                                            names(x@outputs))); });
setMethod("[[",
          signature="gateIO",
          definition=function(x, i, j, ...) {

              if (class(i) != "character")
                  stop("Use a name to select a gateIO object.");

              if (i %in% names(x@inputs)) return(x@inputs[[i]]);
              if (i %in% names(x@outputs)) return(x@outputs[[i]]);
          });

setMethod("[[<-",
          signature="gateIO",
          definition=function(x, i, j, ..., value) {
              if (class(i) != "character")
                  stop("Use a name to select a gateIO object.");

              if (i %in% names(x@inputs)) {
                  if (class(value) == "gval") {
                      x@inputs[[i]] <- value;
                  } else {
                      x@inputs[[i]] <- setVal(x@inputs[[i]], value);
                  }
              } else if (i %in% names(x@outputs)) {
                  if (class(value) == "gval") {
                      x@outputs[[i]] <- value;
                  } else {
                      x@outputs[[i]] <- setVal(x@outputs[[i]], value);
                  }
              }
              return(x);
          });



## Testing gateIO
## gio <- gateIO(inp=list("in1"="0", "in2"="1"),out=list("out"));
## gateIO(inp=list("in1"=3),typeInputs=list("in1"=integer)... or
gio <- gateIO(inp=list("in1"=gval("0",binary), "in2"=gval("1", binary)));
if (formatVal(gio) != "I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)")
    stop("gio problem");
if (getVal(gio, "in1") != "0") stop("second gio problem");
if (!is.null(getVal(gio, "in4"))) stop("third gio problem");

gio <- setVal(gio, "in3"=gval("1", binary));
if (formatVal(gio) != "I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1), in3=1 (symbol: 0/1)") stop("fourth gio problem");

gio <- setVal(gio, "out"=gval(type=binary));
if (formatVal(gio) != "I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1), in3=1 (symbol: 0/1)\nO: out=[empty] (symbol: 0/1)") stop("fifth gio problem");

gio <- setVal(gio, "in1"="1");
if (formatVal(gio) != "I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1), in3=1 (symbol: 0/1)\nO: out=[empty] (symbol: 0/1)") stop("sixth gio problem");

gio <- setVal(gio, name="in1", value=gval(5, integer))
if (formatVal(gio) != "I: in1=5 (integer: min: 0, max: 100), in2=1 (symbol: 0/1), in3=1 (symbol: 0/1)\nO: out=[empty] (symbol: 0/1)") stop("seventh gio problem");

if (getVal(gio, "in1") != 5) stop("eighth gio problem");
if (getVal(gio, "in2") != "1") stop("ninth gio problem");

gio <- setVal(gio, outputs=list("out"=gval(6, integer), "out2"=gval(4, integer)));
if (getVal(gio, "out2") != 4) stop("tenth gio problem");

if (is.empty(gio)) stop("gio is not empty.");
if (!is.empty(gateIO())) stop("empty gateIO is not full.");

if (getVal(gio[["in1"]]) != 5) stop("gio subsetting problem.");

############################################################################
## COLLECTIONS OF INPUTS AND OUTPUTS
##
## A gateIOList records the inputs and outputs of several gates.  We use it
## to record the 'state' of some gate.  The class contains some facilities
## for subsetting the list, according to the hierarchy of names that we are
## using.
gateIOList <- setClass(
    "gateIOList",
    slots = c(data="list"),
    prototype = c(data=list()),
    validity = function(object) {
        if (class(object@data) != "list")
            return("gateIOList must begin with a list.");
        if (sum(sapply(object@data, function(x) {class(x) == "gateIO"})) !=
            length(object@data))
            return("All elements must be gateIO.");
        return(TRUE);
    });

setMethod("initialize",
          signature = "gateIOList",
          definition= function(.Object, ...) {
              args <- list(...);

              ## Create a 'this' object.
              .Object@data[["this"]] <- gateIO();

              if (length(args) > 0) {

                  for (i in 1:length(args)) {
                      if (class(args[[i]]) == "gval") {
                          ## This is a raw gval object, add it to 'this'.
                          .Object@data[["this"]] <-
                              setVal(.Object@data[["this"]],
                                     name=names(args)[i], value=args[[i]]);
                      } else if (class(args[[i]]) == "gateIO") {
                          ## This is a gateIO object, just add it to the list.
                          .Object@data[[names(args)[i]]] <- args[[i]];
                      } else {
                          stop("Can't handle given type in gateIOList.");
                      }
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
              outstr <- paste0(prefix, "this:\n");
              outstr <- paste0(outstr,
                               formatVal(object@data[["this"]],
                                         prefix=paste0(prefix, " "),
                                         style=style));

              ## Do the rest of the elements in whatever order they are
              ## stored.
              for (i in 1:length(object@data)) {
                  ## Skip 'this' element.
                  if (names(object@data)[i] == "this") next;

                  outstr <- paste0(outstr, "\n", prefix, "\n",
                                  prefix, names(object@data)[i],
                                  ":\n");
                  outstr <-
                      paste0(outstr,
                             formatVal(object@data[[names(object@data)[i] ]],
                                       prefix=paste0(prefix, " "),
                                       style=style));
              }

              return(outstr);
          });

setMethod("show",
          signature="gateIOList",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="gateIOList",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

## Things to make a gateIOList act more like a list.

## You can do gl[[1]] or gl[["AND2"]] or gl[["^C1"]] in which case we invoke
## the subset() method.
setMethod("[[",
          signature="gateIOList",
          definition=function(x, i, j, ...) {
              ## If this is a regex to match the beginning of the name, use
              ## the subset method.
              if ((class(i) == "character") && (substr(i, 1, 1) == "^")) {
                  return(subset(x, expr=i));
              } else {
                  return(x@data[[i]]);
              }
          });

setMethod("[[<-",
          signature="gateIOList",
          definition=function(x, i, j, ..., value) {
              if ((class(i) == "character") && (substr(i, 1, 1) == "^")) {
                  cat("can't use that kind of subsetting with assignments.\n");
                  stop();
              }

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

## Retrieves a gval using names like "AND1:in2"
setMethod("getVal",
          signature="gateIOList",
          definition=function(object, ...) {
              argList <- list(...);
              if (length(argList) > 1)
                  stop("getVal(gateIOList) can only get one value at a time.");

              if (grepl(":", argList[[1]])) {
                  parts <- strsplit(argList[[1]], ":")[[1]];
                  gateName <- parts[[1]];
                  valName <- parts[[2]];
              } else {
                  gateName <- "this";
                  valName <- argList[[1]];
              }

              if (!(gateName %in% names(object)))
                  stop(gateName, " not in object.");

              return(getVal(object[[gateName]], valName));
          });

## Use like: gls <- setVal(gls, "AND1:in2", gval("0"))
setMethod("setVal",
          signature="gateIOList",
          definition=function(object, ...) {
              argList <- list(...);
              if (length(argList) != 2)
                  stop("setVal(gateIOList) needs a name to set and a gval.");

              if (grepl(":", argList[[1]])) {
                  parts <- strsplit(argList[[1]], ":")[[1]];
                  gateName <- parts[[1]];
                  valName <- parts[[2]];
              } else {
                  gateName <- "this";
                  valName <- argList[[1]];
              }

              if (!(gateName %in% names(object)))
                  stop(gateName, " not in object.");

              object[[gateName ]] <-
                  setVal(object[[gateName ]],
                         name=valName, value=argList[[2]]);

              return(object);
          });


## Testing the basics of gateIOList
gls <- gateIOList("in1"=gval("0"), "in2"=gval("1"),
                   "AND1"=gateIO(i=list("in1"=gval("1"), "in2"=gval("1")),
                                 o=list(out=gval(type=binary))));

if (formatVal(gls) != "this:\n I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\n\nAND1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("gls problem");
if (formatVal(gls[["this"]]) != "I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)")
    stop("second gls problem");

gls[["this"]] <- gateIO(i=list("in1"=gval("1"), "in2"=gval("0")), o=list("out"=gval(type=binary)))
if (formatVal(gls) != "this:\n I: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nAND1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("third gls problem");

if (formatVal(setVal(gls, "AND1:in2", gval("0"))) != "this:\n I: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nAND1:\n I: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("third-and-a-half gls problem");


###########################################################################
##
## From here, we create a couple of facilities for managing the hierarchical
## names of the gateIOList.  For example, give me all the names with a given
## prefix, or incorporate this sub-list with this prefix and that sort of
## thing.
##
## add(gateIOList, "AND1:in1", "0", binary)
##
## add(gateIOList, "AND1", gateIO object)
##
## subset(gateIOList, expr="C1") -> gives you the list with all the names
## that begin with "C1", with the "C1" removed from the ones that have it
## as a prefix, and the "C1" entry itself turned to 'this', for input to
## the C1 gate.  (We considered doing this with the '[[' method, but that
## seemed like it would interfere with simpler usages.)
setMethod("subset",
          signature = "gateIOList",
          definition= function(object, ...) {
              args <- list(...);

              if (!("expr" %in% names(args))) {
                  cat("Need an expression to subset with.\n");
                  stop();
              }

              outList <- gateIOList();
              target <- args[["expr"]];
              ## If the target ends with a '.', erase it.
              if (substr(target, nchar(target), nchar(target)) == ".")
                  target <- substr(target, 1, (nchar(target) - 1));

              for (name in names(object@data)) {
                  if (grepl(paste0("^", target), name)) {
                      newName <- sub(target, "", name);
                      if (newName == "") {
                          newName <- "this";
                      } else {
                          ## Drop the '.' if it's there.  If it isn't,
                          ## there is probably something wrong, but we're
                          ## letting that pass here.
                          if (substr(newName,1,1) == ".")
                              newName <- substr(newName, 2, nchar(newName));
                      }

                      outList[[newName]] <- object@data[[name]];
                  }
              }
              return(outList);
          });


## Two forms:
## add(gateIOList, newName=gateIO.item) and
## add(gateIOList, tag="C1", sublist=C1sublist) Incorporates the output
## sublist from the C1 gate back into a larger list, prepending "C1" to
## all the names.
setMethod("add",
          signature = "gateIOList",
          definition = function(object, ...) {
              args <- list(...);

              if ("sublist" %in% names(args)) {
                  ## Basic type checking.
                  if (!("tag" %in% names(args)) &&
                      (class(args[["tag"]] != "character"))) {
                      cat("A sublist needs a valid tag.\n");
                      stop();
                  }
                  if (class(args[["sublist"]]) != "gateIOList") {
                      cat("sublist is for joining two gateIOList objects.\n");
                      stop();
                  }

                  ## Sort through the input list and add the values to the
                  ## output object, with the new names on them.
                  for (name in names(args[["sublist"]])) {
                      if (name == "this") {
                          object[[args[["tag"]] ]] <-
                              args[["sublist"]][[name]];
                      } else {
                          object[[paste(args[["tag"]], name, sep=".")]] <-
                              args[["sublist"]][[name]];
                      }
                  }
              } else {
                  ## This is just one or more gateIO objects (though we'll
                  ## check that) to be added to the output object.
                  for (name in names(args)) {
                      if (class(args[[name]]) != "gateIO") {
                          cat(name, "must be a gateIO object.\n");
                          stop();
                      }

                      object[[name]] <- args[[name]];
                  }
              }
              return(object);
          });

## Testing the subsetting and adding functions for gateIOList
gls <- gateIOList("in1"=gval("0"), "in2"=gval("1"),
                   "AND1"=gateIO(i=list("in1"=gval("1"), "in2"=gval("1")),
                                 o=list("out"=gval(type=binary))));
gls <- add(gls, "AND2"=gateIO(i=list("in1"=gval("1"), "in2"=gval("0")),
                              o=list("out"=gval(type=binary))))

if (formatVal(gls) != "this:\n I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\n\nAND1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nAND2:\n I: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)") stop("gls add problem.");

glsSub <- gateIOList("in1"=gval("0"), "in2"=gval("1"),
                     "OR1"=gateIO(i=list("in1"=gval("0"), "in2"=gval("0")),
                                  o=list("out"=gval(type=binary))));
glsSub <- add(glsSub, "OR2"=gateIO(i=list("in1"=gval("0"), "in2"=gval("0")),
                                   o=list("out"=gval(type=binary))));

glsSubA <- gateIOList("in1"=gval("0"), "in2"=gval("0"),
                      "XOR1"=gateIO(i=list("in1"=gval("1"), "in2"=gval("1")),
                                    o=list("out"=gval(type=binary))));
glsSub <- add(gls, tag="C2", sublist=glsSubA);
gls <- add(gls, tag="C1", sublist=glsSub);

if (formatVal(gls) != "this:\n I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\n\nAND1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nAND2:\n I: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nC1:\n I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\n\nC1.AND1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nC1.AND2:\n I: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nC1.C2:\n I: in1=0 (symbol: 0/1), in2=0 (symbol: 0/1)\n\nC1.C2.XOR1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("gls sub add problem.");

glsSub <- subset(gls, expr="C1");
if (formatVal(glsSub) != "this:\n I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\n\nAND1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nAND2:\n I: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)\n\nC2:\n I: in1=0 (symbol: 0/1), in2=0 (symbol: 0/1)\n\nC2.XOR1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("third gls sub add problem.");

glsSub <- subset(gls, expr="C1.C2");
if (formatVal(glsSub) != "this:\n I: in1=0 (symbol: 0/1), in2=0 (symbol: 0/1)\n\nXOR1:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("fourth gls sub add problem.");


############################################################################
## AN ABSTRACTION OF A GATE
##
## A 'gate' object embodies a transformation of a set of inputs to a set of
## outputs.  The object can represent a simple, 'atomic' transformation, or
## it can represent a 'compound' transformation, an agglomeration of a set of
## gate objects.
##
## An atomic gate object consists of a transformation, while a compound
## object consists of a collection of other gate objects.  The inputs to a
## gate object are embodied in a gateIOList object.  For an atomic gate, this
## is a list with one gateIO entry, containing the inputs and outputs to the
## transformation at the heart of the gate.  For a compound gate, the iolist
## is an instantaneous record of all the inputs and outputs of the gate and
## its components, sub-components, sub-sub-components, and so on.
##
## A gate may have complicated interactions among its components that cannot
## be resolved at once.  For example, a gate that uses feedback among its
## components necessarily has a certain time dependence.  Thus there is a
## transformOnce() method that is called a number of times by the main
## transform() method in order to generate the necessary output(s).  The
## transform() method has some facilities for controlling the time base, like
## a tickMax parameter.
##
## A compound gate must have not only a list of the gates that compose it,
## but also a list of the connections between those gates.
##
##
## The idea is that we want to assert a time base for everything equally, so
## that we can 'tick' the gates together, but we also want a hierarchical
## arrangement of gates, so we can define functional groups of gates as a
## unit.  But we want the functional groups to use the same 'ticks' as their
## parent.
##
## This means that a transformation might have a contingent nature,
## returning not a value, but a state of all the inputs to each of its
## operations, that can be absorbed into the state of the parent.  So our
## record of the state (the 'state list') has to be hierarchical.  We do
## that by creating a hierarchy of the names of the inputs and outputs.  So
## the root list might begin with a list of three inputs, but those get
## mapped to the various inputs to this or that gate, and "in1" is
## translated to "AND1:in1" or "C2:in2" or whatever.  Then the AND1 and C2
## gates are executed with whatever inputs are available.  Upon execution
## of AND1, its inputs are renamed from "AND1:in1" to simply "in1", which
## is how the AND1 gate thinks of it.  Its output value list has names like
## "out" or "out1" or whatever, and when reincorporating those values into
## the parent list, they are prefixed with "AND1".  So:
##
##  "AND1:in1" -> "in1" -> execute AND1 gate -> "out" -> "AND1:out"

gateIDCounter <- 0;
gate <- setClass(
    "gate",
    slots=c(io="gateIO",
            definition="function",
            gateList="list",
            cnxnList="cnxnList",
            color="character",
            shape="character",
            fillcolor="character",
            fontcolor="character",
            fontname="character",
            penwidth="numeric",
            style="character",
            ## These are generated.
            stateList="gateIOList",
            transform="function",
            transformOnce="function",
            id="numeric",
            type="character"),
    validity=function(object) {

        if (class(object@io) != "gateIO")
            return("io must be a gateIO object");

        if ((!is.null(object@definition)) &&
            (class(object@definition) != "function"))
            return("definition must be a function if you specify it.");

        if (class(object@gateList) != "list")
            return("gateList must be a list.");

        if ((length(object@gateList) > 0) &&
            (sum(sapply(object@gateList, function(x) class(x) == "gate")) !=
             length(object@gateList)))
            return("All elements of a gateList must be gate objects.");

        if (class(object@cnxnList) != "cnxnList")
            return("The cnxnList must be a cnxnList, of course.");

        if (class(object@color) != "character")
            return("Express color as a hex code or X11 word.");

        if (class(object@shape) != "character")
            return("Shape is character, please.");

        if (! ((class(object@type) == "character") &&
               ((object@type == "atomic") || (object@type == "compound"))))
            return("Type must be 'atomic' or 'compound'.");

        return(TRUE);
    });

## See the tests below for examples of how to initialize your gate.
setMethod("initialize",
          signature = "gate",
          definition = function(.Object, ...) {

              ## Generate a unique ID for this gate.
              gateIDCounter <<- gateIDCounter + 1;
              .Object@id <- gateIDCounter;
              assign("gateIDCounter", gateIDCounter, envir=.GlobalEnv);

              ## Set default values for args that can be omitted.
              .Object@io <- gateIO();
              .Object@gateList  <-  list();
              .Object@cnxnList  <-  cnxnList();
              .Object@color <- "gray";
              .Object@shape <- "circle";
              .Object@fillcolor <- "white";
              .Object@fontcolor <- "black";
              .Object@fontname <- "san-serif";
              .Object@penwidth <- 1.0;
              .Object@style <- "filled";
              ## Parse constructor arguments.
              args <- list(...);

              ## We use grepl to search the list of slot names to find
              ## partial matches to the input args.  This allows us to make
              ## it work even if the names are not spelled out completely.
              for (name in names(args)) {
                  slotIndex <- grepl(paste0("^", name),
                                     slotNames("gate"));
                  if (sum(slotIndex) == 1) {
                      slotIndex <- which(slotIndex);
                      slot(.Object, slotNames("gate")[slotIndex]) <-
                          args[[name]];

                  } else {
                      cat("ambiguous arguments to gate initialization.\n");
                      stop();
                  }
              }

              ## Decide what kind of gate this is to be.
              if (length(.Object@gateList) == 0) {
                  .Object@type <- "atomic";
              } else {
                  .Object@type <- "compound";
              }

              ## Now we have the pieces in place, construct the two transform
              ## methods.
              if (.Object@type == "compound") {
                  ##show(.Object@cnxnList);


                  ## Make a function to execute the gateList.
                  .Object@transformOnce <-
                      function(argList, inspect=FALSE, prefix="") {

                          ## Use cnxnList to copy inputs into place.
                          for (src in names(.Object@cnxnList)) {
                              for (sink in names(.Object@cnxnList[[src]])) {
                                  argList <-
                                      setVal(argList, sink,
                                             getVal(argList, src));
                              }
                          }

                          ## Execute the component gates.  Once.  Might
                          ## arrange to skip execution if the outputs are
                          ## already there.
                          for (gateName in names(.Object@gateList)) {
                              if (inspect) {
                                  cat(prefix, "executing: ", gateName,
                                      " with:\n",
                                      formatVal(subset(argList,
                                                       expr=gateName),
                                                prefix=prefix),
                                      "\n");
                              }

                              gate <- .Object@gateList[[gateName]];
                              argList <-
                                  add(argList,
                                      tag=gateName,
                                      sublist= gate@transformOnce(
                                                        subset(argList,
                                                               expr=gateName),
                                                        inspect=inspect,
                                                        prefix=prefix));
                          }
                          return(argList);

                      };
                  .Object@transform <- function(argList, tickMax=100,
                                                inspect=FALSE, prefix="") {
                      if (class(argList) != "gateIOList")
                          stop("Bad input list to transformOnce.");

                      ## Copy argList values into stateList (including
                      ## outputs?)
                      for (arg in names(argList)) {
                          if (length(argList[[arg]]@inputs) < 1)
                              stop("Need an input for ", arg, ".");

                          for (input in names(argList[[arg]]@inputs)) {
                              .Object@stateList[[arg]]@inputs[[input]] <-
                                  argList[[arg]]@inputs[[input]];
                          }

                          if (length(argList[[arg]]@outputs) > 0) {
                              for (output in names(arg@outputs)) {
                                  .Object@stateList[[arg]]@outputs[[output]] <-
                                      argList[[arg]]@outputs[[output]];
                              }
                          }
                      }

                      tick <- 0;
                      while (is.empty(.Object@stateList[["this"]], "output")) {
                          tick <- tick + 1;
                          if (inspect) cat("iteration:", tick, "\n");
                          .Object@stateList <-
                              .Object@transformOnce(.Object@stateList,
                                                    inspect=inspect,
                                                    prefix=prefix);
                          if (tick >= tickMax) break;
                      }


                      return(.Object@stateList);
                  }

                  .Object@type="compound";

              } else {

                  ## This must be an atomic gate transform.
                  .Object@transformOnce <- function(argList,
                                                    inspect=FALSE, prefix="") {
                      ## We hope argList is a gateIOList of length 1.
                      if ((class(argList) != "gateIOList") ||
                          (length(argList) != 1)) {
                          stop("Bad inputs to transformOnce.");
                      }

                      argList[["this"]] <- clearOutputs(argList[["this"]]);

                      ## Checking that names and types match.
                      inputList <- getVal(.Object@io, "inputs");
                      for (name in names(inputList)) {
                          if (!(name %in% names(argList[["this"]]))) {
                              if (inspect)
                                  cat(prefix, name, " is missing.\n", sep="");
                              return(argList)
                          }
                          if (!check(inputList[[name]]@type,
                                     getVal(argList[["this"]], name))) {
                              stop(name, " is a bad argument.");
                          }
                          ## We also don't want to execute if any of the
                          ## input values are empty.
                          if (is.empty(argList[["this"]]@inputs[[name]])) {
                              if (inspect) {
                                  cat(prefix, name, " is an empty value.\n",
                                      sep="");
                              }
                              return(argList);
                          }
                      }

                      ## Execute the transformation's definition.
                      argList[["this"]] <-
                          setVal(argList[["this"]],
                                 outputs=
                                     .Object@definition(
                                                 getVal(argList[["this"]],
                                                        "inputs")));

                      return(argList);
                  };
                  ## For an atomic transform, this is the same as
                  ## transformOnce, with the tickMax arg, which doesn't do
                  ## anything.
                  .Object@transform <- function(argList, tickMax=100,
                                                inspect=FALSE, prefix="") {

                      argList <- .Object@transformOnce(argList, inspect=inspect,
                                                       prefix=prefix);
                      return(argList);
                  };
                  ## There are no connections for an atomic gate, and the
                  ## gateList is unnecessary, too.
                  .Object@gateList <- list();
                  .Object@cnxnList <- cnxnList();
              }

              ## Make a little recursive function to generate a statelist
              ## from the io object and the gateList.
              makeState <- function(io, gateList=list()) {
                  out <- gateIOList(this=io);

                  if (length(gateList) > 0) {
                      for (gateName in names(gateList)) {
                          out <- add(out,
                                     tag=gateName,
                                     sublist=makeState(gateList[[gateName]]@io,
                                                       gateList[[gateName]]@gateList))
                      }
                  }
                  return(out);
              }

              .Object@stateList <- makeState(.Object@io, .Object@gateList);

              validObject(.Object);
              return(.Object);
          });

setMethod("formatVal",
          signature = "gate",
          definition= function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }
              ## Type and ID
              if (style == "verbose") {
                  outstr <- paste0(prefix,
                                   "type: ", object@type,
                                   ", id: ", object@id);
              } else {
                  outstr <- paste0(prefix, object@type, " (", object@id, ")");
              }

              ## The io list.
              outstr <- paste0(outstr, "\n", prefix, "io:\n");
              outstr <- paste0(outstr, formatVal(object@io,
                                                prefix=paste0(prefix, "  "),
                                                style=style));

              ## Any subsidiary gates, for a compound gate.
              if (length(object@gateList) > 0) {
                  outstr <- paste0(outstr, "\n", prefix, "gateList:\n");
                  for (name in names(object@gateList)) {
                      outstr <- paste0(outstr, prefix, name, ":\n",
                                       formatVal(object@gateList[[name]],
                                                 prefix=paste0(prefix, "| "),
                                                 style=style),
                                      "\n");
                  }

                  ## Also the connections.
                  outstr <- paste0(outstr, prefix, "cnxnList:\n");
                  outstr <- paste0(outstr,
                                   formatVal(object@cnxnList,
                                             prefix=paste0(prefix, "  "),
                                             style=style));
              }

              ## We ignore the other slots, at least for now.  Mostly the
              ## slots with lists are for internal bookkeeping functions and
              ## the methods (definition, transformOnce, transform) can be
              ## examined by experiment.

              return(outstr);
          });

setMethod("show",
          signature="gate",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });


setMethod("showShape",
          signature="gate",
          definition=function(object) {
              cat("color    = ", object@color, "\n");
              cat("shape    = ", object@shape, "\n", sep="");
              cat("fillcolor= ", object@fillcolor, "\n", sep="");
              cat("fontcolor= ", object@fontcolor, "\n", sep="");
              cat("fontname = ", object@fontname, "\n", sep="");
              cat("penwidth = ", object@penwidth, "\n", sep="");
              cat("style    = ", object@style, "\n", sep="");
          });

setMethod("describe",
          signature="gate",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });


## Some testing of the gate object.
g.xor <- gate(def=function(...) {
    args <- list(...)[[1]];

    if (getVal(args[["in1"]]) == getVal(args[["in2"]])) {
        return(list("out"=gval("0")));
    } else {
        return(list("out"=gval("1")));
    }
},
io=gateIO(i=list("in1"=gval(type=binary), "in2"=gval(type=binary)),
          o=list("out"=gval(type=binary))));

g.or <- gate(def=function(...) {
    args <- list(...)[[1]];

    if ((getVal(args[["in1"]]) == "1") || (getVal(args[["in2"]]) == "1")) {
        return(list("out"=gval("1")));
    } else {
        return(list("out"=gval("0")));
    }
},
io=gateIO(i=list("in1"=gval(type=binary), "in2"=gval(type=binary)),
          o=list("out"=gval(type=binary))));

g.and <- gate(def=function(...) {
    args <- list(...)[[1]];

    if ((getVal(args[["in1"]]) == "1") && (getVal(args[["in2"]]) == "1")) {
        return(list("out"=gval("1")));
    } else {
        return(list("out"=gval("0")));
    }
},
io=gateIO(i=list("in1"=gval(type=binary), "in2"=gval(type=binary)),
          o=list("out"=gval(type=binary))));

g.comp <- gate(gateList=list("AND1"=g.and, "AND2"=g.and, "XOR1"=g.xor),
               cnxn=cnxnList("in1"=cnxns("AND1:in1", "AND2:in2"),
                             "in2"=cnxns("AND2:in1", "AND1:in2"),
                             "AND1:out"="XOR1:in1",
                             "AND2:out"="XOR1:in2",
                             "XOR1:out"="out"),
               io=gateIO(i=list("in1"=gval(type=binary),
                                "in2"=gval(type=binary)),
                         o=list("out"=gval(type=binary))));

g.comp2 <- gate(gateList=list("AND1"=g.and, "AND2"=g.and, "XOR1"=g.xor,
                              "C1"=g.comp),
                cnxn=cnxnList("in1"=cnxns("AND1:in1", "AND2:in2"),
                              "in2"=cnxns("AND2:in1", "AND1:in2"),
                              "AND1:out"="XOR1:in1",
                              "AND2:out"=cnxns("XOR1:in2", "C1:in2"),
                              "XOR1:out"="C1:in1",
                              "C1:out"="out"),
                io=gateIO(i=list("in1"=gval(type=binary),
                                 "in2"=gval(type=binary)),
                          o=list("out"=gval(type=binary))));



##                           cl <- cnxnList("AND2:out"="AND3:in1", "AND3.out"="OR1:in1,OR2:in2");
## cl <- add(cl, "AND3.out"="AND4:in2");
## cl <- add(cl, "in1"="AND1:in1");

## These are more like tests of gateIO and gval
if (formatVal(g.or@transformOnce(gateIOList("in1"=gval("1"), "in2"=gval("1"), "out"=gval(type=binary)))) != "this:\n I: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\n O: out=1 (symbol: 0/1)")
    stop("gate problem.");

if (formatVal(g.or@transformOnce(gateIOList("in1"=gval("1"), "out"=gval(type=binary)))) != "this:\n I: in1=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("second gate problem.");

if (formatVal(g.or@transformOnce(gateIOList("in1"=gval("1"), "out"=gval(type=binary)))) != "this:\n I: in1=1 (symbol: 0/1)\n O: out=[empty] (symbol: 0/1)")
    stop("third gate problem.");


############################################################################
## PROBABILISTIC RESULTS
##
## Now we turn to representing a gate as a truth table, an array of potential
## inputs, and a set of outputs and probabilities associated with those
## outputs.
##
## A discrete output can be represented as a collection of potential values,
## and associated probabilities for each one.  But a continuous output must
## have a set of ranges, and probabilities for them.  This is addressed by
## having N probabilities for N discrete symbols and N+1 probabilities for N
## continuous values.
##
## The "probabilities" here are just counts.  When we go to print them out,
## the probabilities can be readily calculated, and there are methods for
## incrementing the counts.
outcome <- setClass(
    "outcome",
    slots=c(keyVal="gval",
            vals="list",
            counts="numeric",
            Nbins="numeric",
            type="character"),
    validity=function(object) {
        if (class(object@keyVal) != "gval")
            return("keyVal must be a gval.");

        if (length(object@vals) <= 0)
            return("Error in determining vals.");

        if (length(object@counts) <= 0)
            return("Something amiss with the counts.");

        return(TRUE);
    });

## Use like this: outcome(gval(type="binary")) or
##                outcome(gval(type="float"),Nbins=5) or
##                outcome(float)
## You can preset the counts with this:
##                outcome(gval(type="float"),Nbins=3, counts=c(2,3,4))
setMethod("initialize",
          signature="outcome",
          definition=function(.Object, ...) {
              args <- list(...);

              if (class(args[[1]]) == "gval") {
                  .Object@keyVal <- args[[1]];
              } else if (class(args[[1]]) == "type") {
                  .Object@keyVal <- gval(type=args[[1]]);
              }

              if (.Object@keyVal@type@baseType == "symbol") {
                  .Object@type <- "discrete";
              } else {
                  .Object@type <- "continuous";
              }

              if (.Object@type == "discrete") {
                  .Object@vals <- as.list(.Object@keyVal@type@range);
                  .Object@counts <- rep(0, length(.Object@vals));
                  .Object@Nbins <- length(.Object@vals);
              } else {
                  Nbins <- grepl("^N", names(args));
                  if (sum(Nbins) == 1) {
                      .Object@Nbins <- args[[which(Nbins)]];
                  } else {
                      .Object@Nbins <- 4; # Default value.
                  }
                  .Object@vals <- as.list(seq(.Object@keyVal@type@min,
                                              .Object@keyVal@type@max,
                                              length.out=(.Object@Nbins + 1)));
                  .Object@counts <- rep(0, .Object@Nbins);
              }

              ## Optionally we can seed the starting probability counts.
              counts <- grepl("^c", names(args));
              if (sum(counts) == 1) {
                  probIndex <- which(counts);

                  if (class(args[[probIndex]]) != "numeric")
                      stop("probability inputs must be numeric.");

                  .Object@counts <- args[[probIndex]];
              }

              validObject(.Object);
              return(.Object);
          });

## formatVal(outcome, prefix="", count=FALSE).  Set count to TRUE to see
## counts instead of probabilities.
setMethod("formatVal",
          signature="outcome",
          definition=function(object, ...) {
              args <- list(...);

              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              count <- FALSE;
              if ("count" %in% names(args)) {
                  count <- args$count;
              }

              outstr <- "";
              if (style == "verbose") {
                  outstr <- paste0(outstr, "type: ", object@type, " (",
                                   formatVal(object@keyVal@type), ")\n");
              }

              if (object@type == "discrete") {
                  for (i in 1:length(object@vals)) {
                      if (count) {
                          P <- object@counts[i];
                      } else {
                          if (count) {
                              P <- object@counts[i];
                          } else {
                              P <- format(probs(object)[i], digits=4);
                          }
                      }
                      outstr <- paste0(outstr, object@vals[[i]],
                                       " (", P, ") ");
                  }
              } else {
                  outstr <- paste0(outstr, "[", collapse="");
                  for (i in 1:length(object@counts)) {
                      if (count) {
                          P <- object@counts[i];
                      } else {
                          if (count) {
                              P <- object@counts[i];
                          } else {
                              P <- format(probs(object)[i], digits=4);
                          }
                      }
                      outstr <- paste0(outstr, object@vals[[i]],
                                       " (", P, ") ");
                  }
                  outstr <- paste0(outstr,
                                   object@vals[[length(object@vals)]],
                                   "]",
                                   collapse="");
              }
              return(outstr);
          });

setMethod("show",
          signature="outcome",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="outcome",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

setMethod("showCount",
          signature="outcome",
          definition=function(object) {
              cat(formatVal(object, count=TRUE), "\n");
          });

## Returns the probabilities implied by the outcome object.
setMethod("probs",
          signature="outcome",
          definition=function(object, ...) {
              if (sum(object@counts) == 0) {
                  return(rep(0, length(object@counts)));
              } else {
                  return(object@counts/sum(object@counts));
              }
          });

## Returns an estimate of the number of bits of information in the variable
## recorded by the outcome object.  Obviously for a continuous variable this
## is a pretty crude estimate.
setMethod("info",
          signature="outcome",
          definition=function(object, ...) {
              xlogx <- function(x) {
                  if (x == 0)
                      return(0)
                  else
                      return(x * log(x, base=2));
              }

              return(-sum(sapply(probs(object), xlogx)));
          });

## Use this function to add a result to the record kept in a given outcome
## object.  See the tests below for examples.  The input can be a raw value
## or a gval.  There is type checking using the outcome object's keyVal@type.
setMethod("record",
          signature="outcome",
          definition=function(object, inVal, inspect=FALSE) {
              if (class(inVal) == "gval") {
                  if (inVal@type@baseType != object@keyVal@type@baseType) {
                      stop("Bad type of input.");
                  }
                  val <- getVal(inVal);
              } else {
                  if (!check(object@keyVal, inVal)) {
                      stop("Input bad type or out of range.");
                  }
                  val <- inVal;
              }

              if (object@type == "discrete") {

                  i <- which(val == object@vals)
                  object@counts[i] <- object@counts[i] + 1;

              } else {
                  i <- object@Nbins;
                  while (val < object@vals[[i]]) i <- i - 1;
                  object@counts[i] <- object@counts[i] + 1;
              }
              validObject(object);
              return(object);
          })



## Testing outcome

## Preliminary outcome tests
ocf <- outcome(gval(type=float),count=c(10,10,5,5))
if (formatVal(ocf) != "[0 (0.3333) 0.25 (0.3333) 0.5 (0.1667) 0.75 (0.1667) 1]")
    stop("continuous outcome problem.");
oc <- outcome(gval(type=binary),count=c(10,15))
if (formatVal(oc) != "0 (0.4) 1 (0.6) ")
    stop("discrete outcome problem.");
if (formatVal(record(oc, "0")) != "0 (0.4231) 1 (0.5769) ")
    stop("record outcome problem.");

############################################################################
## INPUTS AND PROBABILISTIC RESULTS
##
## A version of gateIO, but that records probabilistic outcomes rather than
## just the type and value.
gateProbs <- setClass(
    "gateProbs",
    slots=c(inputs="list", outcomes="list", is.compat="function"),
    validity=function(object) {
        if (class(object@inputs) != "list")
            return("inputs must be a list of gvals.");
        if (sum(sapply(object@inputs, function(x) {class(x) == "gval"})) !=
            length(object@inputs))
            return("All elements of inputs must be gvals.");

        if (class(object@outcomes) != "list")
            return("outcome must be a list.");
        if (length(object@outcomes) < 1)
            return("Need at least one outcome in the gateProbs object.");
        if (sum(sapply(object@outcomes, function(x) {class(x) == "outcome"})) !=
            length(object@outcomes))
            return("All elements of outcomes must be outcome.");
        return(TRUE);
    });

## Use like this: gateProbs(i=list("in1"=gval("0"),"in2"=gval("1")),
##                          o=list("out"=outcome(gval(type=binary),c=c(15,10))))
## OR:    gateProbs(template=gateIO, Nbins=4)
setMethod("initialize",
          signature="gateProbs",
          definition=function(.Object, ...) {
              args <- list(...);

              ## This option uses a 'template' gateIO object to set up the
              ## inputs to this gateProbs, and the outcome objects, too. It
              ## does not touch the output values (though see the commented
              ## out part below).  After setting up the template, use
              ## 'record' to add values.
              temp <- grepl("^t", names(args)); # Looking for "template"
              if (sum(temp) == 1) {
                  templateIndex <- which(temp);
                  if (length(templateIndex) > 1) {
                      stop("Ambiguous argument ('t') to constructor.");
                  }

                  if (class(args[[templateIndex]]) != "gateIO")
                      stop("Template arg must be a gateIO object.");

                  Nbins <- 4;
                  nb <- grepl("^N", names(args)); # Looking for "Nbins"
                  if (sum(nb) == 1) {
                      nbindex <- which(nb);
                      if (length(templateIndex) > 1) {
                          stop("Ambiguous argument ('Nbins') to constructor.");
                      }

                      Nbins <- args[[nbindex]];
                  }

                  .Object@inputs <- args[[templateIndex]]@inputs;
                  .Object@outcomes <- list();
                  template <- args[[templateIndex]];
                  for (outname in names(template@outputs)) {
                      .Object@outcomes[[outname]] <-
                          outcome(template@outputs[[outname]]@type,
                                  Nbins=Nbins);
                      ## Uncomment this to seed the gateProbs outcomes with
                      ## the template output values.
                      ## if (!is.empty(template@outputs[[outname]])) {
                      ##     .Object@outcomes[[outname]] <-
                      ##         record(.Object@outcomes[[outname]],
                      ##                getVal(template@outputs[[outname]]));
                      ## }
                  }
              }

              input <- grepl("^i", names(args));
              if (sum(input) == 1) { ## There's one arg that starts with 'i'.
                  inputIndex <- which(input);
                  if (length(inputIndex) > 1) {
                      stop("Ambiguous argument to constructor.");
                  }

                  ## A modest amount of type checking, though more is done
                  ## in the validity tester.
                  if (class(args[[inputIndex]]) != "list")
                      stop("gateProbs inputs must be a list.");
                  if (class(args[[inputIndex]][[1]]) != "gval")
                      stop("gateProbs inputs must be a list of gvals.");

                  .Object@inputs <- args[[inputIndex]];
              } else if (sum(input) > 1) {
                  stop("gateIO objects forbid multiple input lists.");
              }

              outcomes <- grepl("^o", names(args));
              if (sum(outcomes) == 1) { ## There's one arg that starts with 'o'.
                  outcomeIndex <- which(outcomes)
                  if (length(outcomeIndex) > 1) {
                      stop("Ambiguous argument to constructor.");
                  }

                  if (class(args[[outcomeIndex]]) != "list")
                      stop("gateProbs outcomes must be a list.");
                  if (class(args[[outcomeIndex]][[1]]) != "outcome")
                      stop("gateProbs outcomes must be a list of outcome.");

                  .Object@outcomes <- args[[outcomeIndex]];
              } else if (sum(outcomes) > 1) {
                  stop("gateProbs object forbids multiple outcome lists.");
              }

              .Object@is.compat <- function(inVal, inspect=FALSE) {
                  if ((class(inVal) != "gateIO") &&
                      (class(inVal) != "gateProbs")) {
                      if (inspect) {
                          cat("input must be a gateIO or gateProbs object,");
                          cat(" not:", class(inVal), "\n");
                      }
                      return(FALSE);
                  }

                  if (!identical(names(.Object), names(inVal))) {
                      if (inspect)
                          cat("The object to be recorded is not congruent.\n");
                      return(FALSE);
                  }

                  for (name in names(inVal@inputs)) {

                      if (!identical(inVal[[name]]@type, .Object[[name]]@type)){
                          if (inspect) {
                              cat("Type mismatch between input args.\n");
                              cat("One:", formatVal(inVal[[name]]@type),
                                  "The other:", formatVal(.Object[[name]]@type),
                                  "\n");
                          }
                          return(FALSE);
                      }

                      if (!(getVal(inVal[[name]]) == getVal(.Object[[name]]))) {
                          if (inspect)
                              cat("Value mismatch in input args.\n");
                          return(FALSE);
                      }
                  }

                  if (class(inVal) == "gateIO") {
                      for (name in names(inVal@outputs)) {
                          if (!identical(inVal[[name]]@type,
                                         .Object[[name]]@keyVal@type)){
                              if (inspect) {
                                  cat("Type mismatch between output args.\n");
                              }
                              return(FALSE);
                          }
                      }
                  } else if (class(inVal) == "gateProbs") {
                      for (name in names(inVal@outcomes)) {
                          if (!identical(inVal[[name]]@keyVal@type,
                                         .Object[[name]]@keyVal@type)){
                              if (inspect) {
                                  cat("Type mismatch between output args.\n");
                                  cat("One:", formatVal(inVal[[name]]@keyVal@type),
                                  "The other:", formatVal(.Object[[name]]@type),
                                  "\n");
                              }
                              return(FALSE);
                          }
                      }
                  }
                  return(TRUE);
              };


              validObject(.Object);
              return(.Object);
          });

setMethod("formatVal",
          signature="gateProbs",
          definition=function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              count <- FALSE;
              if ("count" %in% names(args)) {
                  count <- args$count;
              }

              if (style == "verbose") {
                  outstr <- "";
                  out <- c();
                  if (length(object@inputs) > 0) {
                      for (i in 1:length(object@inputs)) {
                          out <- c(out, paste0(prefix, "  ",
                                               names(object@inputs)[i], ": ",
                                               formatVal(object@inputs[[i]])));
                      }
                      outstr <- paste0(prefix, "inputs:\n",
                                       paste0(out, collapse="\n"));
                  }
                  if (outstr != "") outstr <- paste0(outstr, "\n");

                  out <- c();
                  if (length(object@outcomes) > 0) {
                      for (i in 1:length(object@outcomes)) {
                          out <- c(out, paste0(prefix, "  ",
                                               names(object@outcomes)[i], ": ",
                                               formatVal(object@outcomes[[i]],
                                                         count=count)));
                      }
                      outstr <- paste0(outstr, prefix, "outcomes:\n",
                                       paste0(out, collapse="\n"));
                  }

              } else {
                  out <- c();
                  if (length(object@inputs) > 0) {
                      for (i in 1:length(object@inputs)) {
                          out <- c(out, paste0(names(object@inputs)[i], "=",
                                               formatVal(object@inputs[[i]])));
                      }
                      outstr <- paste0(prefix, "I: ",
                                       paste0(out, collapse=", "));
                  }

                  if (outstr != "") outstr <- paste0(outstr, "\n");

                  out <- c();
                  if (length(object@outcomes) > 0) {
                      for (i in 1:length(object@outcomes)) {
                          out <- c(out, paste0(names(object@outcomes)[i], "=",
                                               formatVal(object@outcomes[[i]],
                                                         count=count)));
                      }
                      outstr <- paste0(outstr, prefix, "O: ",
                                       paste0(out, collapse=", "));
                  }
              }

              return(outstr);
          });

setMethod("show",
          signature="gateProbs",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="gateProbs",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

setMethod("showCount",
          signature="gateProbs",
          definition=function(object) {
              cat(formatVal(object, count=TRUE), "\n");
          });

setMethod("names",
          signature="gateProbs",
          definition=function(x) { return(c(names(x@inputs),
                                            names(x@outcomes))); });
setMethod("[[",
          signature="gateProbs",
          definition=function(x, i, j, ...) {

              if (class(i) != "character")
                  stop("Use a name to select a gateProbs object.");

              if (i %in% names(x@inputs)) return(x@inputs[[i]]);
              if (i %in% names(x@outcomes)) return(x@outcomes[[i]]);
          });

## Records a gateIO value in a gateProbs object.  Makes sure they match.
setMethod("record",
          signature="gateProbs",
          definition=function(object, inVal, inspect=FALSE) {

              if (object@is.compat(inVal, inspect=inspect)) {
                  for (outName in names(inVal@outputs)) {
                      object@outcomes[[outName]] <-
                          record(object@outcomes[[outName]],
                                 getVal(inVal[[outName]]));
                  }
              }

              validObject(object);
              return(object);
          });


## Testing gateProbs
gp <- gateProbs(i=list("in1"=gval("0"),"in2"=gval("1")),
                o=list("out"=outcome(gval(type=binary),c=c(15,10))))
if (formatVal(gp) != "I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\nO: out=0 (0.6) 1 (0.4) ")
    stop("gp problem.");

gp <- record(gp, gateIO(i=list("in1"=gval("0"),"in2"=gval("1")),o=list("out"=gval("1",type=binary))));
if (formatVal(gp) != "I: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\nO: out=0 (0.5769) 1 (0.4231) ")
    stop("gp record problem.");

gpf <- gateProbs(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                 o=list("out"=outcome(gval(type=float),c=c(10,10,5,5))))
if (formatVal(gpf) != "I: in1=0.2 (float: min: 0, max: 1), in2=0.7 (float: min: 0, max: 1)\nO: out=[0 (0.3333) 0.25 (0.3333) 0.5 (0.1667) 0.75 (0.1667) 1]")
    stop("gp float problem");

gpt <- gateProbs(t=gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.3, type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.5, type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.7, type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.7, type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.7, type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.7, type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.7, type=float))));
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.2, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.7, type=float))));
## This one has different input values, so shouldn't count.
gpt <- record(gpt, gateIO(i=list("in1"=gval(0.1, float),"in2"=gval(0.7, float)),
                          o=list("out"=gval(0.9, type=float))));

if (formatVal(gpt) != "I: in1=0.2 (float: min: 0, max: 1), in2=0.7 (float: min: 0, max: 1)\nO: out=[0 (0) 0.25 (0.125) 0.5 (0.875) 0.75 (0) 1]")
    stop("gpt template gateProbs problem.");

############################################################################
## TRUTH TABLE
##
## An object to hold a truth table, with a column for each input and columns
## of outcome objects to record the probabilities of different outcomes.  We
## use a vector of gateProbs objects as input to record the trials.  Each
## gateProbs object in the input vector should have the same array of data
## values and names.
##
## This does not contain a data frame, but sort of imitates one, and has a
## data frame export capacity.  It contains a list of gateProbs objects, with
## compatible inputs and outcomes, and has an add function that will either
## accept and add an input-incompatible new gateProbs to the list, or a
## gateIO object that is compatible with some gateProbs in the list, in which
## case it adds its output counts to that compatible existing gateProb.

truthTable <- setClass(
    "truthTable",
    slots=c(data="list"),
    validity=function(object) {
        if (class(object@data) != "list")
            return("Object data should be a list.");
        if ((length(object@data) > 0) &&
            (sum(sapply(object@data, function(x) {class(x) == "gateProbs"})) !=
             length(object@data)))
            return("All elements of data list must be gateProbs.");
        return(TRUE);
    });


## Use like this: truthTable(gateProbs.1,gateProbs.2,gateProbs.3)
##            or: truthTable(gateIO.1,gateIO.2,gateIO.3)
setMethod("initialize",
          signature="truthTable",
          definition=function(.Object, ...) {
              args <- list(...);

              ## args must be uniform, either gateIO objects or gateProbs.
              if ((length(args) > 0) &&
                  (sum(sapply(args, function(x) { class(x) == "gateProbs"})) ==
                   length(args))) {

                  ## All gateProbs.  Add these all to the list, but ignore
                  ## duplicates (input lists identical).
                  for (newItem in args) {
                      if (length(.Object@data) < 1) {
                          .Object@data[[1]] <- newItem;
                      } else {
                          comps <- sapply(.Object@data,
                                          function(x) {newItem@is.compat(x)});
                          ## If there are no matches, add the object.
                          if (sum(comps) == 0) {
                              .Object@data[[length(.Object@data)+1]] <- newItem;
                          }
                      }
                  }
              } else if ((length(args) > 0) &&
                         (sum(sapply(args,
                                     function(x) { class(x) == "gateIO"})) ==
                          length(args))) {
                  ## All gateIO.  Add these by creating a compatible
                  ## gateProbs object for each one.  Record the added
                  ## objects, too, and if there are repeats, just record
                  ## them.
                  Nbins <- 4;
                  if ("Nbins" %in% names(args)) Nbins <- args[["Nbins"]];
                  for (newItem in args) {
                      if (length(.Object@data) < 1) {
                          .Object@data[[1]] <- gateProbs(template=newItem,
                                                         Nbins=Nbins);
                          .Object@data[[1]] <- record(.Object@data[[1]],
                                                      newItem);
                      } else {
                          comps <- sapply(.Object@data,
                                          function(x) {x@is.compat(newItem)});
                          if (sum(comps) == 0) {
                              .Object@data[[length(.Object@data)+1]] <-
                                  gateProbs(template=newItem,
                                            Nbins=Nbins);
                              .Object@data[[length(.Object@data)]] <-
                                  record(.Object@data[[length(.Object@data)]],
                                         newItem);
                          } else if (sum(comps) == 1) {
                              ## There already is a compatible gateProbs.
                              ## Record it.
                              comp <- which(comps);
                              .Object@data[[comp]] <-
                                  record(.Object@data[[comp]], newItem);
                          } else {
                              stop("Multiple compatible gateProbs, which is bad.");
                          }
                      }
                  }
              } else if (length(args) == 0) {
                  ## An empty truth table is allowed.
              } else {
                  stop("Arg list must be uniform, either gateIO or gateProbs.");
              }

              validObject(.Object);
              return(.Object);
          });


setMethod("formatVal",
          signature="truthTable",
          definition=function(object, ...) {
              args <- list(...);
              prefix <- "";
              if ("prefix" %in% names(args)) {
                  prefix <- args$prefix;
              }

              count <- FALSE;
              if ("count" %in% names(args)) {
                  count <- args$count;
              }

              outstr <- "";
              if (length(object@data) > 0) {
                  for (i in 1:length(object@data)) {
                      outstr <-
                          paste0(outstr, "[[", i, "]]\n",
                                formatVal(object@data[[i]], count=count),
                                "\n\n", collapse="");
                  }
              }

              return(outstr);
          });

setMethod("show",
          signature="truthTable",
          definition=function(object) {
              cat(formatVal(object), "\n");
          });

setMethod("describe",
          signature="truthTable",
          definition=function(object) {
              cat(formatVal(object, style="verbose"), "\n");
          });

setMethod("showCount",
          signature="truthTable",
          definition=function(object) {
              cat(formatVal(object, count=TRUE), "\n");
          });


setMethod("[[",
          signature="truthTable",
          definition=function(x, i, j, ...) {
              if (class(i) == "gateIO") {
                  comps <- sapply(x@data, function(y) {y@is.compat(i);})
                  comp <- which(comps);
                  if (length(comp) == 1) {
                      return(x@data[[comp]]);
                  } else if (length(comp) == 0) {
                      stop("Incompatible with current list.");
                  } else {
                      stop("Too many compatible entries.");
                  }
              } else {
                  return(x@data[[i]]);
              }
          });

## You can use this one to record a gateIO object
setMethod("[[<-",
          signature="truthTable",
          definition=function(x, i, j, ..., value) {

              if (class(i) == "gateIO") {
                  comps <- sapply(x@data, function(y) {y@is.compat(i);})
                  comp <- which(comps);
                  if (length(comp) > 1) stop("Too many compatible entries.");
              } else {
                  comp <- i;
              }

              if (class(value) == "gateProbs") {
                  x@data[[comp]] <- value;
              } else if (class(value) == "gateIO") {
                  x@data[[comp]] <- record(x@data[[comp]], value);
              } else {
                  cat("value must be a gateProbs or compatible gateIO object.\n");
                  stop();
              }
              return(x);
          });

## Add either a gateProbs object, or a gateIO.  If the gateIO is not
## compatible with anyone, create a new gateProbs for it.  If it is, just
## record it.
setMethod("add",
          signature = "truthTable",
          definition = function(object, ...) {
              args <- list(...);

              Nbins <- 4;
              if ("Nbins" %in% names(args)) Nbins <- args[["Nbins"]];

              for (arg in args) {

                  if (class(arg) == "gateProbs") {
                      comps <- sapply(object@data,
                                      function(x) {x@is.compat(arg)});
                      if ((length(comps) == 0) || (sum(comps) == 0)) { #
                          ## This is new; add it.
                          object@data[[length(object@data) + 1]] <- arg;
                      } # Otherwise, ignore it.

                  } else if (class(arg) == "gateIO") {

                      comps <- sapply(object@data,
                                      function(x) {x@is.compat(arg)});
                      if ((length(comps) == 0) || (sum(comps) == 0)) {
                          ## This is new; add it.
                          new <- length(object@data) + 1;
                          ## Add an object of the correct shape.
                          object@data[[new]] <-
                              gateProbs(template=arg, Nbins=Nbins);
                          ## Record the data in it.
                          object@data[[new]] <-
                              record(object@data[[new]], arg);

                      } else if (sum(comps) == 1) {
                          ## This is not new; just record the arg.
                          comp <- which(comps);
                          object@data[[comp]] <- record(object@data[[comp]],
                                                        arg);
                      }
                  } else if (class(arg) == "gateIOList") {
                      ## Go through the list and add each gateIO individually.
                      for (g in arg@data) {
                          object <- add(object, g);
                      }
                  }
              }
              return(object);
          });


setMethod("length",
          signature="truthTable",
          definition=function(x) { return(length(x@data)); });

setMethod("export",
          signature="truthTable",
          definition=function(object) {
              if (class(object) != "truthTable") stop("bad class.");

              ## Collect the names from the first gateProbs object in the table.
              cnames <- names(object@data[[1]]@inputs);
              for (outName in names(object@data[[1]]@outcomes)) {
                  outc <- object@data[[1]]@outcomes[[outName]];
                  if (outc@type == "discrete") {
                      for (val in outc@keyVal@type@range) {
                          cnames <- c(cnames, paste(outName, val, sep="."));
                      }
                  } else {
                      for (i in 1:outc@Nbins) {
                          cnames <- c(cnames,
                                      paste(outName,
                                            paste0("B", i),
                                            sep="."));
                      }
                  }
              }

              ## Got the names, now assemble the data.
              outFrame <- data.frame();
              for (gp in object@data) {
                  newRow <- list();
                  for (i in 1:length(gp@inputs)) {
                      newRow[[i]] <- getVal(gp@inputs[[i]]);
                  }
                  for (i in 1:length(gp@outcomes)) {
                      for (count in gp@outcomes[[i]]@counts) {
                          newRow[[length(newRow) + 1]] <- count;
                      }
                  }
                  names(newRow) <- cnames;
                  if (dim(outFrame)[1] == 0) {
                      outFrame <- data.frame(newRow,
                                             stringsAsFactors=FALSE);
                  } else {
                      outFrame <- rbind(outFrame,
                                        data.frame(newRow,
                                                   stringsAsFactors=FALSE));
                  }
              }

              return(outFrame);
          });

## Testing truthTable.
tt <- truthTable();
tt <- add(tt, g.and@transformOnce(gateIOList("in1"=gval("0"),"in2"=gval("0"))));
tt <- add(tt, g.and@transformOnce(gateIOList("in1"=gval("0"),"in2"=gval("1"))));
tt <- add(tt, g.and@transformOnce(gateIOList("in1"=gval("1"),"in2"=gval("0"))));
tt <- add(tt, g.and@transformOnce(gateIOList("in1"=gval("1"),"in2"=gval("1"))));
tt <- add(tt, g.and@transformOnce(gateIOList("in1"=gval("1"),"in2"=gval("1"))));
tt <- add(tt, g.and@transformOnce(gateIOList("in1"=gval("1"),"in2"=gval("1"))));


if (formatVal(tt) != "[[1]]\nI: in1=0 (symbol: 0/1), in2=0 (symbol: 0/1)\nO: out=0 (1) 1 (0) \n\n[[2]]\nI: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\nO: out=0 (1) 1 (0) \n\n[[3]]\nI: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\nO: out=0 (1) 1 (0) \n\n[[4]]\nI: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\nO: out=0 (0) 1 (1) \n\n")
    stop("tt truthTable problem.");
if (formatVal(tt, count=TRUE) != "[[1]]\nI: in1=0 (symbol: 0/1), in2=0 (symbol: 0/1)\nO: out=0 (1) 1 (0) \n\n[[2]]\nI: in1=0 (symbol: 0/1), in2=1 (symbol: 0/1)\nO: out=0 (1) 1 (0) \n\n[[3]]\nI: in1=1 (symbol: 0/1), in2=0 (symbol: 0/1)\nO: out=0 (1) 1 (0) \n\n[[4]]\nI: in1=1 (symbol: 0/1), in2=1 (symbol: 0/1)\nO: out=0 (0) 1 (3) \n\n")
    stop("tt truthTable count problem.");


## Returns a list of gate names from a gate.  The return list has the
## fully-qualified name as the key and the local name as the value:
## e.g. out[["C1.C2.AND"]] == list(label="AND",style="gate") There is also a
## style=terminal for the bare ins and outs of compound gates.
gate.exportGraphNodes <- function(g, name="") {

    if (g@type == "atomic") {
        if (name == "") stop("Need a name for atomic gates.");
        outList <- list();
        outList[[name]] <- list(label=name, style="gate");
        return(outList);
    } else {
        outList <- list();
        subio <- list();
        for (ioname in names(g@io)) {
            if (name == "") {
                pname <- ioname;
            } else {
                pname <- paste(name, ioname, sep=".");
            }
            outList[[pname]] <- list(label=ioname, style="terminal");
        }

        for (i in 1:length(g@gateList)) {
            subGates <- gate.exportGraphNodes(g@gateList[[i]],
                                        name=names(g@gateList)[i]);

            if (name != "")
                for (j in 1:length(subGates))
                    names(subGates)[j] <- paste(name,
                                                subGates[[j]]$label, sep=".");

            outList <- append(outList, subGates);
        }
        return(outList);
    }
}


## Exports a graphViz description of the network instantiated by the gate.
## You'll want to offer a name for the root gate.  The gates within a
## compound gate have names, but the main one does not, so you need to
## specify it in the argument list.
gate.exportGraph <- function(g, name="gate") {
    out <- "digraph gt { graph [overlap=true, fontsize=10, rankdir=LR]\n";

    graphNodes <- gate.exportGraphNodes(g, name);
    for (i in 1:length(graphNodes)) {
        if (graphNodes[[i]]$style == "terminal") {
            out <- paste0(out, "node [shape=circle, style=filled, fillcolor=lightblue, fontname=Helvetica, fontsize=10, label=\"", graphNodes[[i]]$label, "\"] \"", names(graphNodes)[i], "\";\n");
        } else {
            out <- paste0(out, "node [shape=triangle, orientation=270, style=filled, fillcolor=yellow, fontname=Helvetica, fontsize=10, label=\"", graphNodes[[i]]$label, "\"] \"", names(graphNodes)[i], "\";\n");
        }
    }

    if (length(g@cnxnList) > 0) {
        for (i in 1:length(g@cnxnList)) {
            for (j in 1:length(g@cnxnList[[i]])) {
                src <- strsplit(names(g@cnxnList)[i], ":")[[1]];
                sink <- strsplit(names(g@cnxnList[[i]])[j], ":")[[1]];

                out <- paste0(out, src[1], ":e -> ", sink[1]);
                if (length(src) > 1) {
                    if (length(sink) > 1) {
                        out <- paste0(out, " [fontname=Helvetica, fontsize=8, arrowType=\"vee\", taillabel=\"", src[2],
                                     "\",headlabel=\"", sink[2], "\"]\n");
                    } else {
                        out <- paste0(out, " [fontname=Helvetica, fontsize=8, arrowType=\"open\", taillabel=\"", src[2], "\"]\n");
                    }
                } else {
                    out <- paste0(out, " [fontname=Helvetica, fontsize=8, arrowType=\"open\", headlabel=\"", sink[2], "\"]\n");
                }
            }
        }
    }

    return(paste(out, "}"));
}

## ADD TESTS HERE.

## Create a data frame of nodes suitable for use with DiagrammeR graphs.  The
## recurse flag indicates whether component gates are expanded into their
## components, or treated as a single node.
##
nodeIDCounter <- 0;
gate.makeNodeList <- function(g, recurse=FALSE, prefix="") {

    out.nodeList <- data.frame(stringsAsFactors=FALSE);

    ## if prefix is nothing, then we are at the top level, and the 'this'
    ## nodes should be drawn independently.  Otherwise they are not separate.

    if (prefix == "") {
        for (inName in names(g@io@inputs)) {

            nodeIDCounter <<- nodeIDCounter + 1;
            assign("nodeIDCounter", nodeIDCounter, envir=.GlobalEnv);

            out.nodeList <- rbind(out.nodeList,
                                  data.frame(label=inName,
                                             id=nodeIDCounter,
                                             gid=g@id,
                                             type="input",
                                             stringsAsFactors=FALSE));
        }
        for (outName in names(g@io@outputs)) {

            nodeIDCounter <<- nodeIDCounter + 1;
            assign("nodeIDCounter", nodeIDCounter, envir=.GlobalEnv);

            out.nodeList <- rbind(out.nodeList,
                                  data.frame(label=outName,
                                             id=nodeIDCounter,
                                             gid=g@id,
                                             type="output",
                                             stringsAsFactors=FALSE));
        }
    }

    if (length(g@gateList) > 0) {
        for (gName in names(g@gateList)) {

            nodeIDCounter <<- nodeIDCounter + 1;
            assign("nodeIDCounter", nodeIDCounter, envir=.GlobalEnv);

            if (prefix == "") {
                nodeName <- gName;
            } else {
                nodeName <- paste(prefix, gName, sep=".");
            }

            if ((g@gateList[[gName]]@type == "atomic") ||
                ((g@gateList[[gName]]@type == "compound") && (!recurse))) {
                out.nodeList <- rbind(out.nodeList,
                                      data.frame(label=nodeName,
                                                 id=nodeIDCounter,
                                                 gid=g@id,
                                                 type="gate",
                                                 stringsAsFactors=FALSE));
            } else {
                ## Recurse down into a subsidiary compound gate
                out.nodeList <- rbind(out.nodeList,
                                      gate.makeNodeList(g@gateList[[gName]],
                                                        prefix=nodeName,
                                                        recurse=recurse));
            }
        }
    }

    return(out.nodeList);
}




## The next few functions are meant for exporting the graph nature of the
## connection to a graph display and analysis package in R.  Using the
## "DiagrammeR" package, try these:
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

    ## Check gate type
    if (g@type == "atomic") {
        ## This is an atomic gate, just return a single line with its data.
        out.nodeList <- rbind(out.nodeList,
                              data.frame(label=prefix, id=uid,
                                         gid=g@id, type="G",
                                         stringsAsFactors=FALSE));
        uid <- uid + 1;
    } else {
        ## This is a composite gate, and g@gateList is a list the gates it
        ## contains.

        for (iname in names(g@io@inputs)) {
            prefixName <- iname;
            if (prefix != "") prefixName <- paste0(prefix, ".", iname);
            out.nodeList <- rbind(out.nodeList,
                                  data.frame(label=prefixName,
                                             id=uid,
                                             gid=g@id, type="IO",
                                             stringsAsFactors=FALSE));
            uid <- uid + 1;
        }

        ## Sort through the subsidiary gates...
        for (name in names(g@gateList)) {

            ## ... making a nodeList for each of them
            d <- gate.nodeList(g@gateList[[name]], prefix=name, uid=uid);

            ## The gate ID is arbitrary, just needs to be unique in this table.
            uid <- uid + dim(d)[1];

            if (prefix != "") {
                for (i in 1:length(d$label)) {
                    d$label[i] <- paste(prefix, d$label[i], sep=".");
                }
            }
            out.nodeList <- rbind(out.nodeList, d);
        }


        for (oname in names(g@io@outputs)) {
            prefixName <- oname;
            if (prefix != "") prefixName <- paste0(prefix, ".", oname);
            out.nodeList <- rbind(out.nodeList,
                                  data.frame(label=prefixName,
                                             id=uid, gid=g@id, type="IO",
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
    for (gateName in names(g@gateList)) {
        if (inspect) cat("processing:", gateName, "\n");

        if (prefix == "") {
            out.edgeList <- rbind(out.edgeList,
                                  gate.edgeList(g@gateList[[gateName]],
                                                prefix=gateName));
        } else {
            out.edgeList <- rbind(out.edgeList,
                                  gate.edgeList(g@gateList[[gateName]],
                                                prefix=paste(prefix, gateName, sep=".")));
        }

    }

    if (inspect) showList(out.edgeList);

    for (cnxnsName in names(g@cnxnList)) {

        ## There might be multiple sinks for any connection.  We trust that
        ## the ids were generated correctly so there is an id number for
        ## each connection sink.
        sinkNames <- names(g@cnxnList[[cnxnsName]]);
        sinkIDs <- c();
        sinkWts <- c();
        for (name in sinkNames) {
            sinkIDs <- c(sinkIDs, g@cnxnList[[cnxnsName]][[name]]@id);
            sinkWts <- c(sinkWts, g@cnxnList[[cnxnsName]][[name]]@weight);
        }

        if (prefix == "") {
            d <- data.frame(id=as.numeric(sinkIDs),
                            fromLabel=cnxnsName,
                            toLabel=sinkNames,
                            ##weight=as.numeric(sinkWts),
                            stringsAsFactors=FALSE);
        } else {
            d <- data.frame(id=as.numeric(sinkIDs),
                            fromLabel=paste(prefix, cnxnsName, sep="."),
                            toLabel=paste(prefix, sinkNames, sep="."),
                            ##weight=as.numeric(sinkWts),
                            stringsAsFactors=FALSE);
        }

        out.edgeList <- rbind(out.edgeList, d);
        if (inspect) showList(out.edgeList);

    }
    return(out.edgeList);
}

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
    ## A little recursive function to follow the 'from' nodes backwards.
    ## We go backwards because there's no forking in that direction.  Once
    ## these are found, I believe the forwards links left over are
    ## redundant.
    findOriginId <- function(fromString, edgeList, nodeList) {

        if (length(fromString) == 0) return(0);

        ## grepl returns an array of T/F. "sum" makes an ok "or".
        if (sum(grepl(paste0("^", fromString, "$"), nodeList$label))) {

            ## If we're here, we've found the string in the node list...
            fromLoc <- grep(paste0("^", fromString), nodeList$label);

            ## So return the corresponding index.
            return(nodeList$id[fromLoc]);

            ## If we haven't found it, try looking without the suffix.
        } else if (sum(grepl(paste0("^", dropLast(fromString), "$"),
                             nodeList$label))) {

            fromLoc <- grep(paste0("^", dropLast(fromString)),
                            nodeList$label);
            return(nodeList$id[fromLoc]);
        } else {
            ## We didn't find it in the nodelist, so grab the corresponding
            ## "to" and try again, recursively.
            toLoc <- grep(paste0("^", fromString), edgeList$toLabel);

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



## Sorts through the edge list and outputs an edge list that references the
## id field of the node list.
gate.simplify.edgeList <- function(edges, nodes) {


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
            if (grepl(paste0("^", nodes$label[i], "$"), fromShort)) {
                fromId <- nodes$id[i];
            }

            if (grepl(paste0("^", nodes$label[i], "$"), toShort)) {
                toId <- nodes$id[i];
            }
        }
        return(data.frame(from=fromId, to=toId));
    }

    edges <- mdply(edges, findId);


    ## By this point, we've got most of them, and the ones we don't have
    ## are marked with a zero.

    ## We have all the matches, except that lots of the "from" indications
    ## have to be traced back to their origin.
    for (i in 1:length(edges$from)) {
        ## If it's a plain number, no need.
        if (edges$from[i] == 0) {

            cat(">>>>>", edges$fromLabel[i],"\n");
            ## Find this in the to list and fix the from.
            edges$from[i] <- findOriginId(edges$fromLabel[i], edges, nodes);

        }
    }

    return(edges[(edges$to!=0),c("id","from","to")]);
}


#########################################################################
##
## Clean up
if (deleteTestVariables) {
    rm(tl);
    rm(ce, ce2, ce3);
    rm(cl, cl2);
    rm(gv, gv2);
    rm(gio);
    rm(gls, glsSub, glsSubA);
    rm(g.xor, g.and, g.or, g.comp, g.comp2);
    rm(oc, ocf);
    rm(gp, gpf, gpt);
    rm(tt);
}

