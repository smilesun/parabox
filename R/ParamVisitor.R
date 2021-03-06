#' @title Visitor to traverse ParamHandle
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to visit attached ParamHandle object(host). Using a separate visitor to a class is a frequently used design pattern which could seperate the operations(traversal the graph structure of the ParamHandle) on the ParamHandle and the ParamHandle pointer themselves. The ParamHandle node has two kinds of children node: the mandatory node and conditional node. Mandatory nodes are unconditional children for a graph-like hyper parameter, for example, the normal SVM has a hyperparameter C which is mandatory node but the hyperparameter gamma only makes sense when the hyperparameter kernel type is rbf kernel.
#'
#' @return [\code{\link{ParamVisitor}}].
#' @family ParamHelpers
ParamVisitor = R6Class("ParamVisitor",
  inherit = ParamBase, # FIXME: Are we sure?
  public = list(

    # member variables
    host = NULL,
    # constructor
    initialize = function(host) {
      self$host = host
    },

    # public methods

    # Depth First Traversal of current node's mandatory children and add node [arg] to it
    traverseMand = function(arg) {
      if (length(self$host$mand.children) == 0L) return(FALSE)  # if the current node has no mandatory children, recursion stop
      for (name in names(self$host$mand.children)) {
        handle = self$host$mand.children[[name]]
        if (handle$visitor$insertNode(arg)) {
          # recursion: insertNode(arg), traverseMand(arg), insertNode(arg), ...
          return(TRUE)  # insertNode returns true if the input is a direct child of the current node or recursion of insertNode is true
        }
      }
      return(FALSE)  # no place to insert the input among the current node's direct mandatory children.
    },

    # Traverse current node's conditional child and add node arg to it
    traverseCond = function(arg) {
      if (length(self$host$cond.children) == 0L) return(FALSE)  # if the current node has no conditional children, recursion stop
      for (name in names(self$host$cond.children)) {
        handle = self$host$cond.children[[name]]
        if (handle$visitor$insertNode(arg)) {
          # recursion: insertNode(arg), traverseCond(arg), insertNode(arg), ...
          return(TRUE)  # insertNode returns true if the input is a direct child of the current node or recursion of insertNode is true
        }
      }
      return(FALSE)  # no place to insert the input among the current node's direct conditional children.
    },

   parseFlat = function(node.list) {
      node.list = lapply(node.list, function(x) {
        if ("ParamSimple" %in% class(x)) return(makeCondTreeNode(x))
        return(x)
      })
      len = length(node.list)
      safecounter = 0L  # count how many nodes have been inserted
      mnames = lapply(node.list, function(x) x$node$id)
      names(node.list) = unlist(mnames)
      while (length(node.list) != 0) {
        for (name in mnames) {
          if (self$insertNode(node.list[[name]])) node.list[[name]] = NULL  # if inserted successfully, delete the node in wait list
        }
        safecounter = safecounter + 1L
        if (safecounter > len) stop("parseFlat: parsing did not finish after [length of input] steps")
      }
    },

    ## traverse the tree to find out if the the input could be inserted as leave
    insertNode = function(node.depend) {
      if (is.null(node.depend$depend)) {
        # the input is a top layer hyper-parameter
        self$host$addMandChild(ParamHandle$new(node = node.depend$node))
        return(TRUE)
      }
      # the input is **not** a top layer hyper-parameter
      if (is.null(node.depend$depend$id) && is.null(node.depend$fun)) stop("parseFlat: input need at least depend$id or func")
      # the input is direct child of the host
      if (self$host$id == node.depend$depend$id) {
        self$host$addCondChild(ParamHandle$new(node = node.depend$node, depend = node.depend$depend))
        return(TRUE)
      }
      # the input is **not** a direct child of the host
      if (self$traverseMand(node.depend)) return(TRUE)  # child will be added inside the recursion
      if (self$traverseCond(node.depend)) return(TRUE)  # child will be added inside the recursion
      return(FALSE)  # failed to insert the input
    },

    # transform the tree structure to a flat list of **all** nodes and return the list
    toFlat0 = function(res = list()) {
      res[[self$host$node$id]] = self$host$node
      if (length(self$host$mand.children) > 0) {
      for (name in names(self$host$mand.children)) {
        handle = self$host$mand.children[[name]]
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat0(res)
      }
      } # if
      if (length(self$host$cond.children) > 0) {
      for (name in names(self$host$cond.children)) {
        handle = self$host$cond.children[[name]]
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat0(res)
      }
      } # if
      return(res)
    },

    toFlat = function(res = list()) {
      res[[self$host$node$id]] = self$host$val
      if (length(self$host$mand.children) > 0) {
      for (name in names(self$host$mand.children)) {
        handle = self$host$mand.children[[name]]
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat(res)
      }
      } # if
      if (length(self$host$cond.children) > 0) {
      for (name in names(self$host$cond.children)) {
        handle = self$host$cond.children[[name]]
        if (handle$isDependMet()) {
          # difference between toFlat0 and to Flat
        res[[handle$node$id]] = handle$node
        res = handle$visitor$toFlat(res)
        }
      }
      } # if
      return(res)
    },

    # apply func to all node in the tree, without dependency
    treeApply0 = function(func) {
      if (self$host$nochild) return(func(self$host$node))  # apply func to the leave node
      if (length(self$host$mand.children) > 0) {
        for (name in names(self$host$mand.children)) {
          handle = self$host$mand.children[[name]]
          return(handle$visitor$treeApply0(func))
        }
      } # if
      if (length(self$host$cond.children) > 0) {
        for (name in names(self$host$cond.children)) {
          handle = self$host$cond.children[[name]]
          return(handle$visitor$treeApply0(func))
        }
      } # if
    },

    # apply func to all node in the tree, without dependency
    treeApply = function(func) {
      if (is.null(self$host$node)) {
        res = NULL
      }
      else {
        res = func(self$host$node)
      }
      if (self$host$nochild) return(res)  # apply func to the leave node
      if (length(self$host$mand.children) > 0) {
        res2 = lapply(names(self$host$mand.children), function(name) {
          handle = self$host$mand.children[[name]]
          return(handle$visitor$treeApply(func))
      })
        res = c(list(res), res2)
    } # if
      if (length(self$host$cond.children) > 0) {
        res3 = lapply(names(self$host$cond.children), function(name) {
          handle = self$host$cond.children[[name]]
          return(handle$visitor$treeApply(func))
      })
        res = c(list(res), res3)
      } # if
      return(res)
    },

    findDependNode = function(val, name) {
      flag_bit = FALSE
      list_flag = self$treeApply(function(x) {
        if (is.null(x)) return(NULL)
        if (!is.null(x$id) && (x$id == name)) {
          x$handle$val = val
          x$handle$parent$id
          x$handle$id
          flag_ = x$handle$isDependMet()
          flag_bit = flag_
          if (flag_) return(TRUE)
        }
        return(FALSE)
      })
      flag = unlist(list_flag)
      return(any(flag))
    },
      #       findDependNode0 = function(fq, node) {
      #         fqns = names(fq)
      #         for (name in fqns) {
      #           flag = eval(node$depend$fun, envir = setNames(fq[[name]]$val, name))
      #           if (flag) return(TRUE)
      #         }
      #         return(FALSE)
      #       }



    # check if the flat form of paramset violates the dependency
    checkValidFromFlat = function(input = list()) {
      self$treeApply(function(x) x$handle$val = NULL)
      fq = list()  # finished queue
      wq = input   # waiting queue
      hit = TRUE
      while (hit) {
        hit = FALSE
        for (name in names(wq)) {
          if (self$findDependNode(wq[[name]], name)) {
            fq[[name]] =  wq[[name]]
            wq[[name]] = NULL
            hit = TRUE
          } else {
            hit = FALSE
          }
        }
      }
      if (length(wq) > 0) return(FALSE)
      return(TRUE)
    }
  ) # public
) # Class

