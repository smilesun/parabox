#' @title Handle Class for ParamNode
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent the tree structure of ParamSet.
#'
#' @return [\code{\link{ParamHandle}}].
#' @family ParamHelpers
ParamHandle = R6Class("ParamHandle",
  inherit = ParamBase,
  public = list(

    # member variables
    node = NULL, # reference to the ParamNode this handle belongs to
    root = NULL, # reference to the root ParamNode
    parent = NULL, # reference to the parent ParamNode
    condition = NULL, # quoted expression that has to evaluate for TRUE on the parent node to make this Node active.
    children = NULL, #  list of references to the children ParamNodes

    # constructor
    initialize = function(node = NULL, root = NULL, parent = NULL, condition = NULL, children = list()) {
      self$node = node
      self$root = root
      self$parent = parent
      self$condition = condition
      self$children = children
    },

    addChild = function(param, condition) {
      self$children = c(self$children, list(list(param = param, condition = condition)))
    },
    setParent = function(pnode) {
      self$parent = pnode
      self$reldepth = self$parent$reldepth + 1
    },
    sampleCurrentNode = function() {
      if(is.null(self$node)) return(NULL)
      if(self$isdependMet()) {
        catf("sampling %s", self$node$id)
        self$val = self$node$ns$msample()
        self$node$val = self$val
      }
      # self$node$sample will cause infinite recursion
    },

    sampleMandChildChain = function() {
      if(length(self$mand.children) == 0) return(NULL)
      for(name in names(self$mand.children)) {
        handle = self$mand.children[[name]]
        handle$sample()
      }
    },
    sampleCondChildChain = function() {
      if(length(self$cond.children) == 0) return(NULL)
      for(name in names(self$cond.children)) {
        handle = self$cond.children[[name]]
        #if(handle$require.varpar(self))
        handle$sample()
      }
    },
    sample = function() {
      self$sampleCurrentNode()
      self$sampleMandChildChain()
      self$sampleCondChildChain()
    },
    printCurrentNode = function() {
      indent = paste(rep("++",self$reldepth), collapse = "")
      catf("%s-%s:%s", indent, self$id, self$val)
    }
  )
)

