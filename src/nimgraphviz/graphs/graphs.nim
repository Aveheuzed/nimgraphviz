import os
import osproc
import streams
import strformat
import strutils

import "../edges/edges.nim"

import tables
export tables


type
  Graph*[E: Edge or Arrow] = ref object
    ## Represents a GraphViz graph or digraph.
    ## Graph[Edge] makes a `strict graph`
    ## Graph[Arrow] makes a `strict digraph`
    ## Notation: for convenience, `Graph` refers to the (generic) nim object
    ## whereas `graph` refers to the GraphViz structure, represented by `Graph[Edge]`

    name*: string
    ## name of the graph, most often irrelevant but in subgraphs
    ## subgraphs whose name begins with "cluster" may be given special treatment
    ## by some of graphviz' layout engines.

    graphAttr*: Table[string, string]
    ## global graph attributes

    nodeAttrs*: Table[string, Table[string, string]]
    ## node attributes; only nodes that are not referenced by edges of the graph
    ## need to referenced here in order to appear in the graph

    edges*: Table[E, Table[string, string]]
    ## edges with their attributes

    subGraphs: seq[Graph[E]]
    ## (private) list of subgraphs.
    ## A digraph may only have digraphs as children;
    ## a graph may only have graphs as children.
    ## This attribute is kept private to warrant against graphs that include each other.
    ## You may access the other attributes however you like;
    ## sanity checks are run during the dot script generation phase.


func newGraph*[E](): Graph[E] =
  ## doesn't do anything, the default initialisation procedure is enough.
  ## actual body:
  ## `result = Graph[E]()`
  result = Graph[E]()

func newGraph*[E](parent: Graph[E]): Graph[E] =
  ## Returns a new graph, attached to its parent as subgraph.
  ## Some graphviz engines have a specific behaviour when the name of the
  ## subgraph begins with "cluster" -- see the official website.
  ## Note that the subgraphs are full graphs themselves: you can treat them
  ## as standalone objects (e.g. when exporting images)
  # result = newGraph[E]()
  result = Graph[E]()
  parent.subGraphs.add(result)


func addEdge*[E](self: Graph[E], edge: E, attr: varargs[(string, string)]) =
  ## Add an edge to the graph. Optional attributes may be specified as a serie
  ## of (key, value) tuples.
  if not self.edges.hasKey(edge) :
    self.edges[edge] = initTable[string, string]()

  for (k,v) in attr:
    self.edges[edge][k] = v

func addNode*(self: Graph, node: string, attr: varargs[(string, string)]) =
  ## Add a node to the graph. Optional attributes may be specified as a serie
  ## of (key, value) tuples.
  ## Note that you don't need to add a node manually if it appears in an edge.
  if not self.nodeAttrs.hasKey(node) :
    self.nodeAttrs[node] = initTable[string, string]()

  for (k,v) in attr:
    self.nodeAttrs[node][k] = v

# get/set graph attr -----------------------------------------------------------
func `[]`*(self: Graph, gAttr: string): string =
  ## Shortcut to access graph attributes
  self.graphAttr[gAttr]

func `[]=`*(self: Graph, gAttr: string, value: string) =
  ## Shortcut to set graph attributes
  self.graphAttr[gAttr] = value

# get/set node attr ------------------------------------------------------------
func `[]`*(self: Graph, node: string, key: string): string =
  ## Shortcut to access node attributes
  ## Returns the attribute value for the given node, given key.
  ## Throws the relevant exception from Table when the node does not exist.
  self.nodeAttrs[node][key]

func `[]=`*(self: Graph, node: string, key: string, value: string) =
  ## Shortcut to edit node attributes.
  ## If the node hasn't got a table yet, it gets one beforehand.
  self.addNode(node)
  self.nodeAttrs[node][key] = value

# get/set edge attr ------------------------------------------------------------
func `[]`*[E](self: Graph[E], edge: E, key: string): string =
  ## Shortcut to access edge attributes
  ## Returns the attribute value for the given edge, given key.
  ## Throws the relevant exception from Table when the edge does not exist.
  self.edges[edge][key]

func `[]=`*[E](self: Graph[E], edge: E, key: string, value: string) =
  ## Shortcut to edit edge attributes.
  ## If the edge doesn't exist in the graph yet, it is created beforehand.
  self.addEdge(edge)
  self.edges[edge][key] = value



iterator iterEdges*[E](self: Graph[E], node: string): E =
  ## Iterate over all the edges adjacent to a given node
  for edge in self.edges.keys() :
    if edge.a == node or edge.b == node:
      yield edge

iterator iterEdgesIn*(self: Graph[Arrow], node: string): Arrow =
  ## Oriented version: yields only inbound edges
  for edge in self.edges.keys() :
    if edge.b == node:
      yield edge
iterator iterEdgesOut*(self: Graph[Arrow], node: string): Arrow =
  ## Oriented version: yields only outbound edges
  for edge in self.edges.keys() :
    if edge.a == node:
      yield edge


func exportIdentifier(identifier:string): string =
  if identifier.validIdentifier() :
    return identifier

  # if needs be, escape '"' and surround in quotes (do not replace '\' !!)
  return "\"" & identifier.replace("\"", "\\\"") & "\""

func `$`(edge: Edge): string =
  exportIdentifier(edge.a) & " -- " & exportIdentifier(edge.b)
func `$`(edge: Arrow): string =
  exportIdentifier(edge.a) & " -> " & exportIdentifier(edge.b)

func exportSubDot(self: Graph): string # forward declaration

func tableToAttributes(tbl: Table[string, string]): seq[string] =
  for (key, value) in tbl.pairs() :
    if key == "label" :
      let is_likely_html = value.startsWith('<') and value.endsWith('>')
      result.add exportIdentifier(key) & "=" & (if is_likely_html: value else: exportIdentifier(value))
    else:
      result.add exportIdentifier(key) & "=" & exportIdentifier(value)

func exportAttributes(self: Graph): string =
  result = tableToAttributes(self.graphAttr).join(";\n")
  if len(result) > 0 :
    result &= "\n"

func exportNodes(self: Graph): string =
  for (node, tbl) in self.nodeAttrs.pairs() :
    result &= exportIdentifier(node)
    if tbl.len > 0:
      result &= " ["
      result &= tableToAttributes(tbl).join(", ")
      result &= "]"
    result &= ";\n"

func exportEdges(self: Graph): string =
  for (edge, tbl) in self.edges.pairs() :
    result &= $edge
    if tbl.len > 0:
      result &= " ["
      result &= tableToAttributes(tbl).join(", ")
      result &= "]"
    result &= ";\n"

func buildBody(self: Graph): string =
  result = "{\n"
  for sub in self.subGraphs :
    result &= exportSubDot(sub)
  result &= self.exportAttributes()
  result &= self.exportNodes()
  result &= self.exportEdges()
  result &= "}\n"

func exportSubDot(self: Graph): string =
  result = "subgraph " & exportIdentifier(self.name) & " " & self.buildBody()

func exportDot*(self: Graph[Edge]): string =
  ## Returns the dot script corresponding to the graph, including subgraphs.
  result = "strict graph " & exportIdentifier(self.name) & " " & self.buildBody()

func exportDot*(self: Graph[Arrow]): string =
  ## Returns the dot script corresponding to the graph, including subgraphs.
  result = "strict digraph " & exportIdentifier(self.name) & " " & self.buildBody()

proc exportImage*(self: Graph, fileName: string, layout="dot", format="", exec="dot") =
  ## Exports the graph as an image file.
  ##
  ## ``filename`` - the name of the file to export to. Should include ".png"
  ## or the appropriate file extension.
  ##
  ## ``layout`` - which of the GraphViz layout engines to use. Default is
  ## ``dot``. Can be one of: ``dot``, ``neato``, ``fdp``, ``sfdp``, ``twopi``,
  ## ``circo`` (or others if you have them installed).
  ##
  ## ``format`` - the output format to export to. The default is ``svg``.
  ## If not specified, it is deduced from the file name.
  ## You can specify more details with
  ## ``"{format}:{rendering engine}:{library}"``.
  ## (See `GV command-line docs <http://www.graphviz.org/doc/info/command.html>`_
  ## for more details)
  ##
  ## ``exec`` - path to the ``dot`` command; use this when ``dot`` is not in
  ## your PATH

  # This blocks determines the output file name and its content type
  # fileName has precedence over self.name
  # The content type is deduced from the file name unless explicitely specified.

  var (dir, name, ext) = splitFile(fileName)
  if len(dir) == 0 :
    dir = "." # current dir

  if ext == "." or ext == "":
    ext = ".svg" # default format : SVG

  let actual_format =
    if format != "" :
       format
    else :
      ext[1..^1] # remove the '.' in first position
  let file = &"{dir}/{name}{ext}"

  let text = self.exportDot()
  let args = [
    &"-K{layout}",
    &"-o{file}",
    &"-T{actual_format}",
    "-q"
  ]
  let process =
    try :
      startProcess(exec, args=args, options={poUsePath})
    except OSError :
      # "command not found", but I think the default message is explicit enough
      # the try/except block is just there to show where the error can arise
      raise
  let stdin = process.inputStream
  let stderr = process.errorStream
  stdin.write(text)
  stdin.close()
  let errcode = process.waitForExit()
  let errormsg = stderr.readAll()
  process.close()
  if errcode != 0:
    raise newException(OSError, fmt"[errcode {errcode}] " & errormsg)
