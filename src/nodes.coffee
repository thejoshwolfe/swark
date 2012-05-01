# this file contains all of the node classes for the syntax tree. Most
# nodes are created as the result of actions in the [grammar](grammar.html),
# but some are created by other nodes as a method of code generation.

stdlib = require './stdlib'
{
  Program,
  SetInstruction,
  CallInstruction,
} = require './intermediate'

# Return a flattened version of an array.
# Handy for getting a list of `children` from the nodes.
flatten = (array) ->
  flattened = []
  for element in array
    if element instanceof Array
      flattened = flattened.concat flatten element
    else
      flattened.push element
  flattened


# Constant functions for nodes that don't need customization.
YES     = -> yes
NO      = -> no
THIS    = -> this
NEGATE  = -> @negated = not @negated; this

toHex = (n) ->
  n = (n + 0x10000) % 0x10000
  "0x#{n.toString 16}"

class Type
  constructor: (@basic, @args, @returnType) ->
    @synonym = this
    @codeNodes = []
  findRoot: ->
    return this if this is @synonym
    return @synonym = @synonym.findRoot()
  linkTo: (otherType) ->
    throw "assertion failed. everything's broken" unless @synonym is this and otherType.synonym is otherType
    @synonym = otherType
    # combine our lists of code nodes
    otherType.codeNodes?.push @codeNodes...
voidType = new Type "void"
intType = new Type "int"
stringType = new Type "string"
forbidVoid = (type) ->
  if type is voidType
    throw TypeError "that thing can't be void"
linkTypes = (leftType, rightType) ->
  # TODO: detect circularity
  [leftType, rightType] = [leftType.findRoot(), rightType.findRoot()]
  return if leftType is rightType
  # if one of them is totally unknown, it's safe to link them.
  unless leftType.basic?
    return leftType.linkTo rightType
  unless rightType.basic?
    return rightType.linkTo leftType
  unless leftType.basic is rightType.basic
    throw TypeError "incompatible types #{leftType.basic} and #{rightType.basic}"
  unless leftType.basic is "func"
    return leftType.linkTo rightType
  unless leftType.args.length is rightType.args.length
    throw TypeError "number of arguments must match"
  for leftArg, i in leftType.args
    rightArg = rightType.args[i]
    linkTypes leftArg, rightArg
  linkTypes leftType.returnType, rightType.returnType
  # seems legit
  leftType.linkTo rightType

class Variable
  constructor: (@type, @namespace) ->
    @id = @namespace.nextVariableId()
  toString: => @id
  writableAsm: => @namespace.getStorageSpace @id
  readableAsm: => @writableAsm()

class Namespace
  constructor: (@parent) ->
    @names = {}
    unless @parent?
      @root = this
      labelCount = 0
      @nextLabel = -> "_#{labelCount++}"
      idCount = 0
      @nextVariableId = -> "{#{idCount++}}"
    else
      @root = @parent.root
      @nextLabel = @root.nextLabel
      @nextVariableId = @root.nextVariableId
    @parameterCount = 0 # doesn't include secret closure context
    @localVariableCount = 0 # doesn't include parameters or the return address
    @storageSpaces = {}
  find: (name) ->
    @names[name] or @parent?.find name
  createVariable: (name, type, isParameter) ->
    if isParameter
      @parameterCount++
    variable = new Variable type, this
    @names[name] = variable
    variable
  createBuiltinFunc: (o, name, type, asm) ->
    label = @nextLabel()
    o.program.createDataSection label, asm
    variable = @createVariable name, type
    o.instructions.push new SetInstruction variable, {toString: (-> label), readableAsm: -> label}
  createLiteralString: (o, value) ->
    label = @nextLabel()
    o.program.createDataSection label, "dat #{value.length}, #{JSON.stringify value}"
    label
  createSubNamespace: ->
    new Namespace this
  getStorageSpace: (id) ->
    @storageSpaces[id] ?= "[z+#{toHex(-++@localVariableCount)}]"

#### Base

# The **Base** is the abstract base class for all nodes in the syntax tree.
# Each subclass implements the `compileNode` method, which performs the
# code generation for that node. To compile a node to JavaScript,
# call `compile` on it, which wraps `compileNode` in some generic extra smarts,
# to know when the generated code needs to be wrapped up in a closure.
# An options hash is passed and cloned throughout, containing information about
# the environment from higher in the tree (such as if a returned value is
# being requested by the surrounding function), information about the current
# scope, and indentation level.
exports.Base = class Base
  # Construct a node that returns the current node's result.
  # Note that this is overridden for smarter behavior for
  # many statement nodes (e.g. If, For)...
  makeReturn: (res) ->
    me = @unwrapAll()
    if res
      new Call new Literal("#{res}.push"), [me]
    else
      new Return me

  # Does this node, or any of its children, contain a node of a certain kind?
  # Recursively traverses down the *children* of the nodes, yielding to a block
  # and returning true when the block finds a match. `contains` does not cross
  # scope boundaries.
  contains: (pred) ->
    contains = no
    @traverseChildren no, (node) ->
      if pred node
        contains = yes
        return no
    contains

  # Is this node of a certain type, or does it contain the type?
  containsType: (type) ->
    this instanceof type or @contains (node) -> node instanceof type

  # Pull out the last non-comment node of a node list.
  lastNonComment: (list) ->
    i = list.length
    return list[i] while i-- when list[i] not instanceof Comment
    null

  # `toString` representation of the node, for inspecting the parse tree.
  # This is what `swark --nodes` prints out.
  toString: (idt = '', name = @constructor.name) ->
    tree = '\n' + idt + name
    @eachChild (node) -> tree += node.toString idt + TAB
    tree

  # Passes each child to a function, breaking when the function returns `false`.
  eachChild: (func) ->
    return this unless @children
    for attr in @children when @[attr]
      for child in flatten [@[attr]]
        return this if func(child) is false
    this

  traverseChildren: (crossScope, func) ->
    @eachChild (child) ->
      return false if func(child) is false
      child.traverseChildren crossScope, func

  invert: ->
    new Op '!', this

  unwrapAll: ->
    node = this
    continue until node is node = node.unwrap()
    node

  # Default implementations of the common node properties and methods. Nodes
  # will override these with custom logic, if needed.
  children: []

  isStatement     : NO
  jumps           : NO
  isComplex       : YES
  isChainable     : NO
  isAssignable    : NO

  unwrap     : THIS

  # Is this node used to assign a certain variable?
  assigns: NO

#### Block

# The block is the list of expressions that forms the body of an
# indented block of code -- the implementation of a function, a clause in an
# `if`, `switch`, or `try`, and so on...
exports.Block = class Block extends Base
  constructor: (nodes) ->
    @expressions = (item for item in (flatten nodes or []) when item)

  children: ['expressions']

  # Tack an expression on to the end of this expression list.
  push: (node) ->
    @expressions.push node
    this

  # Remove and return the last expression of this expression list.
  pop: ->
    @expressions.pop()

  # Add an expression at the beginning of this expression list.
  unshift: (node) ->
    @expressions.unshift node
    this

  # If this Block consists of just a single node, unwrap it by pulling
  # it back out.
  unwrap: ->
    if @expressions.length is 1 then @expressions[0] else this

  # Is this an empty block of code?
  isEmpty: ->
    not @expressions.length

  isStatement: (o) ->
    for exp in @expressions when exp.isStatement o
      return yes
    no

  jumps: (o) ->
    for exp in @expressions
      return exp if exp.jumps o

  # A Block node does not return its entire body, rather it
  # ensures that the final expression is returned.
  makeReturn: (res) ->
    len = @expressions.length
    while len--
      expr = @expressions[len]
      if expr not instanceof Comment
        @expressions[len] = expr.makeReturn res
        @expressions.splice(len, 1) if expr instanceof Return and not expr.expression
        break
    this

  # A **Block** is the only node that can serve as the root.
  compileToIntermediate: ->
    o = {namespace: new Namespace, program: new Program}
    o.instructions = o.program.createFunc(o.namespace).instructions
    o.namespace.createBuiltinFunc o, "printc", new Type("func", [intType], voidType), stdlib.printc
    o.namespace.createBuiltinFunc o, "prints", new Type("func", [stringType], voidType), stdlib.prints
    @compileStatement o
    o.program

  compileStatement: (o) ->
    codes = []
    for expression in @expressions
      codes.push expression.compileStatement o
    codes.join "\n"

  # Wrap up the given nodes as a **Block**, unless it already happens
  # to be one.
  @wrap: (nodes) ->
    return nodes[0] if nodes.length is 1 and nodes[0] instanceof Block
    new Block nodes

#### Literal

# Literals are static values that can be passed through directly into
# JavaScript without translation, such as: strings, numbers,
# `true`, `false`, `null`...
exports.Literal = class Literal extends Base
  constructor: (value, @type) ->
    # translate bools to ints
    @value = switch value
      when "true" then "1"
      when "false" then "0"
      else value

  compileExpression: (o) ->
    unless @type?
      throw "closures are broken for now" unless (variable = o.namespace.names[@value])?
      throw SyntaxError "undefined variable \"#{@value}\"" unless variable?
      return variable
    # readonly literal values
    thingy = switch @type
      when "int" then {type: intType, value: @value}
      when "string" then {type: stringType, value: o.namespace.createLiteralString o, JSON.parse @value}
    return {} =
      type: thingy.type
      toString: -> thingy.value
      readableAsm: -> thingy.value

  makeReturn: ->
    if @isStatement() then this else super

  isAssignable: ->
    IDENTIFIER.test @value

  isStatement: ->
    @value in ['break', 'continue', 'debugger']

  isComplex: NO

  assigns: (name) ->
    name is @value

  jumps: (o) ->
    return this if @value is 'break' and not (o?.loop or o?.block)
    return this if @value is 'continue' and not o?.loop

  toString: ->
    ' "' + @value + '"'

#### Return

# A `return` is a *pureStatement* -- wrapping it in a closure wouldn't
# make sense.
exports.Return = class Return extends Base
  constructor: (expr) ->
    @expression = expr if expr and not expr.unwrap().isUndefined

  children: ['expression']

  isStatement:     YES
  makeReturn:      THIS
  jumps:           THIS

#### Value

# A value, variable or literal or parenthesized, indexed or dotted into,
# or vanilla.
exports.Value = class Value extends Base
  constructor: (base, props, tag) ->
    return base if not props and base instanceof Value
    @base       = base
    @properties = props or []
    @[tag]      = true if tag
    return this

  children: ['base', 'properties']

  # Add a property (or *properties* ) `Access` to the list.
  add: (props) ->
    @properties = @properties.concat props
    this

  hasProperties: ->
    !!@properties.length

  # Some boolean checks for the benefit of other nodes.
  isArray        : -> not @properties.length and @base instanceof Arr
  isComplex      : -> @hasProperties() or @base.isComplex()
  isAssignable   : -> @hasProperties() or @base.isAssignable()
  isSimpleNumber : -> @base instanceof Literal and SIMPLENUM.test @base.value
  isString       : -> @base instanceof Literal and IS_STRING.test @base.value
  isAtomic       : ->
    for node in @properties.concat @base
      return no if node instanceof Call
    yes

  isStatement : (o)    -> not @properties.length and @base.isStatement o
  assigns     : (name) -> not @properties.length and @base.assigns name
  jumps       : (o)    -> not @properties.length and @base.jumps o

  isObject: (onlyGenerated) ->
    return no if @properties.length
    (@base instanceof Obj) and (not onlyGenerated or @base.generated)

  # The value can be unwrapped as its inner node, if there are no attached
  # properties.
  unwrap: ->
    if @properties.length then this else @base

#### Comment

# pass through block comments as JavaScript block comments
# at the same position.
exports.Comment = class Comment extends Base
  constructor: (@comment) ->

  isStatement:     YES
  makeReturn:      THIS

#### Call

# Node for a function invocation. Takes care of converting `super()` calls into
# calls against the prototype's function of the same name.
exports.Call = class Call extends Base
  constructor: (@variable, @argNodes = []) ->

  children: ['variable', 'argNodes']

  compileStatement: (o) ->
    funcValue = @variable.unwrapAll().compileExpression o
    funcType = funcValue.type.findRoot()
    unless funcType.basic is "func"
      throw SyntaxError "can't call something of type \"#{funcType.basic}\""
    unless funcType.args.length is @argNodes.length
      throw SyntaxError "function takes #{funcType.args.length} arguments, not #{@argNodes.length}"
    args = []
    argTypes = []
    for argNode in @argNodes
      arg = argNode.unwrapAll().compileExpression o
      forbidVoid arg.type
      argTypes.push arg.type
      args.push arg
    callType = new Type "func", argTypes, new Type
    linkTypes funcType, callType
    # good news, guys: your parameter types are now known!
    for codeNode in callType.findRoot().codeNodes
      codeNode.compileFunc o
    o.instructions.push new CallInstruction funcValue, args, o.namespace

  # Walk through the objects in the arguments, moving over simple values.
  # This allows syntax like `call a: b, c` into `call({a: b}, c);`
  filterImplicitObjects: (list) ->
    nodes = []
    for node in list
      unless node.isObject?() and node.base.generated
        nodes.push node
        continue
      obj = null
      for prop in node.base.properties
        if prop instanceof Assign or prop instanceof Comment
          nodes.push obj = new Obj properties = [], true if not obj
          properties.push prop
        else
          nodes.push prop
          obj = null
    nodes

#### Extends

# Node to extend an object's prototype with an ancestor object.
# After `goog.inherits` from the
# [Closure Library](http://closure-library.googlecode.com/svn/docs/closureGoogBase.js.html).
exports.Extends = class Extends extends Base
  constructor: (@child, @parent) ->

  children: ['child', 'parent']

#### Access

# A `.` access into a property of a value, or the `::` shorthand for
# an access into the object's prototype.
exports.Access = class Access extends Base
  constructor: (@name, tag) ->
    @name.asKey = yes

  children: ['name']

  isComplex: NO

#### Index

# A `[ ... ]` indexed access into an array or object.
exports.Index = class Index extends Base
  constructor: (@index) ->

  children: ['index']

  isComplex: ->
    @index.isComplex()

#### Range

# A range literal. Ranges can be used to extract portions (slices) of arrays,
# to specify a range for comprehensions, or as a value, to be expanded into the
# corresponding array of integers at runtime.
exports.Range = class Range extends Base

  children: ['from', 'to']

  constructor: (@from, @to, tag) ->
    @exclusive = tag is 'exclusive'
    @equals = if @exclusive then '' else '='

    # Generate the condition.
    condPart = if @stepNum
      if +@stepNum > 0 then "#{lt} #{@toVar}" else "#{gt} #{@toVar}"
    else if known
      [from, to] = [+@fromNum, +@toNum]
      if from <= to then "#{lt} #{to}" else "#{gt} #{to}"
    else
      cond     = "#{@fromVar} <= #{@toVar}"
      "#{cond} ? #{lt} #{@toVar} : #{gt} #{@toVar}"

    # Generate the step.
    stepPart = if @stepVar
      "#{idx} += #{@stepVar}"
    else if known
      if namedIndex
        if from <= to then "++#{idx}" else "--#{idx}"
      else
        if from <= to then "#{idx}++" else "#{idx}--"
    else
      if namedIndex
        "#{cond} ? ++#{idx} : --#{idx}"
      else
        "#{cond} ? #{idx}++ : #{idx}--"

    varPart  = "#{idxName} = #{varPart}" if namedIndex
    stepPart = "#{idxName} = #{stepPart}" if namedIndex

    # The final loop body.
    "#{varPart}; #{condPart}; #{stepPart}"


#### Slice

# An array slice literal. Unlike JavaScript's `Array#slice`, the second parameter
# specifies the index of the end of the slice, just as the first parameter
# is the index of the beginning.
exports.Slice = class Slice extends Base

  children: ['range']

  constructor: (@range) ->
    super()

#### Obj

# An object literal, nothing fancy.
exports.Obj = class Obj extends Base
  constructor: (props, @generated = false) ->
    @objects = @properties = props or []

  children: ['properties']

  assigns: (name) ->
    for prop in @properties when prop.assigns name then return yes
    no

#### Arr

# An array literal.
exports.Arr = class Arr extends Base
  constructor: (objs) ->
    @objects = objs or []

  children: ['objects']

  filterImplicitObjects: Call::filterImplicitObjects

  assigns: (name) ->
    for obj in @objects when obj.assigns name then return yes
    no

#### Assign

# The **Assign** is used to assign a local variable to value, or to set the
# property of an object -- including within object literals.
exports.Assign = class Assign extends Base
  constructor: (@variable, @value, @context, options) ->
    @param = options and options.param
    @subpattern = options and options.subpattern

  children: ['variable', 'value']

  compileStatement: (o) ->
    name = @variable.unwrapAll().value
    value = @value.unwrapAll().compileExpression o
    variable = o.namespace.find name
    if @context is ":="
      throw SyntaxError "variable \"#{name}\" is already declared" if variable?.namespace is o.namespace
      variableType = new Type
      variable = o.namespace.createVariable name, variableType
      linkTypes variableType, value.type
    else
      unless variable?
        throw SyntaxError "variable \"#{name}\" is not declared"
      linkTypes value.type, variable.type
    o.instructions.push new SetInstruction variable, value

  isStatement: (o) ->
    o?.level is LEVEL_TOP and @context? and "?" in @context

  assigns: (name) ->
    @[if @context is 'object' then 'value' else 'variable'].assigns name

#### Code

# A function definition. This is the only node that creates a new Scope.
# When for the purposes of walking the contents of a function body, the Code
# has no *children* -- they're within the inner scope.
exports.Code = class Code extends Base
  constructor: (params, body, tag) ->
    @params  = params or []
    @body    = body or new Block
    @bound   = tag is 'boundfunc'
    @context = '_this' if @bound
    @compiled = false

  compileExpression: (o) ->
    @label = o.namespace.nextLabel()
    @namespace = o.namespace.createSubNamespace()
    @type = new Type "func", (new Type for _ in @params), new Type
    @type.codeNodes.push this
    {type: @type, value: @label}

  compileFunc: (o) ->
    return if @compiled
    @compiled = true
    o = {namespace: @namespace, program: o.program}
    o.instructions = o.program.createFunc(o.namespace, @label).instructions

    # dude, check out these parameters
    for paramNode, i in @params
      o.namespace.createVariable paramNode.name.value, @type.args[i], true
    @body.compileStatement o
    # TODO figure out how to return stuff
    linkTypes @type.findRoot().returnType, voidType

  children: ['params', 'body']

  isStatement: -> !!@ctor

  jumps: NO

  # A list of parameter names, excluding those generated by the compiler.
  paramNames: ->
    names = []
    names.push param.names()... for param in @params
    names

  # Short-circuit `traverseChildren` method to prevent it from crossing scope boundaries
  # unless `crossScope` is `true`.
  traverseChildren: (crossScope, func) ->
    super(crossScope, func) if crossScope

#### Param

# A parameter in a function definition. Beyond a typical Javascript parameter,
# these parameters can also attach themselves to the context of the function,
# as well as be a splat, gathering up a group of parameters into an array.
exports.Param = class Param extends Base
  constructor: (@name, @value, @splat) ->

  children: ['name', 'value']

  asReference: (o) ->
    return @reference if @reference
    node = @name
    if node.this
      node = node.properties[0].name
      if node.value.reserved
        node = new Literal o.scope.freeVariable node.value
    else if node.isComplex()
      node = new Literal o.scope.freeVariable 'arg'
    node = new Value node
    node = new Splat node if @splat
    @reference = node

  isComplex: ->
    @name.isComplex()

  # Finds the name or names of a `Param`; useful for detecting duplicates.
  # In a sense, a destructured parameter represents multiple JS parameters,
  # thus this method returns an `Array` of names.
  # Reserved words used as param names, as well as the Object and Array
  # literals used for destructured params, get a compiler generated name
  # during the `Code` compilation step, so this is necessarily an incomplete
  # list of a parameter's names.
  names: (name = @name)->
    atParam = (obj) ->
      {value} = obj.properties[0].name
      return if value.reserved then [] else [value]
    # * simple literals `foo`
    return [name.value] if name instanceof Literal
    # * at-params `@foo`
    return atParam(name) if name instanceof Value
    names = []
    for obj in name.objects
      # * assignments within destructured parameters `{foo:bar}`
      if obj instanceof Assign
        names.push obj.variable.base.value
      # * destructured parameters within destructured parameters `[{a}]`
      else if obj.isArray() or obj.isObject()
        names.push @names(obj.base)...
      # * at-params within destructured parameters `{@foo}`
      else if obj.this
        names.push atParam(obj)...
      # * simple destructured parameters {foo}
      else names.push obj.base.value
    names

#### Splat

# A splat, either as a parameter to a function, an argument to a call,
# or as part of a destructuring assignment.
exports.Splat = class Splat extends Base

  children: ['name']

  isAssignable: YES

  constructor: (name) ->
    @name = if name.compile then name else new Literal name

  assigns: (name) ->
    @name.assigns name

  unwrap: -> @name

#### While

# A while loop, the only sort of low-level loop exposed. From
# it, all other loops can be manufactured. Useful in cases where you need more
# flexibility or more speed than a comprehension can provide.
exports.While = class While extends Base
  constructor: (condition, options) ->
    @condition = if options?.invert then condition.invert() else condition
    @guard     = options?.guard

  children: ['condition', 'guard', 'body']

  isStatement: YES

  makeReturn: (res) ->
    if res
      super
    else
      @returns = not @jumps loop: yes
      this

  addBody: (@body) ->
    this

  jumps: ->
    {expressions} = @body
    return no unless expressions.length
    for node in expressions
      return node if node.jumps loop: yes
    no

#### Op

# Simple Arithmetic and logical operations. Performs some conversion from
# our operations into their JavaScript equivalents.
exports.Op = class Op extends Base
  constructor: (op, first, second, flip ) ->
    return new In first, second if op is 'in'
    if op is 'do'
      return @generateDo first
    @operator = op
    @first    = first
    @second   = second
    @flip     = !!flip
    return this

  # The map of invertible operators.
  INVERSIONS =
    '!==': '==='
    '===': '!=='

  children: ['first', 'second']

  isSimpleNumber: NO

  isUnary: ->
    not @second

  isComplex: ->
    not (@isUnary() and (@operator in ['+', '-'])) or @first.isComplex()

  # Am I capable of
  # [Python-style comparison chaining](http://docs.python.org/reference/expressions.html#notin)?
  isChainable: ->
    @operator in ['<', '>', '>=', '<=', '===', '!==']

  invert: ->
    if @isChainable() and @first.isChainable()
      allInvertable = yes
      curr = this
      while curr and curr.operator
        allInvertable and= (curr.operator of INVERSIONS)
        curr = curr.first
      return new Parens(this).invert() unless allInvertable
      curr = this
      while curr and curr.operator
        curr.invert = !curr.invert
        curr.operator = INVERSIONS[curr.operator]
        curr = curr.first
      this
    else if op = INVERSIONS[@operator]
      @operator = op
      if @first.unwrap() instanceof Op
        @first.invert()
      this
    else if @second
      new Parens(this).invert()
    else if @operator is '!' and (fst = @first.unwrap()) instanceof Op and
                                  fst.operator in ['!', 'in', 'instanceof']
      fst
    else
      new Op '!', this

  generateDo: (exp) ->
    passedParams = []
    func = if exp instanceof Assign and (ref = exp.value.unwrap()) instanceof Code
      ref
    else
      exp
    for param in func.params or []
      if param.value
        passedParams.push param.value
        delete param.value
      else
        passedParams.push param
    call = new Call exp, passedParams
    call.do = yes
    call

  toString: (idt) ->
    super idt, @constructor.name + ' ' + @operator

#### In
exports.In = class In extends Base
  constructor: (@object, @array) ->

  children: ['object', 'array']

  invert: NEGATE

  toString: (idt) ->
    super idt, @constructor.name + if @negated then '!' else ''

#### Parens

# An extra set of parentheses, specified explicitly in the source. At one time
# we tried to clean up the results by detecting and removing redundant
# parentheses, but no longer -- you can put in as many as you please.
#
# Parentheses are a good way to force any statement to become an expression.
exports.Parens = class Parens extends Base
  constructor: (@body) ->

  children: ['body']

  unwrap    : -> @body
  isComplex : -> @body.isComplex()

#### For

# our replacement for the *for* loop is our array and object
# comprehensions, that compile into *for* loops here. They also act as an
# expression, able to return the result of each filtered iteration.
#
# Unlike Python array comprehensions, they can be multi-line, and you can pass
# the current index of the loop as a second parameter. Unlike Ruby blocks,
# you can map and filter in a single pass.
exports.For = class For extends While
  constructor: (body, source) ->
    {@source, @guard, @step, @name, @index} = source
    @body    = Block.wrap [body]
    @own     = !!source.own
    @object  = !!source.object
    [@name, @index] = [@index, @name] if @object
    throw SyntaxError 'index cannot be a pattern matching expression' if @index instanceof Value
    @range   = @source instanceof Value and @source.base instanceof Range and not @source.properties.length
    @pattern = @name instanceof Value
    throw SyntaxError 'indexes do not apply to range loops' if @range and @index
    throw SyntaxError 'cannot pattern match over range loops' if @range and @pattern
    @returns = false

  children: ['body', 'source', 'guard', 'step']

  pluckDirectCall: (o, body) ->
    defs = ''
    for expr, idx in body.expressions
      expr = expr.unwrapAll()
      continue unless expr instanceof Call
      val = expr.variable.unwrapAll()
      continue unless (val instanceof Code) or
                      (val instanceof Value and
                      val.base?.unwrapAll() instanceof Code and
                      val.properties.length is 1 and
                      val.properties[0].name?.value in ['call', 'apply'])
      fn    = val.base?.unwrapAll() or val
      ref   = new Literal o.scope.freeVariable 'fn'
      base  = new Value ref
      if val.base
        [val.base, base] = [base, val]
      body.expressions[idx] = new Call base, expr.args
      defs += @tab + new Assign(ref, fn).compile(o, LEVEL_TOP) + ';\n'
    defs

#### Switch

# A JavaScript *switch* statement. Converts into a returnable expression on-demand.
exports.Switch = class Switch extends Base
  constructor: (@subject, @cases, @otherwise) ->

  children: ['subject', 'cases', 'otherwise']

  isStatement: YES

  jumps: (o = {block: yes}) ->
    for [conds, block] in @cases
      return block if block.jumps o
    @otherwise?.jumps o

  makeReturn: (res) ->
    pair[1].makeReturn res for pair in @cases
    @otherwise or= new Block [new Literal 'void 0'] if res
    @otherwise?.makeReturn res
    this

#### If

# *If/else* statements. Acts as an expression by pushing down requested returns
# to the last line of each clause.
#
# Single-expression **Ifs** are compiled into conditional operators if possible,
# because ternaries are already proper expressions, and don't need conversion.
exports.If = class If extends Base
  constructor: (condition, @body, options = {}) ->
    @condition = if options.type is 'unless' then condition.invert() else condition
    @elseBody  = null
    @isChain   = false

  children: ['condition', 'body', 'elseBody']

  bodyNode:     -> @body?.unwrap()
  elseBodyNode: -> @elseBody?.unwrap()

  # Rewrite a chain of **Ifs** to add a default case as the final *else*.
  addElse: (elseBody) ->
    if @isChain
      @elseBodyNode().addElse elseBody
    else
      @isChain  = elseBody instanceof If
      @elseBody = @ensureBlock elseBody
    this

  # The **If** only compiles into a statement if either of its bodies needs
  # to be a statement. Otherwise a conditional operator is safe.
  isStatement: (o) ->
    o?.level is LEVEL_TOP or
      @bodyNode().isStatement(o) or @elseBodyNode()?.isStatement(o)

  jumps: (o) -> @body.jumps(o) or @elseBody?.jumps(o)

  makeReturn: (res) ->
    @elseBody  or= new Block [new Literal 'void 0'] if res
    @body     and= new Block [@body.makeReturn res]
    @elseBody and= new Block [@elseBody.makeReturn res]
    this

  ensureBlock: (node) ->
    if node instanceof Block then node else new Block [node]

# Faux-Nodes
# ----------
# Faux-nodes are never created by the grammar, but are used during code
# generation to generate other combinations of nodes.

#### Closure

# A faux-node used to wrap an expressions body in a closure.
Closure =

  # Wrap the expressions body, unless it contains a pure statement,
  # in which case, no dice. If the body mentions `this` or `arguments`,
  # then make sure that the closure wrapper preserves the original values.
  wrap: (expressions, statement, noReturn) ->
    return expressions if expressions.jumps()
    func = new Code [], Block.wrap [expressions]
    args = []
    if (mentionsArgs = expressions.contains @literalArgs) or expressions.contains @literalThis
      meth = new Literal if mentionsArgs then 'apply' else 'call'
      args = [new Literal 'this']
      args.push new Literal 'arguments' if mentionsArgs
      func = new Value func, [new Access meth]
    func.noReturn = noReturn
    call = new Call func, args
    if statement then Block.wrap [call] else call

  literalArgs: (node) ->
    node instanceof Literal and node.value is 'arguments' and not node.asKey

  literalThis: (node) ->
    (node instanceof Literal and node.value is 'this' and not node.asKey) or
      (node instanceof Code and node.bound)

# Constants
# ---------

# Levels indicate a node's position in the AST. Useful for knowing if
# parens are necessary or superfluous.
LEVEL_TOP    = 1  # ...;
LEVEL_PAREN  = 2  # (...)
LEVEL_LIST   = 3  # [...]
LEVEL_COND   = 4  # ... ? x : y
LEVEL_OP     = 5  # !...
LEVEL_ACCESS = 6  # ...[0]

# Tabs are two spaces for pretty printing.
TAB = '  '

IDENTIFIER_STR = "[$A-Za-z_\\x7f-\\uffff][$\\w\\x7f-\\uffff]*"
IDENTIFIER = /// ^ #{IDENTIFIER_STR} $ ///
SIMPLENUM  = /^[+-]?\d+$/

# Is a literal value a string?
IS_STRING = /^['"]/

