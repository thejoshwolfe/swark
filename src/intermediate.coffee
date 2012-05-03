
exports.Program = class Program
  constructor: ->
    @funcs = []
    @dataSections = {}
  createFunc: (args...) ->
    func = new Func args...
    @funcs.push func
    func
  createDataSection: (label, asm) ->
    @dataSections[label] = asm
  getIntermediateString: -> @getSomeString "toString"
  compileToAssembly: -> @getSomeString "toAsm"
  getSomeString: (stringGetterName) ->
    result = []
    for func in @funcs
      result.push func[stringGetterName]()
    result.push "; ========= data ========="
    for label, asm of @dataSections
      result.push ":#{label} #{asm}"
    result.join "\n"

exports.Func = class Func
  constructor: (@namespace, @label) ->
    @instructions = []
  toString: ->
    result = []
    result.push "function #{@label or "main"} {"
    for instruction in @instructions
      result.push "    #{instruction.toString()}"
    result.push "}"
    result.push ""
    result.join "\n"
  toAsm: ->
    isMain = not @namespace.parent?
    result = []
    if isMain
      result.push "; main"
    else
      # we need z because sp can't be offset inline
      result.push ":#{@label}"
      result.push "set z, sp"
    for instruction in @instructions
      result.push instruction.toAsm()
    if isMain
      result.push "; exit"
      result.push "set [0x8ffe], 0"
    else
      result.push "; return"
      result.push "add sp, #{@namespace.parameterCount + 2}"
      result.push "set pc, [z]"
    result.push ""
    result.join "\n"

exports.SetInstruction = class SetInstruction
  constructor: (@dest, @source) ->
  toAsm: -> "set #{@dest.writableAsm()}, #{@source.readableAsm()}"
  toString: -> "set #{@dest.toString()}, #{@source.toString()}"

exports.CallInstruction = class CallInstruction
  constructor: (@func, @args, @namespace, @returnValue) ->
  toString: -> "#{if @returnValue? then @returnValue.toString() + " = " else ""}call #{@func.toString()} (#{(" " + arg.toString() for arg in @args).join ","} )"
  toAsm: ->
    # position the stack just after our local variables
    codes = []
    codes.push "; function call"
    codes.push "sub sp, #{@namespace.localVariableCount}"
    # don't forget the secret close context
    codes.push "set push, z"
    for arg in @args
      codes.push "set push, #{arg.readableAsm()}"
    codes.push "jsr #{@func.readableAsm()}"
    # restore the stack and z
    codes.push "add sp, #{@namespace.localVariableCount}"
    codes.push "set z, sp"
    codes.join "\n"

