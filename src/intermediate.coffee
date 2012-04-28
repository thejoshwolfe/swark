
exports.Program = class Program
  constructor: ->
    @parts = []
  createPart: ->
    part = {instructions: []}
    @parts.push part
    part
  getIntermediateString: ->
    result = []
    for part in @parts
      for instruction in part.instructions
        result.push instruction.toString()
    result.join "\n"
  compileToAssembly: ->
    result = []
    for part in @parts
      for instruction in part.instructions
        result.push instruction.toAsm()
    result.join "\n"

exports.RawInstruction = class RawInstruction
  constructor: (@asm) ->
  toAsm: -> @asm
  toString: -> "raw #{JSON.stringify @asm}"

exports.SetInstruction = class SetInstruction
  constructor: (@dest, @source) ->
  toAsm: -> "set #{@dest}, #{@source}"
  toString: -> @toAsm()

exports.CallInstruction = class CallInstruction
  constructor: (@funcAccess, @argAccesses, @namespace) ->
  toString: -> "call #{@funcAccess} (#{(" " + arg for arg in @argAccesses).join ","} )"
  toAsm: ->
    # position the stack just after our local variables
    codes = []
    codes.push "; function call"
    codes.push "sub sp, #{@namespace.localVariableCount}"
    # don't forget the secret close context
    codes.push "set push, z"
    for arg in @argAccesses
      codes.push "set push, #{arg}"
    codes.push "jsr #{@funcAccess}"
    # restore the stack and z
    codes.push "add sp, #{@namespace.localVariableCount}"
    codes.push "set z, sp"
    codes.join "\n"

