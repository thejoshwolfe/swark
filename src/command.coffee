# The `swark` utility. Handles command-line compilation of swark
# saved, printed as a token stream or as the syntax tree, or launch an
# interactive REPL.

# External dependencies.
fs             = require 'fs'
path           = require 'path'
optparse       = require './optparse'
Swark          = require './swark'
{spawn, exec}  = require 'child_process'

printLine = (line) -> process.stdout.write line + '\n'
printWarn = (line) -> process.stderr.write line + '\n'

hidden = (file) -> /^\.|~$/.test file

# The help banner that is printed when `swark` is called without arguments.
BANNER = '''
  Usage: swark [options] path/to/code.swark -- [args]
'''

# The list of all the valid option flags that `swark` knows how to handle.
SWITCHES = [
  ['-e', '--eval',            'pass a string from the command line as input']
  ['-h', '--help',            'display this help message']
  ['-i', '--interactive',     'run an interactive REPL']
  ['-n', '--nodes',           'print out the parse tree that the parser produces']
  ['-o', '--output [FILE]',   'set the output file for compiled DCPU16 code']
  ['-p', '--print',           'print out the compiled DCPU16 assembly']
  ['-s', '--stdio',           'listen for and compile scripts over stdio']
  ['-a', '--assembly',        'treat input as dcpu16 assembly instead of swark']
  ['-t', '--tokens',          'print out the tokens that the lexer/rewriter produce']
  ['-v', '--version',         'display the version number']
]

# Top-level objects shared by all the functions.
opts         = {}
source       = null
optionParser = null

# Run `swark` by parsing passed options and determining what action to take.
# Many flags cause us to divert before compiling anything. Flags passed after
# `--` will be passed verbatim to your script as arguments in `process.argv`
exports.run = ->
  parseOptions()
  return usage()                         if opts.help
  return version()                       if opts.version
  return require './repl'                if opts.interactive
  return compileStdio()                  if opts.stdio
  return compileScript null, source      if opts.eval
  return require './repl'                unless source?
  literals = if opts.run then opts.arguments[1..] else []
  process.argv = process.argv[0..1].concat literals
  process.argv[0] = 'swark'
  compileFile source, path.normalize source

compileFile = (path, base) ->
  try
    code = fs.readFileSync path
  catch err
    if err.code is 'ENOENT'
      console.error "File not found: #{path}"
      process.exit 1
    else
      throw err

  if opts.assembly
    require('./dcpu16exec').runAssembly code.toString()
  else
    compileScript path, code.toString(), base


# Compile a single source script, containing the given code, according to the
# requested options. If evaluating the script directly sets `__filename`,
# `__dirname` and `module.filename` to be correct relative to the script's path.
compileScript = (file, input, base) ->
  o = opts
  options = {filename: file}

  try
    t = task = {file, input, options}
    if      o.tokens      then printTokens Swark.tokens t.input
    else if o.nodes       then printLine Swark.nodes(t.input).toString().trim()
    else if o.run
      require('./dcpu16exec').runAssembly Swark.compile(t.input, t.options)
    else
      t.output = Swark.compile t.input, t.options
      if o.print          then printLine t.output.trim()
      if o.output         then fs.writeFileSync o.output, t.output, "utf8"
  catch err
    printWarn err instanceof Error and err.stack or "ERROR: #{err}"
    process.exit 1

# Attach the appropriate listeners to compile scripts incoming over **stdin**,
# and write them back to **stdout**.
compileStdio = ->
  code = ''
  stdin = process.openStdin()
  stdin.on 'data', (buffer) ->
    code += buffer.toString() if buffer
  stdin.on 'end', ->
    compileScript null, code

# Pretty-print a stream of tokens.
printTokens = (tokens) ->
  strings = for token in tokens
    [tag, value] = [token[0], token[1].toString().replace(/\n/, '\\n')]
    "[#{tag} #{value}]"
  printLine strings.join(' ')

# Use the [OptionParser module](optparse.html) to extract all options from
# `process.argv` that are specified in `SWITCHES`.
parseOptions = ->
  optionParser  = new optparse.OptionParser SWITCHES, BANNER
  o = opts      = optionParser.parse process.argv[2..]
  o.compile     or=  !!o.output
  o.run         = not (o.compile or o.print)
  o.print       = !!  (o.print or (o.eval or o.stdio and o.compile))
  if o.arguments.length > 1
    console.error "can only specify one source file"
    process.exit 1
  source        = o.arguments[0]
  return

# Print the `--help` usage message and exit. Deprecated switches are not
# shown.
usage = ->
  printLine (new optparse.OptionParser SWITCHES, BANNER).help()

# Print the `--version` message and exit.
version = ->
  printLine "swark version #{Swark.VERSION}"
