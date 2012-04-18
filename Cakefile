fs            = require 'fs'
path          = require 'path'
{spawn, exec} = require 'child_process'

# ANSI Terminal Colors.
enableColors = no
unless process.platform is 'win32'
  enableColors = not process.env.NODE_DISABLE_COLORS

bold = red = green = reset = ''
if enableColors
  bold  = '\x1B[0;1m'
  red   = '\x1B[0;31m'
  green = '\x1B[0;32m'
  reset = '\x1B[0m'

sources = [
  'swark', 'grammar', 'helpers'
  'lexer', 'nodes', 'rewriter', 'scope'
].map (filename) -> "src/#{filename}.coffee"

# Run a source file through coffeescript
run = (args, cb) ->
  proc =         spawn 'coffee', args
  proc.stderr.on 'data', (buffer) -> console.log buffer.toString()
  proc.on        'exit', (status) ->
    process.exit(1) if status != 0
    cb() if typeof cb is 'function'

# Log a message with a color.
log = (message, color, explanation) ->
  console.log color + message + reset + ' ' + (explanation or '')

option '-p', '--prefix [DIR]', 'set the installation prefix for `cake install`'

task 'install', 'install swark into /usr/local (or --prefix)', (options) ->
  base = options.prefix or '/usr/local'
  lib  = "#{base}/lib/swark"
  bin  = "#{base}/bin"
  node = "~/.node_libraries/swark"
  console.log   "Installing swark to #{lib}"
  console.log   "Linking to #{node}"
  console.log   "Linking 'swark' to #{bin}/swark"
  exec([
    "mkdir -p #{lib} #{bin}"
    "cp -rf bin lib LICENSE README package.json src #{lib}"
    "ln -sfn #{lib}/bin/swark #{bin}/swark"
    "mkdir -p ~/.node_libraries"
    "ln -sfn #{lib}/lib/swark #{node}"
  ].join(' && '), (err, stdout, stderr) ->
    if err then console.log stderr.trim() else log 'done', green
  )


task 'build', 'build the swark language from source', build = (cb) ->
  files = fs.readdirSync 'src'
  files = ('src/' + file for file in files when file.match(/\.coffee$/))
  run ['-c', '-o', 'lib/swark'].concat(files), cb


task 'build:parser', 'rebuild the Jison parser (run build first)', ->
  for key, val of require('util')
    global[key] = val
  require 'jison'
  parser = require('./lib/swark/grammar').parser
  fs.writeFile 'lib/swark/parser.js', parser.generate()

task 'clean', 'delete auto-generated files', ->
  exec([
    "rm -rf lib"
  ])

