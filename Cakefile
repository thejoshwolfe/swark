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

# Run a source file through coffeescript
run = (args, cb) ->
  proc =         spawn './node_modules/coffee-script/bin/coffee', args
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

get_mtime = (file) ->
  try new Date(fs.statSync(file).mtime)
  catch e
    new Date(0)

build = ->
  files = fs.readdirSync 'src'
  files = ('src/' + file for file in files when file.match(/\.coffee$/))
  out_dir = 'lib/swark'
  is_out_of_date = (file) ->
    out_file = file.replace /^src\/(.*)\.coffee$/, "#{out_dir}/$1.js"
    return get_mtime(file) > get_mtime(out_file)
  files = (file for file in files when is_out_of_date file)
  if files.length > 0
    console.log "compiling #{files.length} coffee file#{if files.length == 1 then "" else "s"} to js"
    run ['-c', '-o', out_dir].concat(files), build_parser
  else
    build_parser()

build_parser = ->
  for key, val of require('util')
    global[key] = val
  in_file = 'lib/swark/grammar.js'
  out_file = 'lib/swark/parser.js'
  if get_mtime(in_file) > get_mtime(out_file)
    console.log 'building parser'
    require 'jison'
    parser = require('./lib/swark/grammar').parser
    fs.writeFileSync out_file, parser.generate()
  build_dropin_dasm()

build_dropin_dasm = ->
  # TODO: code reuse?
  source_regex = /^(.*)\.dasm(?:16)?$/
  files = fs.readdirSync 'src'
  files = (file for file in files when file.match source_regex)
  out_dir = 'lib/swark'
  to_source_file = (file) -> "src/" + file
  to_out_file = (file) -> file.replace source_regex, "#{out_dir}/$1.js"
  is_out_of_date = (file) ->
    source_file = to_source_file file
    out_file = to_out_file file
    return get_mtime(source_file) > get_mtime(out_file)
  files = (file for file in files when is_out_of_date file)
  if files.length > 0
    console.log "compiling #{files.length} dropin dasm file#{if files.length == 1 then "" else "s"} to js"
    fs.writeFileSync to_out_file(file), "exports.source=#{JSON.stringify fs.readFileSync to_source_file(file), 'utf8'};\n" for file in files

task 'build', 'build everything', build

task 'clean', 'delete auto-generated files', ->
  exec([
    "rm -rf lib"
  ])

