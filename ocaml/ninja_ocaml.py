#!/usr/bin/env python
#
#  Copyright 2015 Anthony Scemama
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
#  This file can be downloaded here:
#  https://raw.githubusercontent.com/scemama/ninja_ocaml/master/ninja_ocaml.py
#

"""Build OCaml projects using ninja."""

__author__ = """Anthony Scemama <scemama@irsamc.ups-tlse.fr>"""

import os
import sys
import subprocess

def _help_ ():
  print """
  1) Download and install ninja :
     https://github.com/martine/ninja/releases/latest
  2) Copy the script into your OCaml project.
  3) Run the script. It will build a default build.ninja file
  4) Edit the build.ninja file
  5) Compile the main target using `ninja`
  6) Compile all the targets using `ninja all`
  """

def create_generated_ninja():
  """Creates the generated.ninja file"""

  # Header
  PACKAGES=""
  THREAD=""
  SYNTAX=""
  OCAMLC_FLAGS=""
  GENERATED_NINJA="generated.ninja"
  with open('build.ninja','r') as f:
      for line in f:
          if line.startswith("PACKAGES"):
              PACKAGES=line.split('=',1)[1].strip()
          elif line.startswith("THREAD"):
              THREAD=line.split('=',1)[1].strip()
          elif line.startswith("SYNTAX"):
              SYNTAX=line.split('=',1)[1].strip()
          elif line.startswith("OCAMLC_FLAGS"):
              OCAMLC_FLAGS=line.split('=',1)[1].strip()
          elif line.startswith("LINK_FLAGS"):
              LINK_FLAGS=line.split('=',1)[1].strip()
          elif line.startswith("GENERATED_NINJA"):
              GENERATED_NINJA=line.split('=',1)[1].strip()

  if PACKAGES != "":
      LINK_FLAGS = "-linkpkg "+PACKAGES

  header = [
"""
########################################################
# This file was auto-generated.                        #
# This file will be overwritten. Don't edit this file! #
# Changes should be done in the build.ninja file.      #
########################################################

""",

             "PACKAGES=%s"%(PACKAGES),
             "THREAD=%s"%(THREAD),
             "SYNTAX=%s"%(SYNTAX),
             "OCAMLC_FLAGS=%s"%(OCAMLC_FLAGS),
             "LINK_FLAGS=%s"%(LINK_FLAGS),
             "GENERATED_NINJA=%s"%(GENERATED_NINJA),
             ]

  header += """
rule ocamlc
  command = ocamlfind ocamlc -c $OCAMLC_FLAGS $THREAD $PACKAGES $SYNTAX -o $out $in
  description = Compiling $out (bytecode)

rule ocamlopt
  command = ocamlfind ocamlopt -c $OCAMLC_FLAGS $THREAD $PACKAGES $SYNTAX -o $o $in
  description = Compiling $out (native)

rule ocamlc_link
  command = ocamlfind ocamlc $OCAMLC_FLAGS $THREAD $LINK_FLAGS $PACKAGES $SYNTAX -o $out $in
  description = Compiling $out (bytecode)

rule ocamlopt_link
  command = ocamlfind ocamlopt $OCAMLC_FLAGS $THREAD $LINK_FLAGS $PACKAGES $SYNTAX -o $out $in
  description = Compiling $out (native)

""".splitlines()

  # Get the list of .ml files
  all_files = os.listdir(os.getcwd())
  files = [ os.path.splitext(i)[0] for i in all_files if i.endswith('.ml') ]
  while "myocamlbuild" in files:
    files.remove("myocamlbuild")
  ml_files = ' '.join( [ '%s.ml'%i for i in files ] )

  # Dependencies
  result = subprocess.Popen(
      ("ocamlfind ocamldep {0} {1} {2}".format(PACKAGES,SYNTAX,ml_files)).split()
      ,stdout=subprocess.PIPE).communicate()[0]
  result = result.replace('\\\n',' ')
  dependencies = {}
  for line in result.splitlines():
      key, value = line.split(':')
      dependencies[key.strip()] = value.strip()

  result = header
  template = """
build {0}.cmi: ocamlc {0}.mli  | $GENERATED_NINJA
build {0}.cmo: ocamlc {0}.ml  | $GENERATED_NINJA {1}
build {0}.cmx {0}.o: ocamlopt {0}.ml | $GENERATED_NINJA {2}
  o = {0}.o
"""

  template_root_byte = """
build {2}.byte: ocamlc_link {1} {0}
"""

  template_root_native = """
build {2}: ocamlopt_link {1} {0}
"""

  # Find roots
  dep = {}
  for f in dependencies:
      dep[f] = [ i.strip() for i in dependencies[f].split() ]

  roots = {}
  for f in dependencies:
      Found = False
      for g,l in dep.iteritems():
          if f in l:
              Found = True
      if not Found:
         roots[f] = []

  def get_deps(l):
    result = []
    for i in l:
      if i in dep:
        result += get_deps(dep[i])
    result += l
    newresult = []
    for r in result:
      if r not in newresult:
        newresult.append(r)
    return newresult

  for r in roots:
    roots[r] = [ i for i in get_deps(dep[r]) if not i.endswith(".cmi") ]

  # Write the $GENERATED_NINJA file
  result += [ template.format(basename,
                              dependencies["%s.cmo"%basename],
                              dependencies["%s.cmx"%basename]
                              ) for basename in files ]
  result += [ template_root_byte.format(basename,
                               ' '.join(roots[basename]),
                               os.path.splitext(basename)[0]
                              ) for basename in roots if basename.endswith('.cmo')]
  result += [ template_root_native.format(basename,
                               ' '.join(roots[basename]),
                               os.path.splitext(basename)[0]
                              ) for basename in roots if basename.endswith('.cmx')]

  output = '\n'.join(result)
  try:
    with open(GENERATED_NINJA,'r') as f:
      inp = f.read()
  except IOError:
    inp = ""

  if inp != output:
    with open(GENERATED_NINJA,'w') as f:
      f.write(output)

def create_build_ninja ():
    with open('build.ninja','w') as f:
        f.write("""
MAIN=
# Main program to build

PACKAGES=
# Required opam packages, for example:
# PACKAGES=-package core,sexplib.syntax

THREAD=
# If you need threding support, use:
# THREAD=-thread

SYNTAX=
# If you need pre-processing, use:
# SYNTAX=-syntax camlp4o

OCAMLC_FLAGS=
# Flags to give to ocamlc, for example:
# OCAMLC_FLAGS=-g -warn-error A

LINK_FLAGS=
# Flags to give to the linker, for example:
# LINK_FLAGS=-cclib '-Wl,-rpath=../lib,--enable-new-dtags'

GENERATED_NINJA=generated.ninja
# Name of the auto-generated ninja file

rule create_generated
  command = python ./ninja_ocaml.py
  description = Finding dependencies between modules

rule run_ninja
  command = ninja -f $in $target
  description = Compiling OCaml executables
  pool = console

rule run_clean
  command = ninja -f $GENERATED_NINJA -t clean ; rm $GENERATED_NINJA
  pool = console
  description = Cleaning directory

rule ocamlc
  command = ocamlfind ocamlc -c $OCAMLC_FLAGS $THREAD $PACKAGES $SYNTAX -o $out $in
  description = Compiling $in (bytecode)

rule ocamlopt
  command = ocamlfind ocamlopt -c $OCAMLC_FLAGS $THREAD $PACKAGES $SYNTAX -o $out $in
  description = Compiling $in (native)

rule ocamlc_link
  command = ocamlfind ocamlc $OCAMLC_FLAGS $THREAD $LINK_FLAGS $PACKAGES $SYNTAX -o $out $in
  description = Compiling $out (bytecode)

rule ocamlopt_link
  command = ocamlfind ocamlopt $OCAMLC_FLAGS $THREAD $LINK_FLAGS $PACKAGES $SYNTAX -o $out $in
  description = Compiling $out (native)


build clean: run_clean
build always $GENERATED_NINJA: create_generated

build $MAIN: run_ninja $GENERATED_NINJA
  target = $MAIN

build all: run_ninja $GENERATED_NINJA
  target =

default $MAIN

""")


def main():

    for h in "help -h -help --help ?".split():
        if h in sys.argv:
            _help_ ()
            return

    if "build.ninja" in os.listdir(os.getcwd()):
        create_generated_ninja ()
    else:
        create_build_ninja ()
        print """
==========================================================
A default build.ninja file was created.
Now, edit build.ninja and compile your project using:

  ninja

==========================================================
"""

if __name__ == '__main__':
    main()

