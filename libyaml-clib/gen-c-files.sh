#!/bin/sh

set -xe

if [ -z "$1" ] ; then
	echo "No yaml version specified, aborting..."
	exit 1
fi

LIBYAML_VER="$1"
LIBYAML_BALL="yaml-${LIBYAML_VER}.tar.gz"
LIBYAML_URL="http://pyyaml.org/download/libyaml/${LIBYAML_BALL}"

[ -f "${LIBYAML_BALL}" ] || curl -LO "${LIBYAML_URL}"

[ -d "yaml-${LIBYAML_VER}" ] || tar xf "${LIBYAML_BALL}"

(
cd "yaml-${LIBYAML_VER}"

rm -f ../cbits/*.[ch] ../cbits/License
cp src/api.c \
   src/dumper.c \
   src/emitter.c \
   src/loader.c \
   src/parser.c \
   src/reader.c \
   src/scanner.c \
   src/writer.c \
   src/yaml_private.h \
   include/yaml.h \
   License \
   ../cbits/
)

rm -r "yaml-${LIBYAML_VER}"
rm "${LIBYAML_BALL}"

