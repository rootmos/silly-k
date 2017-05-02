#!/bin/sh
scheme --compile-imported-libraries --optimize-level 3 --libdirs .:nanopass-framework-scheme:lalr-scm --script c.scm $*
