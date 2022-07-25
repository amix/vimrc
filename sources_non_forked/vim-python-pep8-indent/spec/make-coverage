#!/bin/sh

set -ex

rm -f .coverage_covimerage
export PYTHON_PEP8_INDENT_TEST_PROFILE_BASE=/tmp/vim-python-pep8-profile

Xvfb :99 2>/dev/null >&2 &
export DISPLAY=:99

export VIMRUNNER_REUSE_SERVER=1

ret=0
for file in ./spec/indent/*_spec.rb; do
  # shellcheck disable=SC2086
  bundle exec rspec "$file" $RSPEC_OPTIONS || ret=1

  for p in "${PYTHON_PEP8_INDENT_TEST_PROFILE_BASE}".*; do
    covimerage write_coverage --append "$p"
    rm "$p"
    covimerage report -m
  done
done
exit $ret
