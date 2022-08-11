#!/bin/bash
typoo
(typoo)

if stuff
then
    somestuff
    test = (())
    (())
    a = (1 + (2 + 3))
    if
    then
    elif
    then
    else
    fi
else
    otherstuff
fi

if stuff
    then
    somestuff
    else
    otherstuff
fi

function f() {
    if
    fi
}


# check if command exists
command_exists () {
  type "${1}"  > /dev/null 2>&1;
  a = (1 + (2 + 3))
}

# Fetch the update
fetch() {
  if type wget > /dev/null 2>&1 ; then
    $debug && echo "fetching update via wget"
    wget --no-check-certificate -O "${2}" "${1}" >/dev/null 2>&1
  elif type curl > /dev/null 2>&1 ; then
    $debug && echo "fetching update via curl"
    curl --insecure --remote-name -o "${2}" "${1}" >/dev/null 2>&1
  else
    echo 'Warning: Neither wget nor curl is available. online updates unavailable' >&2
    exit 1
  fi
}
