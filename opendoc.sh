#!/bin/bash

if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  open _build/default/_doc/_html/Music/Music/
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    # WSL
    DOCPATH=$(wslpath -w ./_build/default/_doc/_html/Music/Music/)
    explorer.exe ${DOCPATH} || true
    # Why `|| true`? For unknown reasons, explorer.exe returns error code 1 even
    # when it succeeds in opening the path in a window.
  else
    nautilus _build/default/_doc/_html/Music/Music/
  fi
fi
