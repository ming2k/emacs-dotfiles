# 0001 Preserve Minibuffer History With Savehist

## Status

Accepted

## Context

Vertico's history-based sorting depends on minibuffer history variables being restored before commands invoke completion. For `org-roam-node-find`, the relevant variable is `org-roam-node-history`.

The configuration stores Emacs state under XDG paths, including `~/.local/state/emacs/history`. If `savehist-file` is set after `(savehist-mode 1)`, savehist can read the default history file during startup and write the XDG history file during shutdown. That split makes history sorting appear to reset after restart.

Desktop persistence can also save global variables. If desktop and savehist both persist `org-roam-node-history`, stale desktop state can overwrite the richer savehist value.

## Decision

Set `savehist-file` before enabling `savehist-mode`.

Persist `org-roam-node-history` and `org-roam-ref-history` with savehist.

Do not persist Org-roam history variables with desktop.

## Consequences

Vertico can use `org-roam-node-history` across Emacs restarts.

There is one persistence owner for Org-roam minibuffer history.

Previously overwritten history cannot be restored from the current savehist file.
