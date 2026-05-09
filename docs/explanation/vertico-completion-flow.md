# Vertico Completion Flow

Vertico is the minibuffer UI for the standard Emacs completion API. It does not decide what a selected candidate means. It displays candidates, sorts them, lets the user select one, and returns the selected string to the command that called `completing-read`.

The command owns the action after selection. `M-x` executes a command, `find-file` opens a file, `switch-to-buffer` switches buffers, and `org-roam-node-find` visits or captures an Org-roam node.

## Flow

```text
command
  -> completing-read
     -> completion table
        -> completion metadata
        -> completion-all-completions
           -> completion styles filter candidates
     -> Vertico sorts and displays candidates
     -> user selects a candidate
  -> completing-read returns a string
command handles that string
```

In this configuration, filtering usually comes from `orderless` with `basic` as fallback. File completion has a category override because paths have special completion semantics.

Sorting is separate from filtering. Vertico chooses the sorting function in this order:

```text
vertico-sort-override-function
  -> completion metadata display-sort-function
  -> vertico-sort-function
  -> identity
```

The default Vertico sorter is normally `vertico-sort-history-length-alpha`. It moves candidates present in the active minibuffer history to the front, then sorts remaining candidates by length and alphabetically.

## Org-roam Node Find

`C-c n f` calls `org-roam-node-find`.

```text
org-roam-node-find
  -> org-roam-node-read
     -> org-roam-node-read--completions
        -> org-roam-node-list
        -> org-roam database
        -> completion candidates
     -> completing-read with history variable org-roam-node-history
        -> Vertico UI
  -> existing node: org-roam-node-visit
  -> new title: org-roam-capture-
```

Org-roam builds completion candidates as pairs:

```emacs-lisp
(DISPLAY-STRING . ORG-ROAM-NODE)
```

Vertico sorts and displays the `DISPLAY-STRING`. After selection, `org-roam-node-read` maps the returned string back to the node object.

This means history sorting depends on string equality. If the candidate display string changes, old entries in `org-roam-node-history` may no longer match current candidates.

## Selection

When `RET` is pressed in Vertico, Vertico inserts the current candidate into the minibuffer and exits if the input is valid. Then `completing-read` returns the selected string.

For `org-roam-node-find`, the returned string is not itself the final action. Org-roam uses it to find the node. If the node has a file, org-roam opens that file and jumps to the node position. If the string is a new title, org-roam starts capture using `org-roam-capture-templates`.

## History Persistence

History-based sorting only survives restart if the relevant history variable is restored before completion starts.

For Org-roam, the relevant variable is `org-roam-node-history`. It must be saved by `savehist`, and `savehist-file` must be set before `savehist-mode` is enabled. If `savehist-file` is set too late, Emacs can read one history file at startup and write another at shutdown, which makes Vertico's history sort appear to reset.

Desktop state should not also save `org-roam-node-history`. Two persistence systems saving the same variable can race and overwrite the richer history with stale state.

See also:

- [How to Preserve Org-roam Vertico History](../how-to/how-to-preserve-org-roam-vertico-history.md)
- [Completion and History Reference](../reference/completion-and-history.md)
- [ADR 0001: Preserve Minibuffer History With Savehist](../adr/0001-preserve-minibuffer-history-with-savehist.md)
