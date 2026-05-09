# Completion and History Reference

## Files

| Path | Purpose |
|------|---------|
| `lisp/init-completion.el` | Completion styles, Vertico, Marginalia, Consult, Corfu |
| `lisp/init-session.el` | Savehist, desktop, recentf, saveplace |
| `lisp/init-org.el` | Org-roam keybindings and capture templates |
| `~/.local/state/emacs/history` | Savehist state |

## Completion Stack

| Layer | Setting | Value |
|-------|---------|-------|
| Matching | `completion-styles` | `(orderless basic)` |
| Category defaults | `completion-category-defaults` | `nil` |
| File override | `completion-category-overrides` | `((file (styles basic partial-completion orderless)))` |
| UI | `vertico-mode` | enabled |
| Count | `vertico-count` | `15` |
| Resize | `vertico-resize` | `t` |
| Cycle | `vertico-cycle` | `t` |

## Vertico Sort Precedence

1. `vertico-sort-override-function`
2. completion metadata `display-sort-function`
3. `vertico-sort-function`
4. `identity`

## Common Vertico Sorters

| Function | Order |
|----------|-------|
| `vertico-sort-history-length-alpha` | history, length, alpha |
| `vertico-sort-history-alpha` | history, alpha |
| `vertico-sort-length-alpha` | length, alpha |
| `vertico-sort-alpha` | alpha |
| `nil` | no Vertico sorter |

## History Variables

| Command | History variable |
|---------|------------------|
| `org-roam-node-find` | `org-roam-node-history` |
| `org-roam-ref-find` | `org-roam-ref-history` |
| `M-x` | `extended-command-history` |
| `find-file` | `file-name-history` |
| search commands | `search-ring`, `regexp-search-ring` |

## Savehist Requirements

| Requirement | Value |
|-------------|-------|
| `savehist-file` set before `savehist-mode` | required |
| `org-roam-node-history` in `savehist-additional-variables` | required |
| `org-roam-ref-history` in `savehist-additional-variables` | required |
| `savehist-save-minibuffer-history` | `t` |
| `org-roam-node-history` in `desktop-globals-to-save` | forbidden |
| `org-roam-ref-history` in `desktop-globals-to-save` | forbidden |

## Command Actions After Selection

| Command | Candidate | Action after `RET` |
|---------|-----------|--------------------|
| `execute-extended-command` / `M-x` | command name | call command |
| `find-file` | file path | visit file |
| `switch-to-buffer` | buffer name | switch buffer |
| `org-roam-node-find` | node title or alias | visit existing node or start capture |
| `org-roam-node-insert` | node title or alias | insert link or start capture |

## Org-roam Candidate Shape

| Form | Meaning |
|------|---------|
| `(DISPLAY-STRING . ORG-ROAM-NODE)` | completion alist entry |
| `DISPLAY-STRING` | Vertico display and savehist value |
| `ORG-ROAM-NODE` | object used by org-roam after selection |

## Diagnostics

| Check | Command |
|-------|---------|
| Saved Org-roam history | `rg -n "org-roam-node-history" ~/.local/state/emacs/history` |
| Savehist config | `rg -n "savehist-file|savehist-additional-variables|savehist-mode" lisp/init-session.el` |
| Vertico config | `rg -n "vertico|completion-styles|completion-category" lisp/init-completion.el` |
| Org-roam display template | `rg -n "org-roam-node-display-template" lisp/init-org.el` |
| Desktop history conflict | `rg -n "desktop-globals-to-save|org-roam-node-history" lisp/init-session.el` |
