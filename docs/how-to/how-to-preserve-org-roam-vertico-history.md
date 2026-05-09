# How to Preserve Org-roam Vertico History

## Goal

Keep recently selected `C-c n f` Org-roam nodes near the top of Vertico results after restarting Emacs.

## Steps

1. Configure `savehist-file` before enabling `savehist-mode`.

   ```emacs-lisp
   (setq savehist-file (expand-file-name "emacs/history" my/xdg-state-home)
         savehist-additional-variables
         '(mark-ring
           global-mark-ring
           search-ring
           regexp-search-ring
           extended-command-history
           org-roam-node-history
           org-roam-ref-history))
   (savehist-mode 1)
   ```

2. Keep `org-roam-node-history` out of `desktop-globals-to-save`.

   ```emacs-lisp
   ;; Do not add org-roam-node-history here.
   ;; Do not add org-roam-ref-history here.
   ```

3. Select several notes with `C-c n f`.

4. Exit Emacs normally.

5. Check that the XDG history file contains more than one recent Org-roam entry.

   ```bash
   rg -n "org-roam-node-history|savehist" ~/.local/state/emacs/history
   ```

6. Restart Emacs and press `C-c n f`.

7. Confirm recently selected notes appear near the top of Vertico results.

## Verify Current Config

Check the order in `lisp/init-session.el`:

```text
savehist-file
savehist-additional-variables
savehist-mode
```

`savehist-file` must not be set only after `(savehist-mode 1)`.

## Repair A Broken Setup

1. Move `savehist-file` into the `:init` section before `(savehist-mode 1)`.

2. Keep `org-roam-node-history` in `savehist-additional-variables`.

3. Remove any active desktop saving of `org-roam-node-history`.

4. Rebuild the history by selecting notes again with `C-c n f`.

Previously overwritten history entries cannot be reconstructed from the current savehist file.

## Troubleshoot

Check whether savehist is writing the expected file:

```bash
ls -l ~/.local/state/emacs/history
```

Check whether Org-roam history is present:

```bash
rg -n "org-roam-node-history" ~/.local/state/emacs/history
```

Check whether the display string changed:

```bash
rg -n "org-roam-node-display-template" ~/.emacs.d/lisp ~/.config/emacs/lisp
```

If the display template changed, rebuild history by selecting the affected nodes again.
