# Org-Mode Configuration

Modular org-mode configuration with completion disabled for distraction-free writing.

## Structure

- `config.el` - Main loader
- `org.el` - Core org-mode settings
- `org-roam.el` - Zettelkasten note-taking

## Key Features

### Core Org-Mode
- **Directory**: `~/org/` (main), `~/org/roam/` (notes)
- **TODO states**: TODO → DOING → DONE/CANCELLED
- **Completion**: Disabled for writing focus, manual with `C-c TAB`

### Org-Roam
- **Structure**: `~/org/roam/YYYY/MM/DD/YYYYMMDDHHMMSS.org`
- **Templates**: default (d), project (p), note (n)
- **Database**: Auto-sync enabled

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `C-c a` | org-agenda | Open agenda |
| `C-c c` | org-capture | Quick capture |
| `C-c n f` | org-roam-node-find | Find/create note |
| `C-c n i` | org-roam-node-insert | Insert link |
| `C-c TAB` | Manual completion | File paths only |

## Workflow

1. Use `C-c c t` for tasks
2. Use `C-c n f` for knowledge notes  
3. Review with `C-c a`
4. Manual completion when needed with `C-c TAB`