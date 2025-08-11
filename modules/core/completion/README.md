# Completion System

This directory contains the modular completion system for Emacs, providing mode-specific completion behaviors while maintaining a consistent user experience.

## Architecture

### Core System (`config.el`)

The main completion configuration provides:

- **Corfu**: In-buffer completion popup with VS-like ordering
- **Vertico**: Enhanced minibuffer completion
- **Marginalia**: Rich annotations for completion candidates
- **Orderless**: Flexible completion matching styles
- **LSP Integration**: Automatic eglot setup for programming languages
- **Safe Dabbrev**: Error-resistant word completion wrapper
- **Performance Optimizations**: 0.1s delay, smart caching, optimized backends

### Mode-Specific Modules

Each mode has its own completion behavior tailored to its workflow:

#### Programming Modes
- **Enabled**: Auto-completion via Corfu
- **Backends**: LSP (eglot) + safe-dabbrev + file completion
- **Languages**: Python, Rust, JavaScript/TypeScript, Go, C/C++
- **Features**: Smart ordering, usage tracking, immediate completions

#### Text/Writing Modes
- **Disabled**: Auto-completion turned off to prevent interference
- **Manual Completion**: Available via keybindings when needed
- **Modes**: Org-mode, Org-roam, Markdown

## File Structure

```
modules/core/completion/
├── config.el                    # Core completion system
├── org-completion.el            # Org-mode completion behavior
├── org-roam-completion.el       # Org-roam completion behavior
├── markdown-completion.el       # Markdown completion behavior
└── README.md                    # This file
```

## Module Details

### `org-completion.el`
- **Purpose**: Org-mode completion configuration
- **Behavior**: Disables auto-completion, provides manual file completion
- **Functions**:
  - `org-disable-completion()` - Turns off corfu-auto and clears completion functions
  - `org-manual-path-completion()` - Manual file path completion
- **Setup**: Hooks into `org-mode-hook` when org is loaded

### `org-roam-completion.el`
- **Purpose**: Org-roam specific completion behavior
- **Behavior**: Inherits from org-mode, additional roam-specific disabling
- **Functions**:
  - `org-roam-disable-completion()` - Disables completion for roam buffers
  - `setup-org-roam-completion()` - Hooks into roam file finding
- **Setup**: Hooks into `org-roam-find-file-hook` when org-roam is loaded

### `markdown-completion.el`
- **Purpose**: Markdown mode completion configuration  
- **Behavior**: Follows org-mode pattern, disabled auto-completion
- **Functions**:
  - `markdown-disable-completion()` - Turns off corfu-auto and clears completion functions
  - `markdown-manual-path-completion()` - Manual file path completion
- **Setup**: Hooks into `markdown-mode-hook` and `gfm-mode-hook`

## Usage

### Automatic Loading

All completion modules are automatically loaded by the main config when their respective packages are available:

```elisp
;; In config.el
(require 'org-completion)
(require 'org-roam-completion)  
(require 'markdown-completion)
```

### Manual Completion Triggers

When auto-completion is disabled, use these manual triggers:

- `C-c c` - Manual completion
- `M-/` - Alternative manual completion  
- `C-M-i` - Standard completion-at-point
- `C-c TAB` - Manual file path completion (org/markdown specific)

### Programming Mode Completion

In programming modes, completion is automatic:

- **Trigger**: 1 character prefix, 0.1s delay
- **Navigation**: `TAB`/`C-n`/`C-p` in popup
- **Backends**: LSP first, then dabbrev, then file completion
- **Features**: Smart ordering, usage tracking, error handling

## Customization

### Adding New Mode-Specific Completion

To add completion behavior for a new mode:

1. **Create new file**: `modules/core/completion/mymode-completion.el`
2. **Follow the pattern**:
   ```elisp
   ;;; mymode-completion.el -*- lexical-binding: t; -*-
   ;;; Commentary:
   ;; MyMode specific completion configuration
   ;;; Code:
   
   (defun mymode-disable-completion ()
     "Disable automatic completion in mymode."
     (setq-local corfu-auto nil)
     (setq-local completion-at-point-functions nil))
   
   (defun setup-mymode-completion ()
     "Setup mymode completion behavior."
     (add-hook 'mymode-hook #'mymode-disable-completion))
   
   (with-eval-after-load 'mymode
     (setup-mymode-completion))
   
   (provide 'mymode-completion)
   ;;; mymode-completion.el ends here
   ```
3. **Add to config.el**: `(require 'mymode-completion)`
4. **Test thoroughly**

### Modifying Existing Behavior

To modify completion behavior for existing modes:

1. **Edit the relevant file** (e.g., `org-completion.el`)
2. **Maintain the established patterns**
3. **Test with the actual mode active**
4. **Update this README** if behavior significantly changes

### Performance Tuning

Key performance settings in `config.el`:

```elisp
;; Corfu performance
(corfu-auto-delay 0.1)        ; Response time
(corfu-auto-prefix 1)         ; Minimum chars to trigger
(corfu-scroll-margin 5)       ; Popup scroll behavior

;; Completion styles
(completion-styles '(orderless basic))  ; Matching algorithms
(completion-cycle-threshold 3)          ; Tab completion behavior
```

## Troubleshooting

### Common Issues

#### Auto-completion not working in programming modes
- **Check**: LSP server is installed and running
- **Verify**: `eglot-managed-mode` is active in the buffer
- **Debug**: Check `*eglot events*` buffer for errors

#### Manual completion not working in text modes  
- **Check**: Mode-specific completion module is loaded
- **Verify**: `corfu-auto` is nil in the buffer
- **Try**: Different manual trigger keybindings

#### Completion errors or crashes
- **Check**: `*Messages*` buffer for error details
- **Verify**: `safe-dabbrev-capf` is being used instead of raw `dabbrev-capf`
- **Debug**: Use `emacs --debug-init` to catch initialization issues

#### Performance problems
- **Reduce**: `corfu-auto-delay` if completion is too eager
- **Increase**: `corfu-auto-prefix` to require more characters
- **Check**: Large buffers or complex completion sources

### Debug Commands

```elisp
;; Check completion functions for current buffer
(message "Completion functions: %s" completion-at-point-functions)

;; Check if corfu-auto is enabled
(message "Corfu auto: %s" corfu-auto)

;; Test manual completion
(call-interactively 'completion-at-point)

;; Check LSP status
(eglot-current-server)
```

## Development

### Code Style

- Use `with-eval-after-load` for package-specific setup
- Prefix functions with mode name (e.g., `org-disable-completion`)
- Include descriptive docstrings
- Follow existing naming patterns
- Test in isolation before integration

### Testing

- Test with relevant modes active
- Verify both automatic and manual completion
- Check performance with large buffers  
- Test error conditions (missing LSP, malformed text)
- Ensure no conflicts with other packages

### Contributing

1. **Follow established patterns** in existing modules
2. **Test thoroughly** before proposing changes
3. **Update documentation** for new features
4. **Consider backward compatibility**
5. **Maintain performance standards**