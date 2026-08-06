# About

This is `NOT` a part of [GNU Emacs](https://www.gnu.org/software/emacs/) but a personal-daily-use configuration maintained by @ Cabins from China.

More details can be found from wiki([wiki](https://github.com/cabins/emacs.d/wiki))

# Dev Code

`噪鹃`, English name? No, just Chinese `噪鹃`.

# Target

1. Works on Windows & macOS & GNU/Linux & Android (By [termux](https://termux.com/))
2. Lightweight (as possible as vanilla with built-in packages)
3. Only latest version of Emacs (current is 31+) is supported, old version config may be found in other branch (v28, v30...)

# Programming

By [Eglot](https://github.com/joaotavora/eglot) (built-in since v29).

All you need to do is `install the specific server and put it into the PATH environment variable`. The supported servers are listed in Eglot / lsp-mode repo.

> Solution for jdtls on Windows issue: install `jdtls` with `scoop` will be good.
> `scoop install jdtls`

As treesit is added as built-in package, some programming mode now is managed by `-ts-mode`, such as `go-ts-mode`, `rust-ts-mode` etc.

# Installation

1. Just run the code below:

```bash
git clone https://github.com/cabins/emacs.d ~/.emacs.d
```

2. Launch Emacs. No package installation is needed: this config only uses
   built-in packages (Emacs 31+).
3. Enjoy the life.

# Change Log since v31

- treesit settings
- markdown-ts-mode built-in
- smarter, eager completion
- dropped all third-party packages, pure builtin config
