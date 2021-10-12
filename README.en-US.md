[简体中文版本](./README.md)

# About this project


This is `NOT` a part of [GNU Emacs](https://www.gnu.org/software/emacs/) but a personal configuration maintained by © Cabins from China.

# Target

1. Emacs 28 oriented
2. Full-featured config (my another repo for vanilla Emacs)
3. Speed oriented

# About Programming language

Thanks for LSP, you just need install the property language server. Except for personalized config.

# About the default FONT

Choose the available font from a specific list, add the font you preferred to the start of the list in `init-ui.el` file.

# Installation

Launch a terminal, and run the code below, and then launch your Emacs, enjoy it.

```bash
git clone https://github.com/cabins/tenon-emacs ~/.emacs.d
```

> Note: If you use Windows 10,  you should set an environment variable named `HOME`,  and set its value to your user directory,  like `C:/Users/<your_name>`. Otherwise emacs will use `%Appdata%` as home directory by default.

## Testing

This project is tested on

- macOS 11.5
- Windows 10

it should run on other platforms, please feedback by issue if not.

## Special for Chinese user

> MacType issue

If you use Mactype on your machine, you should add emacs to mactype's exlude list, to avoid the blinking issue.

> For Windows user

If you found font issue, you may need install fonts with all-the-icon, and install Symbola manually.
