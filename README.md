# svg-mode-line-themes
![screenshot](https://github.com/sabof/svg-mode-line-themes/raw/master/screenshot.png)

## Installation

You need to obtain [xmlgen](https://github.com/philjackson/xmlgen). When
installing through package.el, this should be done automatically.

Add to your .emacs

```lisp
(require 'svg-mode-line-themes)
(smt/enable)
(smt/set-theme 'black-crystal)
(set-face-attribute
   'mode-line nil
   :box nil)
```

## Usage

### Cycle through available themes

`M-x smt/next-theme`

### Currently available themes:

* nasa
* black-crystal
* diesel

## Customization

Font size calculation is imperfect, so it might require adjustments for your
paritcular font/size combination. Should the font/size be different from the
defualt, the clicking and element auto-hiding will not work precisely. The
following example shows how to explicitly set font parameters.

```lisp
(let* (( archetype (cdr (assoc 'archetype smt/themes))))
  (setf (getf archetype :baseline) 12)
  (setf (getf archetype :style)
        (list :font-family "DejaVu Sans Mono"
              :font-size "10pt")))
```
