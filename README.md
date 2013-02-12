# svg-mode-line-themes
![screenshot](https://github.com/sabof/svg-mode-line-themes/raw/master/screenshot.png)

## Usage

Add to your .emacs

### Installation

You need to obtain [xmlgen](https://github.com/philjackson/xmlgen). When
installing through package.el, this should be done automatically.

```lisp
(require 'svg-mode-line-themes)
(smt/enable)
(smt/set-theme 'black-crystal)
```

### Usage
## Cycle through available themes

`M-x smt/next-theme`

## Currently available themes:

* nasa
* black-crystal
* diesel

### Customization

Font size calculation is imperfect at the moment, so it might require
adjustments for your paritcular font/size. The easiest way to do it for all
themes is by changing the corresponding "archetype" objects. Ex.

```lisp
(let ((archetype (cdr (assoc 'archetype smt/themes))))
  (setf (getf archetype :baseline) 13))

```
