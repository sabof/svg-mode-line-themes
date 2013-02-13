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
Font size calculation is imperfect, so it might require adjustments for your
particular font/size combination. Should the font/size be different from the
emacs default, clicking and row auto-hiding will not work precisely. The
following example shows how to explicitly set font parameters.

```lisp
(let (( theme-archetype (cdr (assoc 'archetype smt/themes)))
      ( row-archetype (cdr (assoc 'archetype smt/rows))))
  (setf (getf row-archetype :baseline) 12)
  (setf (getf theme-archetype :style)
        (list :font-family "DejaVu Sans Mono"
              :font-size "10pt")))
```

## Usage

You can cycle through available themes, using `M-x smt/next-theme`

## Creating and modifying themes

You might want to see how the included themes are implemented, but here is a
short introduction.

Three types of objects are used: widgets, rows and themes. Widgets correspond to
bits of text which might have an individual style and on-click behaviour.

Objects are implemented as plists and prototype inheritance is used. Each object
has a `:prototype` property pointing to it's parent. Each object type also has a
"namespace" - an alist (ex. smt/widgets). The prototype can contain a direct
refference to an object, or a name of an object from the namespace. Ultimately
each object of a certain type inherits from an "archetype" object contained
within the corresponding namespace.

By using a macro such as `smt/deftheme`, one creates an object and adds it to the
type's namespace. Should an object with the same name already exist, it will be
replaced - which is an easy way to change the behaviour of existing themes.

Most properties can have direct values, or contain a function that accepts one
value - the object itself, but there are exceptions. The corresponding getter
(such as `smt/w-text`, which retrieves the text from a widget) will return the
value directly, or will return the result of evaluation of a function.

Each theme has a `:rows` property, which is a list of rows. The order is
important - should rows happen to overlap, rows appearing later on the list
will be hidden.

Here is an example of creating a theme derived from `black-crystal`, but without
the line-number information:

```lisp
(smt/deftheme black-crystal-no-pager
  :prototype 'black-crystal
  :rows (list
         'default-left
         (smt/make-row
          :prototype 'default-right
          :margin 2)))
```

Themes also have a property called `:local-widgets` - an alist which can shadow
globally defined widgets. This is an example of `diesel`, with blue titles.

```lisp
(defun smt/diesel-blue-title-style (widget)
  ;; smt+ combines styles
  (smt/+ (smt/t-style (smt/t-prototype widget))
         (list :fill (if (smt/window-active-p)
                         "#21D0EE"
                         "#4C5055")
               :font-weight "bold")))

(smt/deftheme diesel-blue
  :prototype 'diesel
  :local-widgets
  (lambda (widget)
    (let (( parent-local-widgets
            (smt/t-local-widgets
             (smt/t-prototype widget))))
      (append (list (cons 'buffer-name
                          (smt/make-widget
                           :prototype 'buffer-name
                           :style 'smt/diesel-blue-title-style))
                    (cons 'minor-modes
                          (smt/make-widget
                           :prototype 'minor-modes
                           :style 'smt/diesel-blue-title-style)))
              parent-local-widgets))))
```
