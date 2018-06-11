(use-package hydra
  :config
  (defhydra zoom-font (global-map "<f2>")
    "zoom font"
    ("+" increase-font-size)
    ("-" decrease-font-size)))

(provide 'init-hydra)
