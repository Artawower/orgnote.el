# web-roam
Package for syncing roam notes with external second brain service.


## Install
### Doom
**package.el**

``` emacs-lisp
(package! web-roam
  :recipe (:host github :repo "artawower/web-roam.el"))
```

**config.el**

``` emacs-lisp
(use-package! web-roam
  :defer t
  :hook (org-mode . web-roam-sync-mode))
```


