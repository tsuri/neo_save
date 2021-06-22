;;; -*- lexical-binding: t -*-

;;; speadup startup by leaving garbage around
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(setq package-archives nil)

(advice-add #'x-apply-session-resources :override #'ignore)

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
