;;; basic-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'faster-whichkey)

(describe "sanity"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "is loaded" (expect (featurep 'faster-whichkey) :to-be t))
)
