(module empty scheme/base
;  (require (for-syntax scheme/base))
;  (provide (for-syntax #%app #%top #%datum #%module-begin))
  (provide #%app #%top #%datum #%module-begin 
	   require only-in except-in
	   provide all-defined-out all-from-out))
;	   (all-from-except #%kernel define lambda)))
