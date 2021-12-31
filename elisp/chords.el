(require 'key-chord)
(key-chord-mode 1)

(require 'cperl-mode)

(setq xms (if (boundp 'xms) 1 0))

(key-chord-define cperl-mode-map "s\/" (concat "s\/\/\/" (when xms "xms")))
(key-chord-define cperl-mode-map "s\\{" (concat "s{}{}" (when xms "xms")))
(key-chord-define cperl-mode-map "s!" (concat "s!!!" (when xms "xms")))

(when xms (key-chord-define cperl-mode-map "//" (concat "//xm" )))
(key-chord-define cperl-mode-map "m!" (concat "m!!" (when xms "xms")))
(key-chord-define cperl-mode-map "m{" (concat "m{}" (when xms "xms")))

(provide 'chords)
