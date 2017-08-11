#lang at-exp racket/base
(require racket/string)

(provide katex-convert-unicode
         string-replace*)

(define (literal-alternatives→regexp literal-alternatives)
  (string-append "("
                 (string-join (map regexp-quote literal-alternatives) "|")
                 ")"))

(define (string-replace* str mathmode? sym→*)
  (define →* (map (λ (x)
                    (cons (symbol->string (car x))
                          (cadr x)))
                  sym→*))
  (define hash→* (make-immutable-hash →*))
  (regexp-replace* (literal-alternatives→regexp (map car →*))
                   str
                   (λ (found . _)
                     (let ([replacement (hash-ref hash→* found)])
                       (if mathmode?
                           replacement
                           (string-append "$" replacement "$"))))))

(define (katex-convert-unicode str mathmode? [more-sym→* '()])
  (define sym→*
    `([₀ "{}_0"]
      [₁ "{}_1"]
      [₂ "{}_2"]
      [₃ "{}_3"]
      [₄ "{}_4"]
      [₅ "{}_5"]
      [₆ "{}_6"]
      [₇ "{}_7"]
      [₈ "{}_8"]
      [₉ "{}_9"]
      [ᵢ "{}_i"]
      [ⱼ "{}_j"]
      [ₖ "{}_k"]
      [ₗ "{}_l"]
      [ₘ "{}_m"]
      [ₙ "{}_n"]
      [ₒ "{}_o"]
      [ₓ "{}_x"]
      [⁰ "{}^0"]
      [¹ "{}^1"]
      [² "{}^2"]
      [³ "{}^3"]
      [⁴ "{}^4"]
      [⁵ "{}^5"]
      [⁶ "{}^6"]
      [⁷ "{}^7"]
      [⁸ "{}^8"]
      [⁹ "{}^9"]
      [ⁱ "{}^i"]
      [ʲ "{}^j"]
      [ᵏ "{}^k"]
      [ˡ "{}^l"]
      [ᵐ "{}^m"]
      [ⁿ "{}^n"]
      [ᵒ "{}^o"]
      [⊆ "\\subseteq{}" "\\ensuremath{\\subseteq}"]
      [⊂ "\\subset{}" "\\ensuremath{\\subset}"]
      [⊇ "\\supseteq{}" "\\ensuremath{\\supseteq}"]
      [⊃ "\\supset{}" "\\ensuremath{\\supset}"]
      [→ "\\rightarrow{}" "\\ensuremath{\\rightarrow}"]
      [⇒ "\\Rightarrow{}" "\\ensuremath{\\Rightarrow}"]
      [← "\\leftarrow{}" "\\ensuremath{\\leftarrow}"]
      [⇐ "\\Leftarrow{}" "\\ensuremath{\\Leftarrow}"]
      [↔ "\\leftrightarrow{}" "\\ensuremath{\\leftrightarrow}"]
      [⇔ "\\Leftrightarrow{}" "\\ensuremath{\\Leftrightarrow}"]
      ;; Partially extracted from my .XCompose generator
      [ñ "\\tilde{n}" "{\\ifmmode\\tilde{n}\\else\\~{n}\\fi}"]
      [Ñ "\\tilde{N}" "{\\ifmmode\\tilde{N}\\else\\~{N}\\fi}"]
      [⋆ "\\star{}" "\\ensuremath{\\star}"]
      [⍣ "\\ddot{\\star}}" "\\ensuremath{\\ddot{\\star}}"]
      [⃰ "^*" "^*"]
      [⟨ "\\langle{}" "\\ensuremath{\\mathsmaller{\\raisemath{.15ex}{\\langle}}}"]
      [⟩ "\\rangle{}" "\\ensuremath{\\mathsmaller{\\raisemath{.15ex}{\\rangle}}}"]
      [⋯ "\\cdots{}"]
      [⋮ "\\vdots{}"]
      [⋰ "\\iddots{}"]
      [⋱ "\\ddots{}"]
      [⧺ "\\mathbin{+\\mkern-6.5mu+}" "\\ensuremath{\\mathbin{+\\mkern-6.5mu+}}"]
      [∅ "\\emptyset{}" "\\ensuremath{\\emptyset}"]
      [ı⃗ "\\vec{\\i}}" "\\ensuremath{\\vec{\\i}"]
      [⊕ "\\oplus{}" "\\ensuremath{\\oplus}"]
      [⊖ "\\ominus{}" "\\ensuremath{\\ominus}"]
      [⋓ "\\Cup{}" "\\ensuremath{\\Cup}"]
      ;[ₗ "\\ensuremath{_{l}}"]
      [∷ "::"]
      [Λ "\\Lambda{}" "\\ensuremath{\\Lambda}"]
      [∀ "\\forall{}"]
      [∃ "\\exists{}"]
      [≡ "\\equiv{}"]
      [≢ "\\not\\equiv{}"]
      [… "\\ldots{}"]
      [⋯ "\\cdots{}"]
      [⋰ "\\uddots{}"] ;; or \iddots from package mathdots, see http://tex.stackexchange.com/a/17650
      [⋱ "\\ddots{}"]
      [∌ "\\notni{}"]
      [ℰ "\\mathcal{E}"]
      [𝒮 "\\mathcal{S}"]
      [• "\\bullet{}"]
      [|'| "{}'"]
      [′ "{}'"]
      [″ "{}''"]
      [‴ "{}'''"]
      [⁗ "{}''''"]
      [∪ "\\cup"]
      [∩ "\\cap"]
      [⋃ "\\bigcup{}"]
      [⋂ "\\bigcap{}"]
      [⋀ "\\bigwedge{}"]
      [⋁ "\\bigvee{}"]
      [± "\\pm{}"]
      [≟ "\\stackrel{?}{=}"]
      [≛ "\\stackrel{*}{=}"]
      [≝ "\\stackrel{\\scriptscriptstyle\\mathsf{def}}{=}"]
      [∃ "\\exists{}"]
      [⋅ "\\cdot"]
      ))
  (if (string? str)
      (string-replace*
       str
       mathmode?
       (append more-sym→* sym→*))
      str))