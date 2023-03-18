use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos()]
pub enum Token {
    #[token("‿")]
    Stranding,
    #[token("@")]
    Null,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(".")]
    Dot,
    #[regex(r#"[;:?]"#)]
    Punct,
    #[regex(r#"[⋄,\n]"#)]
    Delim,
    #[regex("[←⇐↩]")]
    Assign,
    #[regex("[𝕎𝕏𝔽𝔾𝕊]")]
    SpecFun,
    #[regex("[𝕨𝕩𝕗𝕘𝕤]")]
    SpecVal,
    #[regex("_𝕣_|𝕣")]
    SpecMod2,
    #[regex("'[a-zA-Z]'")]
    Char,
    #[regex(r#""(?:\\.|[^\\"])*""#)]
    String,
    #[regex(r#"\d+(?:\.\d+)?(?:[eE]\d+)?"#)]
    Number,
    #[regex(r#"[-×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!+]"#)]
    Primitive,
    #[regex("[˙˜˘¨⌜⁼´˝`]", priority = 2)]
    OneModifier,
    #[regex("[∘○⊸⟜⌾⊘◶⎉⚇⍟⎊]")]
    TwoModifier,
}
