if exists('b:current_syntax')
  finish
endif

syntax keyword cblcKeyword class struct private public import const if else while for return
syntax keyword cblcType void int float double char string bool
syntax keyword cblcBoolean true false
syntax keyword cblcBuiltin display
syntax match cblcStd /\<std::[A-Za-z_][A-Za-z0-9_]*\>/
syntax match cblcNumber /\v<\d+(\.\d+)?>/
syntax region cblcString start=/"/ skip=/\\"/ end=/"/
syntax region cblcChar start=/'/ skip=/\\'/ end=/'/
syntax match cblcComment /\/\/.*$/
syntax match cblcOperator /[-+*\/=<>!&|.:]\+/

highlight default link cblcKeyword Keyword
highlight default link cblcType Type
highlight default link cblcBoolean Boolean
highlight default link cblcBuiltin Function
highlight default link cblcStd Function
highlight default link cblcNumber Number
highlight default link cblcString String
highlight default link cblcChar Character
highlight default link cblcComment Comment
highlight default link cblcOperator Operator

let b:current_syntax = 'cblc'
