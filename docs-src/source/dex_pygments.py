from pygments.lexer import RegexLexer
from pygments.lexer import words
from pygments import token
from sphinx.highlighting import lexers

class DexLexer(RegexLexer):
    name = 'Dex'

    tokens = {
        'root':
          [ (r'//.*\n', token.Comment)
           
          ,  (words(
              ( 'case'
              , 'def'
              , 'default'
              , 'extern'
              , 'for'
              , 'import'
              , 'in'
              , 'init'
              , 'of'     
              , 'return'
              , 'type'
              , 'using'
              ), suffix=r'\b'), token.Keyword)

          , (words( ( '$','->', '=' )), token.Keyword)

          , (words( ('(',')', '{', '}','[',']'
                    , '.', ',', ':'
                    )), token.Punctuation)

          , (r'.', token.Text)
          ]

    }


