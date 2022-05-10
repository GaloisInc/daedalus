from pygments.lexer import RegexLexer
from pygments.lexer import words
from pygments import token
from sphinx.highlighting import lexers

class DaeDaLusLexer(RegexLexer):
    name = 'DaeDaLus'

    tokens = {
        'root':
          [ (r'--.*\n', token.Comment)
          , (r'{-', token.Comment, 'comment')

          , (r"'", token.String, 'char')
          , (r'"', token.String, 'string')

          ,  (words(('import', 'bitdata', 'where', 'def'
                 , 'for', 'map', 'many', 'many?', 'in', 'if', 'then', 'else'
                 , 'is', 'of', 'as', 'as!', 'as?'
                 , 'First', 'Choose', 'case', 'block'
                 , 'let'
                 , 'commit'

                 , 'Accept'
                 , 'Optional', 'Optional?'
                 , 'Many', 'Many?'
                 , 'UInt8'
                 , 'Match'
                 , 'END'

                 , 'insert', 'Insert', 'lookup', 'Lookup'
                 , 'Index', 'concat', 'length', 'rangeUp', 'rangeDown'
                 , 'GetStream', 'SetStream', 'Offset'
                 , 'Take', 'Drop', 'arrayStream', 'bytesOfStream'

                 , 'wordToFloat'
                 , 'wordToDouble'
                 , 'isNaN'
                 , 'isInfinite'
                 , 'isDenormalized'
                 , 'isNegativeZero'

                 , 'emit', 'emitArray', 'emitBuilder'

                 , 'int', 'uint', 'sint', 'bool', 'maybe', 'float', 'double'
                 , 'stream', 'builder'

                 ), suffix=r'\b'), token.Keyword)

          , (words( ( '$$', '^', '|', '<|', '@'
                    , '->', '='
                    )), token.Keyword)

          , (words( ( 'false', 'true'
                    , 'nothing'
                    , 'just'
                    , 'empty'
                    , 'pi'
                    ), suffix=r'\b'), token.Literal )

          , (r'0[xX][0-9a-fA-f]+', token.Number)
          , (r'0[bB][0-1]+', token.Number)
          , (r'[0-9]+', token.Number)

          , (r'[\$?A-Za-z][A-Za-z0-9]*', token.Name)


          , (words( ('(',')', '{', '}', '{|','|}','[',']'
                    , '.', ',', '..', ';', ':', '->'
                    )), token.Punctuation)

          , (words( ( '!', '==', '!=', '<=', '>=', '<', '>'
                    , '.|.', '.&.', '.^.'
                    , '||', '&&', '+', '-', '*', '/', '%'
                    , '#', '<#', '<<', '>>'
                    )), token.Operator)

          , (r'.', token.Text)
          ]

        , 'comment':
          [ (r'{-', token.Comment, '#push')
          , (r'-}', token.Comment, '#pop')
          , (r'.', token.Comment)
          ]

        , 'string':
          [ (r'\\"', token.String)
          , (r'"',   token.String, '#pop')
          , (r'.',   token.String)
          ]

        , 'char':
          [ (r"\\'", token.String)
          , (r"'",   token.String, '#pop')
          , (r'.',   token.String)
          ]

    }


