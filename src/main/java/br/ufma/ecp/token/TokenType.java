package br.ufma.ecp.token;

import java.util.List;
import java.util.Map;

public enum TokenType {
    //Symbols
    LPAREN,RPAREN,
    LBRACE, RBRACE,
    LBRACKET,RBRACKET,
    COMMA, SEMICOLON, DOT,
    PLUS,  MINUS,ASTERISK, SLASH,
    AND, OR, NOT,
    LT, GT, EQ,

     // Literals.
     NUMBER,
     STRING,
     IDENT,

    // keywords
    WHILE, CLASS,CONSTRUCTOR,FUNCTION,
    METHOD,FIELD,STATIC,VAR,INT,
    CHAR,BOOLEAN,VOID,TRUE,FALSE,
    NULL,THIS,LET,DO,IF,ELSE, RETURN,

    EOF,

    ILLEGAL;

     static public boolean isSymbol (String lexeme) {
        String symbols = "{}()[].,;+-*/&|<>=~";
        return symbols.indexOf(lexeme) > -1;
    }


    static public boolean isKeyword (TokenType type) {
        List<TokenType> keywords  = 
            List.of(
                METHOD,
                WHILE,
                IF,
                CLASS,
                CONSTRUCTOR
            );
            return keywords.contains(type);
    }

}
