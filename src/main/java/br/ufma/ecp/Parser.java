package br.ufma.ecp;

import br.ufma.ecp.VMWriter.Command;
import br.ufma.ecp.VMWriter.Segment;
import br.ufma.ecp.token.Token;
import br.ufma.ecp.token.TokenType;
import static br.ufma.ecp.token.TokenType.*;

import br.ufma.ecp.SymbolTable.Kind;
import br.ufma.ecp.SymbolTable.Symbol;

public class Parser {

    private static class ParseError extends RuntimeException {
    }

    private SymbolTable symTable = new SymbolTable();

    private Scanner scan;
    private Token currentToken;
    private Token peekToken;
    private StringBuilder xmlOutput = new StringBuilder();
    private String className;
    private VMWriter vMWriter;
    private int ifLabelNum = 0;
    private int whileLabelNum = 0;

    public Parser(byte[] input) {
        scan = new Scanner(input);
        vMWriter = new VMWriter();
        nextToken();
    }

    public String VMOutput() {
        return vMWriter.vmOutput();
    }

    private void nextToken() {
        currentToken = peekToken;
        peekToken = scan.nextToken();
    }

    void parse() {
        parseClass();
    }

    void parseClass() {
        printNonTerminal("class");
        expectPeek(TokenType.CLASS);
        expectPeek(TokenType.IDENT);
        className = currentToken.lexeme;
        expectPeek(TokenType.LBRACE);

        while (peekTokenIs(TokenType.STATIC) || peekTokenIs(TokenType.FIELD)) {
            parseClassVarDec();
        }

        while (peekTokenIs(TokenType.FUNCTION) || peekTokenIs(TokenType.CONSTRUCTOR) || peekTokenIs(TokenType.METHOD)) {
            parseSubroutineDec();
        }

        expectPeek(TokenType.RBRACE);

        printNonTerminal("/class");
    }

    void parseIf() {
        printNonTerminal("ifStatement");

        var labelTrue = "IF_TRUE" + ifLabelNum;
        var labelFalse = "IF_FALSE" + ifLabelNum;
        var labelEnd = "IF_END" + ifLabelNum;

        ifLabelNum++;
    
        expectPeek(IF);
        expectPeek(LPAREN);
        parseExpression();
        expectPeek(RPAREN);

        vMWriter.writeIf(labelTrue);
        vMWriter.writeGoto(labelFalse);
        vMWriter.writeLabel(labelTrue);
    
        expectPeek(LBRACE);
        parseStatements();
        expectPeek(RBRACE);
        if (peekTokenIs(ELSE)){
            vMWriter.writeGoto(labelEnd);
        }

        vMWriter.writeLabel(labelFalse);

        if (peekTokenIs(ELSE))
        {
            expectPeek(ELSE);
            expectPeek(LBRACE);
            parseStatements();
            expectPeek(RBRACE);
            vMWriter.writeLabel(labelEnd);
        }

        printNonTerminal("/ifStatement");
    }

    void parseWhile() {
        printNonTerminal("whileStatement");

        var labelTrue = "WHILE_EXP" + whileLabelNum;
        var labelFalse = "WHILE_END" + whileLabelNum;
        whileLabelNum++;

        vMWriter.writeLabel(labelTrue);

        expectPeek(WHILE);
        expectPeek(LPAREN);
        parseExpression();

        vMWriter.writeArithmetic(Command.NOT);
        vMWriter.writeIf(labelFalse);

        expectPeek(RPAREN);
        expectPeek(LBRACE);
        parseStatements();

        vMWriter.writeGoto(labelTrue); // Go back to labelTrue and check condition
        vMWriter.writeLabel(labelFalse); // Breaks out of while loop because ~(condition) is true

        expectPeek(RBRACE);
        printNonTerminal("/whileStatement");
    }

    void parseStatements() {
        printNonTerminal("statements");
        while (peekToken.type == TokenType.LET ||
                peekToken.type == TokenType.RETURN ||
                peekToken.type == TokenType.DO ||
                peekToken.type == TokenType.WHILE ||
                peekToken.type == TokenType.IF) {
            parseStatement();
        }

        printNonTerminal("/statements");
    }

    void parseStatement() {
        switch (peekToken.type) {
            case LET:
                parseLet();
                break;
            case RETURN:
                parseReturn();
                break;
            case WHILE:
                parseWhile();
                break;
            case IF:
                parseIf();
                break;
            case DO:
                parseDo();
                break;
            default:
                throw error(peekToken, "Expected a statement");
        }
    }

    void parseDo() {
        printNonTerminal("doStatement");
        expectPeek(TokenType.DO);
        expectPeek(TokenType.IDENT);
        parseSubroutineCall();
        expectPeek(TokenType.SEMICOLON);

        printNonTerminal("/doStatement");
    }

    void parseReturn() {
        printNonTerminal("returnStatement");
        expectPeek(RETURN);
        if (!peekTokenIs(SEMICOLON)) {
            parseExpression();
        } else {
            vMWriter.writePush(Segment.CONST, 0);
        }
        expectPeek(SEMICOLON);
        vMWriter.writeReturn();

        printNonTerminal("/returnStatement");
    }

    public void compileOperators(TokenType type) {

        if (type == ASTERISK) {
            vMWriter.writeCall("Math.multiply", 2);
        } else if (type == SLASH) {
            vMWriter.writeCall("Math.divide", 2);
        } else {
            vMWriter.writeArithmetic(typeOperator(type));
        }
    }

    private Command typeOperator(TokenType type) {
        if (type == PLUS)
            return Command.ADD;
        if (type == MINUS)
            return Command.SUB;
        if (type == LT)
            return Command.LT;
        if (type == GT)
            return Command.GT;
        if (type == EQ)
            return Command.EQ;
        if (type == AND)
            return Command.AND;
        if (type == OR)
            return Command.OR;
        return null;
    }

    void parseTerm() {
        printNonTerminal("term");
        switch (peekToken.type) {
            case NUMBER:
                expectPeek(TokenType.NUMBER);
                vMWriter.writePush(Segment.CONST, Integer.parseInt(currentToken.lexeme));
                break;
            case STRING:
                expectPeek(TokenType.STRING);
                var strValue = currentToken.lexeme;
                vMWriter.writePush(Segment.CONST, strValue.length());
                vMWriter.writeCall("String.new", 1);
                for (int i = 0; i < strValue.length(); i++) {
                    vMWriter.writePush(Segment.CONST, strValue.charAt(i));
                    vMWriter.writeCall("String.appendChar", 2);
                }
                break;
            case FALSE:
            case NULL:
            case TRUE:
                expectPeek(FALSE, NULL, TRUE);
                vMWriter.writePush(Segment.CONST, 0);
                if (currentToken.type == TRUE)
                    vMWriter.writeArithmetic(Command.NOT);
                break;
            case THIS:
                expectPeek(THIS);
                vMWriter.writePush(Segment.POINTER, 0);
                break;
            case IDENT:
                expectPeek(TokenType.IDENT);

                Symbol sym = symTable.resolve(currentToken.lexeme);

                if (peekTokenIs(TokenType.LPAREN) || peekTokenIs(TokenType.DOT)) {
                    parseSubroutineCall();
                } else { // variavel comum ou array
                    if (peekTokenIs(TokenType.LBRACKET)) { // array
                        expectPeek(TokenType.LBRACKET);
                        parseExpression();

                        expectPeek(TokenType.RBRACKET);// push the value of the address pointer back onto stack
                    } else { //variavel simples
                        vMWriter.writePush(kind2Segment(sym.kind()), sym.index());
                    }  
                }
                break;
            case LPAREN:
                expectPeek(TokenType.LPAREN);
                parseExpression();
                expectPeek(TokenType.RPAREN);
                break;
            case MINUS:
            case NOT:
                expectPeek(MINUS, NOT);
                var op = currentToken.type;
                parseTerm();
                if (op == MINUS)
                    vMWriter.writeArithmetic(Command.NEG);
                else
                    vMWriter.writeArithmetic(Command.NOT);

                break;
            default:
                throw error(peekToken, "term expected");
        }

        printNonTerminal("/term");
    }

    static public boolean isOperator(String op) {
        return !op.isEmpty() && "+-*/<>=~&|".contains(op);
    }

    void parseExpression() {
        printNonTerminal("expression");
        parseTerm();
        while (isOperator(peekToken.lexeme)) {
            var ope = peekToken.type;
            expectPeek(peekToken.type);
            parseTerm();
            compileOperators(ope);
        }
        printNonTerminal("/expression");
    }

    int parseExpressionList() {
        printNonTerminal("expressionList");

        var nArgs = 0;

        if (!peekTokenIs(TokenType.RPAREN)) // verifica se tem pelo menos uma expressao
        {
            parseExpression();
            nArgs = 1;
        }

        // procurando as demais
        while (peekTokenIs(TokenType.COMMA)) {
            expectPeek(TokenType.COMMA);
            parseExpression();
            nArgs++;
        }

        printNonTerminal("/expressionList");
        return nArgs;
    }

    void parseLet() {
        
        var isArray = false;

        printNonTerminal("letStatement");
        expectPeek(TokenType.LET);
        expectPeek(TokenType.IDENT);

        var symbol = symTable.resolve(currentToken.lexeme);

        if (peekTokenIs(TokenType.LBRACKET)) {
            expectPeek(TokenType.LBRACKET);
            parseExpression();
            expectPeek(TokenType.RBRACKET);

            isArray = true;
        }

        expectPeek(TokenType.EQ);
        parseExpression();

        if (isArray) {
    

        } else {
            vMWriter.writePop(kind2Segment(symbol.kind()), symbol.index());
        }

        expectPeek(TokenType.SEMICOLON);
        printNonTerminal("/letStatement");
    }

    void parseSubroutineCall() {

        var nArgs = 0;

        if (peekTokenIs(TokenType.LPAREN)) { // método da propria classe
            expectPeek(TokenType.LPAREN);

            nArgs = parseExpressionList() + 1;
            expectPeek(TokenType.RPAREN);

        } else {
            // pode ser um metodo de um outro objeto ou uma função
            expectPeek(TokenType.DOT);
            expectPeek(TokenType.IDENT); // nome da função

            expectPeek(TokenType.LPAREN);
            nArgs += parseExpressionList();

            expectPeek(TokenType.RPAREN);
        }

    }

    void parseClassVarDec() {
        printNonTerminal("classVarDec");
        expectPeek(FIELD, STATIC);

        SymbolTable.Kind kind = Kind.STATIC;
        if (currentTokenIs(FIELD))
            kind = Kind.FIELD;

        // 'int' | 'char' | 'boolean' | className
        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        String type = currentToken.lexeme;

        expectPeek(IDENT);
        String name = currentToken.lexeme;

        symTable.define(name, type, kind);
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);

            name = currentToken.lexeme;
            symTable.define(name, type, kind);
        }

        expectPeek(SEMICOLON);
        printNonTerminal("/classVarDec");
    }

    void parseSubroutineDec() {
        printNonTerminal("subroutineDec");

        ifLabelNum = 0;
        whileLabelNum = 0;

        symTable.startSubroutine();

        expectPeek(TokenType.CONSTRUCTOR, TokenType.FUNCTION, TokenType.METHOD);
        var subroutineType = currentToken.type;

        if (subroutineType == METHOD) {
            symTable.define("this", className, Kind.ARG);
        };

        // 'int' | 'char' | 'boolean' | className
        expectPeek(TokenType.VOID, TokenType.INT, TokenType.CHAR, TokenType.BOOLEAN, TokenType.IDENT);
        expectPeek(TokenType.IDENT);

        var functionName = className + "." + currentToken.lexeme;

        expectPeek(TokenType.LPAREN);
        parseParameterList();
        expectPeek(TokenType.RPAREN);
        parseSubroutineBody(functionName, subroutineType);

        printNonTerminal("/subroutineDec");
    }

    void parseParameterList() {
        printNonTerminal("parameterList");

        SymbolTable.Kind kind = Kind.ARG;

        if (!peekTokenIs(RPAREN)) // verifica se tem pelo menos uma expressao
        {
            expectPeek(INT, CHAR, BOOLEAN, IDENT);
            String type = currentToken.lexeme;

            expectPeek(IDENT);
            String name = currentToken.lexeme;
            symTable.define(name, type, kind);

            while (peekTokenIs(COMMA)) {
                expectPeek(COMMA);
                expectPeek(INT, CHAR, BOOLEAN, IDENT);
                type = currentToken.lexeme;

                expectPeek(IDENT);
                name = currentToken.lexeme;

                symTable.define(name, type, kind);
            }

        }

        printNonTerminal("/parameterList");
    }

    void parseSubroutineBody(String functionName, TokenType subroutineType) {

        printNonTerminal("subroutineBody");
        expectPeek(TokenType.LBRACE);
        while (peekTokenIs(TokenType.VAR)) {
            parseVarDec();
        }

        var nlocals = symTable.varCount(Kind.VAR);

        vMWriter.writeFunction(functionName, nlocals);

        parseStatements();
        expectPeek(TokenType.RBRACE);
        printNonTerminal("/subroutineBody");
    }

    void parseVarDec() {
        printNonTerminal("varDec");
        expectPeek(VAR);

        SymbolTable.Kind kind = Kind.VAR;

        // 'int' | 'char' | 'boolean' | className
        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        String type = currentToken.lexeme;

        expectPeek(IDENT);
        String name = currentToken.lexeme;
        symTable.define(name, type, kind);

        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);

            name = currentToken.lexeme;
            symTable.define(name, type, kind);

        }

        expectPeek(SEMICOLON);
        printNonTerminal("/varDec");
    }

    // funções auxiliares
    public String XMLOutput() {
        return xmlOutput.toString();
    }

    private void printNonTerminal(String nterminal) {
        xmlOutput.append(String.format("<%s>\r\n", nterminal));
    }

    boolean peekTokenIs(TokenType type) {
        return peekToken.type == type;
    }

    boolean currentTokenIs(TokenType type) {
        return currentToken.type == type;
    }

    private void expectPeek(TokenType... types) {
        for (TokenType type : types) {
            if (peekToken.type == type) {
                expectPeek(type);
                return;
            }
        }

        throw error(peekToken, "Expected a statement");

    }

    private void expectPeek(TokenType type) {
        if (peekToken.type == type) {
            nextToken();
            xmlOutput.append(String.format("%s\r\n", currentToken.toString()));
        } else {
            throw error(peekToken, "Expected " + type.name());
        }
    }

    private static void report(int line, String where,
            String message) {
        System.err.println(
                "[line " + line + "] Error" + where + ": " + message);
    }

    private ParseError error(Token token, String message) {
        if (token.type == TokenType.EOF) {
            report(token.line, " at end", message);
        } else {
            report(token.line, " at '" + token.lexeme + "'", message);
        }
        return new ParseError();
    }

    private Segment kind2Segment(Kind kind) {
        if (kind == Kind.STATIC)
            return Segment.STATIC;
        if (kind == Kind.FIELD)
            return Segment.THIS;
        if (kind == Kind.VAR)
            return Segment.LOCAL;
        if (kind == Kind.ARG)
            return Segment.ARG;
        return null;
    }

}