package br.ufma.ecp;

import static br.ufma.ecp.token.TokenType.*;



import br.ufma.ecp.token.Token;
import br.ufma.ecp.token.TokenType; 

public class App 
{

    
    public static void main( String[] args )
    {

    
        String input = """
            // é um comentario 10
            45 \"hello\" variavel + while < , if
            /*
            comentario em bloco
            */
            42 ola
            
            """;
          Scanner scan = new Scanner (input.getBytes());
          for (Token tk = scan.nextToken(); tk.type != TokenType.EOF; tk = scan.nextToken()) {
              System.out.println(tk);
          }
    }
}
