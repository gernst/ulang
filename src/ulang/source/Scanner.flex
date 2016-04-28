package ulang.source;

import arse.*;

%%

%class Scanner
%function next
%type arse.Token
%unicode
%line
%column
%{
    private int commentNesting = 0;
    
    public int line()   { return yyline; }
    public int column() { return yycolumn; }
    public int pos()    { return yychar; }
    
    public Token tok(String text) {
		return new Token(text, pos(), text.length());
	}
	
    public Token tok() {
    	return tok(yytext());
    }
	
	public Token semicolon() {
    	return tok(";");
	}
%}

nl = \r|\n|\r\n
sp = [ \t\f]
ws = {nl} | {sp} 

special = "[]" | "()" | "::"
id = [^ \r\n\t\f()\[\]\\.,:;]+
delim = [()\[\]\\.,:;]

%state BLOCK_COMMENT
%state LINE_COMMENT

%%

<BLOCK_COMMENT> {

"/*"        { commentNesting ++; }
"*/"        { commentNesting --; if(commentNesting == 0) yybegin(YYINITIAL); }
{nl}        { /* ignore */ }
.           { /* ignore */ }
}

<LINE_COMMENT> {
{nl}        { yybegin(YYINITIAL); return semicolon(); }
.           { /* ignore */ }
}

<YYINITIAL> {

"//"        { yybegin(LINE_COMMENT);  }
"/*"        { commentNesting = 1; yybegin(BLOCK_COMMENT); }

// {ws}+    { /* ignore */ }

{sp}* {nl} {sp}+
            { /* continue on the next indented line */ }
{ws}* {nl}  { return semicolon();  }
{sp}+       { /* ignore */ }

{delim}     { return tok(); }
{id}        { return tok(); }
{special}   { return tok(); }

// .|\n        { throw new RuntimeException("in scan: unexpected character '" + tok() + "'"); }
}