package ulang.source;

%%

%class Scanner
%function next
%type String
%unicode
%line
%column
%{
    private int commentNesting = 0;
    
    public int line()   { return yyline; }
    public int column() { return yycolumn; }
    public int pos()    { return yychar; }
    
    public String tok() { return yytext(); }
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
{nl}        { yybegin(YYINITIAL); return (";"); }
.           { /* ignore */ }
}

<YYINITIAL> {

"//"        { yybegin(LINE_COMMENT);  }
"/*"        { commentNesting = 1; yybegin(BLOCK_COMMENT); }

// {ws}+    { /* ignore */ }

{sp}* {nl} {sp}+ 
            { /* continue on the next indented line */ }
{ws}* {nl}  { return ";";  }
{sp}+       { /* ignore */ }

{delim}     { return tok(); }
{id}        { return tok(); }
{special}   { return tok(); }

// .|\n        { throw new RuntimeException("in scan: unexpected character '" + tok() + "'"); }
}