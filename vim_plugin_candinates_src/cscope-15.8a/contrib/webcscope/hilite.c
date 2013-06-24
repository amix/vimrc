/*
 CopyRight (C) 1999, Dmitry Obukhov, dso@usa.net
 mailto: dso@usa.net
 http://www.EmbeddedStuff.com

 ----------------------------------------------
 Last modified 6 Apr 97
 ----------------------------------------------
 Converts C (C++) source to HTML code fragment
 with syntax highlighting.
 Since program written for personal purpose
 the <TABLE> tags generated. This is optional
 page format specific thing.

 Usage: CTHM <input_file>. All output is done
 to STDOUTPUT, error messages to STDERR.
 For HTML fragment generation:
     CHTM file.c > file.htm

 - Some input convertion required to use this
   code as CGI module. Will be done soon.
 - Optimization required for blocks of EOL
   comments
*/

#include <stdio.h>

// ------------------- Decoding status values

#define START        0
#define INLINE       1
#define DEFINE       2
// ------------------- Decoding Remark
#define REM1        20
#define REM2        21
#define REM_END     22
#define REM_STAR    23
#define REM_STAR_1  24
#define STRING      25 // String is "like" remark


// ------------------- HTML TAG Generation
#define ON  1
#define OFF 0

// ------------------- HTML TAG type
#define MODE_KEYWORD     0
#define MODE_REMARK      2
#define MODE_REMARK_EOL  4
#define MODE_DEFINE      6
#define MODE_STRING      8


int is_delimeter(char c)
{
    int ii=0;
    char dlms[] =
    "\t\r\n (){}[]+-*/%\"'&|^~:;<>.,";
    //--------------------------------
    while (dlms[ii])
    {
        if (c==dlms[ii++]) return 1;
    }
    return 0;
}

int is_keyword(char * str)
{
    char * kwords[] =
    {
        "asm",          "auto",
        "break",        "case",
        "cdecl",        "char",
        "class",        "const",
        "continue",     "default",
        "delete",       "do",
        "double",       "else",
        "enum",         "extern",
        "far",          "float",
        "for",          "friend",
        "goto",         "huge",
        "if",           "inline",
        "int",          "interrupt",
        "long",         "near",
        "new",          "operator",
        "pascal",       "private",
        "protected",    "public",
        "register",     "return",
        "short",        "signed",
        "sizeof",       "static",
        "struct",       "switch",
        "template",     "this",
        "typedef",      "union",
        "unsigned",     "virtual",
        "void",         "volatile",
        "while",        NULL
    };
    int ii=0;
    int jj;
    int check;

    while (kwords[ii])
    {
        jj = 0;
        check = 1;
        while (kwords[ii][jj] && check)
        {
            if (str[jj] != kwords[ii][jj])
            {
               check = 0;
            }
            jj++;
        }
        if (check) return 1;
        ii++;
    }
    return 0;
}


void set_mode(int on_off, int mode)
{
    char * tags[] =
    {
        //-------------------- KEYWORD
        "<strong>",
        "</strong>",
        //-------------------- Classic remarks
        "<font color=\"#336600\">",
        "</font>",
        //-------------------- EOL Remarks
        "<font color=\"#336600\">",
        "</font>",
        //-------------------- #DEFINE
        "<font color=\"#663300\"><strong>",
        "</strong></font>",
        //-------------------- "string"
        "<font color=\"#0000CC\">",
        "</font>",
        NULL,  NULL
    };
    fprintf(stdout,tags[mode + 1 - on_off]);
}

void print_char_html(char c)
{
    switch (c)
    {
        case '<':
             fprintf(stdout,"&lt;");
             break;
        case '>':
             fprintf(stdout,"&gt;");
             break;
        case '"':
             fprintf(stdout,"&quot;");
             break;
        case '&':
             fprintf(stdout,"&amp;");
             break;
        case '|':
             fprintf(stdout,"&brvbar;");
             break;
        default:
             fprintf(stdout,"%c",c);
    }
}



int main(int _argc, char** _argv)
{
   FILE *in, *out;
   char c;
   int  mode;
   char buf[80];
   int  bufidx = 0;
   int  progress = 1;
   int  echo;
   int  saved_mode;
   int  kw;
   char tmpc;
   char prevc;

   if (_argc < 2)
   {
      fprintf(stderr,
      "USAGE: c2html <file>\n");
      return 1;
   }


   if ((in = fopen(_argv[1], "rt")) == NULL)
   {
      fprintf(stderr,
      "Cannot open input file.\n");
      return 1;
   }

	fprintf(stdout, "<pre>");
   mode = START;

   while (!feof(in) && progress)
   {
        echo = 1;
        prevc = c;
        c = fgetc(in);

        if (c=='/' && (mode < REM1))
        {
            saved_mode = mode;
            mode = REM1;
        }

        switch (mode)
        {
            case REM1:
                 echo = 0;
                 mode = REM2;
                 break;

            case REM2:
                 if (c=='/')
                 {
                    if (saved_mode == DEFINE)
                    {
                      set_mode(OFF, MODE_DEFINE);
                    }
                    mode = REM_END;
                    set_mode(ON, MODE_REMARK_EOL);
                 }
                 else if (c=='*')
                 {
                    if (saved_mode == DEFINE)
                    {
                      set_mode(OFF, MODE_DEFINE);
                    }
                    mode = REM_STAR;
                    set_mode(ON, MODE_REMARK);
                 }
                 else
                 {
                    mode = saved_mode;
                 }
                 printf("/");
                 break;

            case REM_END:
                 if (c=='\n')
                 {
                   set_mode(OFF, MODE_REMARK_EOL);
                 }
                 break;

            case REM_STAR:
                 if (c=='*')
                 {
                    mode = REM_STAR_1;
                 }
                 break;

            case REM_STAR_1:
                 if (c=='/')
                 {
                    mode = INLINE;
                    fprintf(stdout,"/");
                    echo = 0;
                    set_mode(OFF, MODE_REMARK);
                 }
                 else mode = REM_STAR;
                 break;

            case START:
                 if (c=='#')
                 {
                    mode = DEFINE;
                    set_mode(ON, MODE_DEFINE);
                    break;
                 }
                 else if (c==' ') break;

                 mode = INLINE;
                 // and continue in next case

            case INLINE:
                 if (c=='"' &&        //
                     prevc != 0x27 && //
                     prevc != '\\')   //
                 {
                    set_mode(ON, MODE_STRING);
                    mode = STRING;
                 }
                 break;

            case STRING:
                 if (c=='"' && prevc != '\\')
                 {
                    print_char_html('"');
                    set_mode(OFF, MODE_STRING);
                    echo = 0;
                    mode = INLINE;
                 }
                 break;

            case DEFINE:
                 if (c=='\n')
                 {
                    set_mode(OFF, MODE_DEFINE);
                 }
                 break;

        }

        if (echo && //
            (mode == INLINE || //
             (mode!=INLINE &&  //
              bufidx)))        //
        {
            buf[bufidx++] = c;
            buf[bufidx]   = 0;
            if (is_delimeter(c))
            {
                kw = 0;
                if (bufidx>2)
                {
                  kw = is_keyword(buf);
                }
                if (kw)
                {
                  set_mode(ON, MODE_KEYWORD);
                }
                tmpc = buf[bufidx-1];
                buf[bufidx-1] = 0;
                fprintf(stdout,"%s",buf);
                if (kw)
                {
                  set_mode(OFF, MODE_KEYWORD);
                }
                print_char_html(tmpc);
                bufidx = 0;
                buf[0] = 0;
            }
        }
        else if (echo) print_char_html(c);

        if (c=='\n' && mode != REM_STAR)
        {
            mode = START;
        }
   }

   fclose(in);
   fprintf(stdout,"</pre>\n");
   fprintf(stdout,
   "<!-- == Generated by CHTM convertor -->\n");
   fprintf(stdout,
   "<!-- == CopyRight (C) 1999, Dmitry Obukhov, dso@usa.net -->\n");

   return 0;
}
