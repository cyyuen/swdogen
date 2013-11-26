{
  open Swgparser
  
  open Lexing
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = (alpha|'_')((alpha|digit|'_')* )

(* ordinary string character (without: single/double quotes, backslash) *)
let url_char = alpha|digit|['-' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '_' '=' '+' '[' ']' '{' '}' '|' ';' ':' '<' '>' ',' '.' '/' '?' '~' '`']
let string_char = url_char|' '

let urlscheme = 
  ( "http://" 
  | "https://" 
  | "ftp://" 
  | '/')

let url = urlscheme url_char+
let blanks = [' ' '\t']+

let minescheme = 
  ( "application/" 
  | "audio/" 
  | "image/" 
  | "message" 
  | "model/" 
  | "multiplepart/" 
  | "text/" 
  | "video/")

let mine = minescheme url_char+


(* the swg token is defined within the comments *)
rule token = parse
  "/**"            { swg_entry lexbuf  }
  | "\n"           { new_line lexbuf;
                     token lexbuf      }
  | eof            { EOF               }
  | _              { token lexbuf      }

and swg_entry = parse
  | "@resource"    { T_AT_RESOURCE     }
  | "@operation"   { T_AT_OPERATION    }
  | "@desc"        { T_AT_DESC         }
  | "@api"         { T_AT_API          }
  | "@method"      { T_AT_METHOD       }
  | "@summary"     { T_AT_SUMMARY      }
  | "@return"      { T_AT_RETURN       }
  | "@response"    { T_AT_RESPONSE     }
  | "@notes"       { T_AT_NOTES        }
  | "@param"       { T_AT_PARAM        }
  | "@model"       { T_AT_MODEL        }
  | "@property"    { T_AT_PROPERTY     }
  | "@basePath"    { T_AT_BASEPATH     }
  | "@produces"    { T_AT_PRODUCES     } 
  | "@consumes"    { T_AT_CONSUMES     }
  | "@auth/apiKey" { T_AT_AUTH_APIKEY  }
  | "*/"           { token lexbuf;     }
  | "\n"           { new_line lexbuf; 
                     swg_entry lexbuf  }
  | eof            { EOF               }
  | _              { swg_entry lexbuf  }

and swg_body = parse
  | "\n"           { new_line lexbuf;
                     swg_entry lexbuf }

  (* parameter type *)
  | "#path"        { T_PARAM_PATH   }
  | "#body"        { T_PARAM_BODY   }
  | "#query"       { T_PARAM_QUERY  }
  | "#header"      { T_PARAM_HEADER }
  | "#form"        { T_PARAM_FORM   }

  (* http method *)
  | "GET"          { T_METHOD_GET    }
  | "POST"         { T_METHOD_POST   }
  | "PUT"          { T_METHOD_PUT    }
  | "DELET"        { T_METHOD_DELETE }
  | "HEAD"         { T_METHOD_HEAD   }

  (* predefined type *)
  | "int"          { T_INT      } 
  | "long"         { T_LONG     }
  | "float"        { T_FLOAT    }
  | "double"       { T_DOUBLE   }
  | "string"       { T_STRING   }
  | "byte"         { T_BYTE     }
  | "boolean" 
  | "bool"         { T_BOOLEAN  }
  | "date"         { T_DATE     }
  | "datetime"     { T_DATETIME }
  | "list"
  | "array"        { T_ARRAY    }
  | "set"          { T_SET      }
  | "option"       { T_OPTION   }
  | "void"         { T_VOID     }

  (* punctuation *)
  | "("            { T_LPAREN }
  | ")"            { T_RPAREN }
  | "["            { T_LBRACE }
  | "]"            { T_RBRACE }
  | ","            { T_COMMA  }
  | "-"            { T_MINUS  }
  | "|"            { T_VBAR   }
  | ":"            { T_COLON  }
  | "?"            { T_QMARK  }
  | "="            { T_ASG    }

  (* other words *)
  | blanks { swg_body lexbuf }
  | digit+ as lxm  
           { T_INT_LITERAL(int_of_string lxm) }         
  | (digit+ '.' digit+) as lxm
           { T_FLOAT_LITERAL(float_of_string lxm) }
  | identifier as lxm
           { T_IDENTIFIER(lxm) }
  | '\"' (('\'' | "\\\"" | "\\\\" | string_char)* as lxm) '\"'
           { T_STRING_LITERAL(lxm) }
  | url as lxm     
           { T_URL (lxm) }
  | mine as lxm
           { T_MIME (lxm) }
  | eof    { EOF          }
  | _      { swg_body lexbuf }

{
  let token lexbuf =
    if lexbuf.lex_curr_pos = 0 then
      token lexbuf
    else
      swg_body lexbuf
}