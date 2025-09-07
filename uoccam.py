import re
import sys
import lark

LEX_GRAMMAR = '''
stop :- STOP
skip :- SKIP
if :- IF
seqk :- SEQ
park :- PAR
int :- INT
chan :- CHAN
proc :- PROC
not :- NOT
name :- [a-zA-Z_][a-zA-Z0-9_]*
eq :- :=
chanin :- ?
chanout :- !
copen :- {
cclose :- }
sqopen :- [
sqclose :- ]
popen :- (
pclose :- )
declend :- :
eqq :- =
neg :-  -
plus :- +
minus :- -
mul :- *
fs :- /
bs :- \
eqc :- =
lt :- <
le:- <=
gt :- >
ge :- >=
neq :- <>
and :- &
or :- |
'''


TOKENS = [
    ("WS",      r"[ \t\n]+"),
    ("STOP",    r"STOP"),
    ("SKIP",    r"SKIP"),
    ("IF",      r"IF"),
    ("SEQ",     r"SEQ"),
    ("WHILE", r"WHILE"),
    ("PAR",     r"PAR"),
    ("INT",     r"INT"),
    ("CHAN",    r"CHAN"),
    ("PROC",    r"PROC"),
    ("NOT",     r"NOT"),
    ("LE",      r"<="),
    ("GE",      r">="),
    ("NEQ",     r"<>"),
    ("EQ",      r":="),
    ("LT",      r"<"),
    ("GT",      r">"),
    ("EQQ",     r"="),
    ("DECLEND", r":"),
    ("PLUS",    r"\+"),
    ("MINUS",   r"-"),
    ("MUL",     r"\*"),
    ("FS",      r"/"),
    ("BS",      r"\\"),
    ("AND",     r"&"),
    ("OR",      r"\|"),
    ("CHANIN",  r"\?"),
    ("CHANOUT", r"!"),
    ("COPEN",   r"\{"),
    ("CCLOSE",  r"\}"),
    ("SQOPEN",  r"\["),
    ("SQCLOSE", r"\]"),
    ("POPEN",   r"\("),
    ("PCLOSE",  r"\)"),
    ("NAME",    r"[a-zA-Z_][a-zA-Z0-9_]*"),
    ("INTEGER", r"[0-9]*")
]

def get_token(string):
    for tag, pattern in TOKENS:
        match = re.match(pattern, string)
        if match:
            if tag == "WS":
               return get_token(string[match.end():])
            return (tag, match)

RUL_GRAMMAR = '''
process :- STOP -
         | SKIP -
         | action -
         | construction 
         | instance 
         | specification process

action :- assignment -
        | input -
        | output -

assignment :- variable EQ expr -

input :- channel CHANIN variable -

output :- channel CHANOUT expr -

construction :- seq | cond | loop | par | alt

seq :- SEQ COPEN { process } CCLOSE

cond :- IF COPEN choice CCLOSE

choice :- bool COPEN process CCLOSE

bool :- expr

loop :- WHILE bool COPEN process CCLOSE      # Note: WHILE token not defined, add if needed

par :- PAR COPEN { process } CCLOSE

alt :- guard COPEN process CCLOSE

guard :- input 
        | bool AND input

type :- primitive_type 
      | SQOPEN literal SQCLOSE primitive_type

primitive_type :- INT 
                | CHAN

literal :- integer      # Define a token INTEGER as needed

element :- NAME 
          | NAME SQOPEN expr SQCLOSE

variable :- element

channel :- element

operand :- element 
          | literal 
          | POPEN expr PCLOSE

expr :- monadic_operator operand 
       | operand dyadic_operator operand 
       | operand

specification :- declaration 
                | definition

declaration :- type NAME DECLEND 
              | INT NAME EQQ expr

definition :- NAME POPEN { formal } PCLOSE COPEN process CCLOSE DECLEND

formal :- primitive_type NAME

instance :- NAME POPEN { element } PCLOSE

monadic_operator :- MINUS 
                  | NOT

dyadic_operator :- PLUS 
                 | MINUS 
                 | MUL 
                 | FS
                 | BS
                 | EQ
                 | EQQ
                 | LT
                 | LE
                 | GT
                 | GE
                 | NEQ
                 | AND
                 | OR

'''

def expecttok(toklist,tokpos,expect):
    if(toklist[tokpos][0] == expect):
       return toklist[tokpos][1].group()
    else:
       return None

'''
alt :- guard COPEN process CCLOSE

============================================
specification :- declaration
                | definition

declaration :- type NAME DECLEND
              | INT NAME EQQ expr

definition :- NAME POPEN { formal } PCLOSE COPEN process CCLOSE DECLEND

formal :- primitive_type NAME

instance :- NAME POPEN { element } PCLOSE

monadic_operator :- MINUS
                  | NOT

dyadic_operator :- PLUS
                 | MINUS
                 | MUL
                 | FS
                 | BS
                 | EQ
                 | EQQ
                 | LT
                 | LE
                 | GT
                 | GE
                 | NEQ
                 | AND
                 | OR
'''

def monadic_operator(toklist,tokpos):
    if(expecttok(toklist,tokpos,"MINUS")):
       return ({"monadic_operator":"MINUS"},tokpos+1)
    elif(expecttok(toklist,tokpos,"MINUS")):
       return ({"monadic_operator":"MINUS"},tokpos+1)

def dyadic_operator(toklist,tokpos):
    if(expecttok(toklist,tokpos,"PLUS")):
       return ({"dyadic_operator":"PLUS"},tokpos+1)
    elif(expecttok(toklist,tokpos,"MINUS")):
       return ({"dyadic_operator":"MINUS"},tokpos+1)
    elif(expecttok(toklist,tokpos,"MUL")):
       return ({"dyadic_operator":"MUL"},tokpos+1)
    elif(expecttok(toklist,tokpos,"FS")):
       return ({"dyadic_operator":"FS"},tokpos+1)
    elif(expecttok(toklist,tokpos,"BS")):
       return ({"dyadic_operator":"BS"},tokpos+1)
    elif(expecttok(toklist,tokpos,"EQ")):
       return ({"dyadic_operator":"EQ"},tokpos+1)
    elif(expecttok(toklist,tokpos,"EQQ")):
       return ({"dyadic_operator":"EQQ"},tokpos+1)
    elif(expecttok(toklist,tokpos,"LE")):
       return ({"dyadic_operator":"LE"},tokpos+1)
    elif(expecttok(toklist,tokpos,"GE")):
       return ({"dyadic_operator":"GE"},tokpos+1)
    elif(expecttok(toklist,tokpos,"NEQ")):
       return ({"dyadic_operator":"NEQ"},tokpos+1)
    elif(expecttok(toklist,tokpos,"AND")):
       return ({"dyadic_operator":"AND"},tokpos+1)
    elif(expecttok(toklist,tokpos,"OR")):
       return ({"dyadic_operator":"OR"},tokpos+1)

def instance(toklist,tokpos):
    if(nam := expecttok(toklist,tokpos,"NAME")):
       tokpos = tokpos + 1
       elemlist = []
       if(expecttok(toklist,tokpos,"POPEN")):
          while(elem := element(toklist,tokpos)):
              tokpos = elem[1]
              elemlist.append(elem[0])
          if(expecttok(toklist,tokpos,"PCLOSE")):
             return ({"instance":[nam[0],elemlist]},tokpos+1)

def formal(toklist,tokpos):
    if (ptype := primitive_type(toklist,tokpos)):
       tokpos = ptype[1]
       if(nam := expecttok(toklist,tokpos,"NAME")):
          return ({"formal":[ptype[0],nam[0]]},tokpos+1)

def definition(toklist,tokpos):
    if(nam := expecttok(toklist,tokpos,"NAME")):
      tokpos = tokpos + 1
      if(expecttok(toklist,tokpos,"POPEN")):
        tokpos = tokpos + 1
        formallist = []
        while(form := formal(toklist,tokpos)):
             tokpos = form[1]
             formallist.append(form[0])
        if(expecttok(toklist,tokpos,"PCLOSE")):
           tokpos = tokpos + 1
           if(expecttok(toklist,tokpos,"COPEN")):
             tokpos = tokpos + 1
             if(tok := process(toklist,tokpos)):
                tokpos = tok[1]
                if(expecttok(toklist,tokpos,"CCLOSE")):
                   tokpos = tokpos + 1
                   if(expecttok(toklist,tokpos,"DECLEND")):
                      return ({"definition":[nam[0],formallist,tok[0]]},tokpos+1)

def decleration(toklist,tokpos):
    if(typ := type(toklist,tokpos)):
       tokpos = typ[1]
       if(nam := expecttok(toklist,tokpos,"NAME")):
          tokpos = tokpos + 1
          if(expecttok(toklist,tokpos,"DECLEND")):
             return ({"decleration":[typ[0],nam[0]]},tokpos+1)
    elif(expecttok(toklist,tokpos,"INT")):
         tokpos = tokpos + 1
         if(nam := expecttok(toklist,tokpos,"NAME")):
            tokpos = tokpos + 1
            if(expecttok(toklist,tokpos,"EQQ")):
               tokpos = tokpos + 1
               if(typ := expr(toklist,tokpos)):
                  return ({"decleration":[nam[0],expr[0]]},typ[1])

def specification(toklist,tokpos):
    if (decl := decleration(toklist,tokpos)):
       return ({"specifiation":decl[0]},decl[1])
    elif (defn := definition(toklist,tokpos)):
       return ({"specifiation":defn[0]},defn[1])

def operand(toklist,tokpos):
    if (lit := element(toklist,tokpos)):
       return ({"operand":lit[0]},lit[1])
    elif (lit := literal(toklist,tokpos)):
       return ({"operand":lit[0]},lit[1])
    elif (expecttok(toklist,tokpos,"POPEN")):
       if(exp := expr(toklist,tokpos)):
          tokpos = exp[1]
          if (expecttok(toklist,tokpos,"PCLOSE")):
             return ({"operand":expr[0]},expr[1])

def expr(toklist,tokpos):
    if (mon := monadic_operator(toklist,tokpos)):
        tokpos = mon[1]
        if (op := operand(toklist,tokpos)):
           return ({"expr":[mon[0],op[0]]},op[1])
    elif (op1 := operand(toklist,tokpos)):
           tokpos = op1[1]
           if(dop := dyadic_operator(toklist,tokpos)):
              tokpos = dop[1]
              if (op2 := operand(toklist,tokpos)):
                 return ({"expr":[op[0],dop[0],op2[0]]},op2[1])
           else:
              return ({"expr":op1[1]},op1[1])

def element(toklist,tokpos):
    if(tok = expecttok(toklist,tokpos,"NAME")):
      return ({"element":tok},tokpos+1)
    elif(tok = expecttok(toklist,tokpos,"NAME")):
      tokpos = tokpos+1
      if(expecttok(toklist,tokpos,"SQOPEN")):
         tokpos = tokpos+1
         if (lit := literal(toklist,tokpos)):
             tokpos = lit[1]
             if(expecttok(toklist,tokpos,"SQCLOSE")):
               tokpos = tokpos + 1
               return ({"element":[tok,lit[0]]},tokpos+1)

def channel(toklist,tokpos):
    if (lit := element(toklist,tokpos)):
       return ({"channel":lit[0]},lit[1])

def variable(toklist,tokpos):
    if (lit := element(toklist,tokpos)):
       return ({"variable":lit[0]},lit[1])

def literal(toklist,tokpos):
    if(tok = expecttok(toklist,tokpos,"INTEGER")):
      return ({"literal":["INTEGER",tok]},tokpos+1)

def primitive_type(toklist,tokpos):
    if(expecttok(toklist,tokpos,"INT")):
       return ({"ptype":"INT"},tokpos+1)
    elif(expecttok(toklist,tokpos,"CHAN")):
       return ({"ptype":"CHAN"},tokpos+1)

def type(toklist,tokpos):
    if (ptype := primitive_type(toklist,tokpos)):
        return ({"type":ptype[0]},ptype[1])
    elif(expecttok(toklist,tokpos,"SQOPEN")):
         tokpos = tokpos+1
         if (lit := literal(toklist,tokpos)):
             tokpos = lit[1]
             if(expecttok(toklist,tokpos,"SQCLOSE")):
               tokpos = tokpos + 1
               if (ptype := primitive_type(toklist,tokpos)):
                   return ({"type":[lit[0],ptype[0]]},ptype[1])

def par(toklist,tokpos):
    if(expecttok(toklist,tokpos,"PAR")):
       tokpos = tokpos+1
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := process(toklist,tokpos)):
              tokpos = proc[1]
              proclist= [proc[0]]
              while(proc := process(toklist,tokpos)):
                  tokpos = proc[1]
                  proclist.append(proc[0])
              if(expecttok(toklist,tokpos,"CCLOSE")):
                  return({"par":proclist},tokpos+1)

def loop(toklist,tokpos):
    if(expecttok(toklist,tokpos,"WHILE")):
       tokpos = tokpos+1
       if(bul:=expr(toklist,tokpos)):
          tokpos = bul[1]
          if(expecttok(toklist,tokpos,"COPEN")):
             tokpos=tokpos+1
             if (proc := process(toklist,tokpos)):
                 tokpos =  proc[1]
                 if(expecttok(toklist,tokpos,"CCLOSE")):
                    return({"loop":[bul[0],proc[0]]},tokpos+1)

def choice(toklist,tokpos):
    if(bul:=expr(toklist,tokpos)):
       tokpos = bul[1]
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := process(toklist,tokpos)):
              tokpos =  proc[1]
              if(expecttok(toklist,tokpos,"CCLOSE")):
                  return({"choice":[bul[0],proc[0]]},tokpos+1)

def cond(toklist,tokpos):
    if(expecttok(toklist,tokpos,"IF")):
       tokpos = tokpos+1
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := cond(toklist,tokpos)):
              tokpos = proc[1]
              if(expecttok(toklist,tokpos,"CCLOSE")):
                  return({"cond":proc[0]},tokpos+1)

def seq(toklist,tokpos):
    if(expecttok(toklist,tokpos,"SEQ")):
       tokpos = tokpos+1
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := process(toklist,tokpos)):
              tokpos = proc[1]
              proclist= [proc[0]]
              while(proc := process(toklist,tokpos)):
                  tokpos = proc[1]
                  proclist.append(proc[0])
              if(expecttok(toklist,tokpos,"CCLOSE")):
                  return({"seq":proclist},tokpos+1)

def construction(toklist,tokpos):

    if(se := seq(toklist,tokpos)):
       return ({"construction":se[0]},se[1])

    elif(con := cond(toklist,tokpos)):
       return ({"construction":con[0]},con[1])

    elif(loo := loop(toklist,tokpos)):
       return ({"construction":loo[0]},loo[1])

    elif(pa := par(toklist,tokpos)):
       return ({"construction":pa[0]},pa[1])

    elif(al := alt(toklist,tokpos)):
       return ({"construction":alt[0]},alt[1])

def output(toklist,tokpos):

    if(chan := channel(toklist,tokpos)):
       tokpos = chan[1]
       if(expecttok(toklist,tokpos,"CHANOUT")):
           tokpos = tokpos + 1
           if(exp := expr(toklist,tokpos)):
               return ({"output":[chan[0],exp[0]]},exp[1])

def input(toklist,tokpos):

    if(chan := channel(toklist,tokpos)):
       tokpos = chan[1]
       if(expecttok(toklist,tokpos,"CHANIN")):
           tokpos = tokpos + 1
           if(var := variable(toklist,tokpos)):
               return ({"input":[chan[0],[var[0]]},var[1])

def assignment(toklist,tokpos):

    if(var := varable(toklist,tokpos)):
       tokpos = var[1]
       if(expecttok(toklist,tokpos,"EQ")):
           tokpos = tokpos + 1
           if(exp := expr(toklist,tokpos)):
               return ({"assignment":[var[0],exp[0]]},exp[1])

def action(toklist,tokpos):

    if(assgn := assignment(toklist,tokpos)):
       return ({"action":assgn[0]},assgn[1])

    elif(inp := input(toklist,tokpos)):
       return ({"action":inp[0]},inp[1])

    elif(op := output(toklist,tokpos)):
       return ({"action":op[0]},op[1])

def process(toklist,tokpos):

    if(expecttok(toklist,tokpos,"STOP")):
       tokpos = tokpos + 1
       return ({"process":"STOP"},tokpos)

    elif(expecttok(toklist,tokpos,"SKIP")):
       tokpos = tokpos + 1
       return ({"process":"SKIP"},tokpos)

    elif(actionres := action(toklist,tokpos)):
       return ({"process":actionres[0]},actionres[1])

    elif(conres := construction(toklist,tokpos)):
       return ({"process":conres[0]},conres[1])

    elif(inres := instance(toklist,tokpos)):
       return ({"process":inres[0]},inres[1])

    elif(spec := specification(toklist,tokpos)):
       tokpos = spec[1]
       if (proc := process(toklist,tokpos)):
          return ({"process":[spec[0],proc[0]]},proc[1])


def handle_parse(toklist):
    return process(toklist,0)


def main():
    filestr = 0
    toklist = []
    if(sys.argv > 1):
       with open(sys.argv[1],"r") as file:
             filestr = file.read()
       while filestr:
          tok = get_token(filestr)
          if tok:
             filestr = filestr[tok[1].end():]
             toklist.append(tok)
       treejson = handle_parse(toklist)

if __name__ == "__main__":
    main()

