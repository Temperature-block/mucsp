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

#TODO
#Adjust string on match
#Error out when no match
#pack in tuple and provide a tag

TOKENS = [
    ("WS",      r"[ \t\n]+"),
    ("STOP",    r"STOP"),
    ("SKIP",    r"SKIP"),
    ("IF",      r"IF"),
    ("SEQ",     r"SEQ"),
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
]

def get_token(string):
    for tag, pattern in TOKENS:
        match = re.match(pattern, string)
        if match:
            if tag == "WS":
               return get_token(string[match.end():])
            return (tag, match)

RUL_GRAMMAR = '''
process :- stop | skip | action | construction | instance | specification process

action :- assignment | input | output

assignment :- variable eq expr

input :- channel chanin variable

output :- channel chanout expr

construction :- seq | cond | loop | par | alt

seq :- seqk copen { process } cclose

cond :- if { choice }

choice :- bool copen process close

bool :- expr

loop :- while bool copen process cclose

par :- park copen { process } cclose

alt :- guard copen process cclose

guard :- input | bool & input

type :- primitive.type | sqopen literal sqclose primitive.type

primitive.type = int | chan

literal :- integer

element :- name | name sqopen expr sqclose

variable :- element

channel :- element

operand = element | literal | popen expr pclose

expr :- monadic.operator operand | operand dyadic.operator operand | operand

specification :- decleration | definition

decleration :- type name declend | int name = expr

definition :- procname popen {0,formal  } pclose copen process cclose declend

formal :- primitive.type name

instance :- name popen {0, element} pclose

monadic.operator :- neg | not
'''

filestr = 0
if(sys.argv > 1):
    with open(sya.argv[1],"r") as file:
         filestr = file.read()

