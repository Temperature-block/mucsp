import re
import sys
import lark
import json
import copy

import secrets
import string


c_includes_type1 = '''
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mqueue.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
'''

def make_uuid(length=12):
    if length < 1:
        raise ValueError("Length must be at least 1")
    letters = string.ascii_letters
    letters_and_digits = string.ascii_letters + string.digits
    first_char = secrets.choice(letters)
    remaining = ''.join(secrets.choice(letters_and_digits) for _ in range(length - 1))
    return first_char + remaining

DEBUG = 1
procuuids = []

namedprocs =  []


def append_unique(name, data_dict):
    # Check if name already exists in any dict
    for entry in global_list:
        if name in entry:
            raise ValueError(f"Duplicate name detected: {name}")
    # Append if unique
    namedprocs.append({name: data_dict})

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
    ("WS",      r"[ \t\n]"),
    ("SCOLON",   r";"),
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
    ("COMMA",   r","),
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
    ("INTEGER", r"[0-9]+"),
]

nlines = 0
def get_token(string):
    #print(string)
    if not string:
       return None
    global nlines
    for tag, pattern in TOKENS:
        match = re.match(r'^' + pattern, string)
        if match:
            if tag == "WS":
               if match.group() == "\n":
                  nlines = nlines+1
               return get_token(string[match.end():])
            return (tag, match,string[match.end():])
    print("unrecognized token at line ",nlines,string[0:])

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

simple_proc :- STOP -
         | SKIP -
         | action -
         | construction
         | instance
         | simple_specification simple_proc

assignment :- variable EQ expr -

input :- channel CHANIN variable -

output :- channel CHANOUT expr -

construction :- seq | cond | loop | par | alt

seq :- SEQ COPEN { process } CCLOSE

cond :- IF COPEN { choice } CCLOSE

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
          | NAME SQOPEN literal SQCLOSE 

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

simple_specification :- declaration

declaration :- type NAME DECLEND 
              | INT NAME EQQ expr

definition :- NAME POPEN { formal, } PCLOSE COPEN process CCLOSE DECLEND

formal :- primitive_type NAME

instance :- NAME POPEN { element, } PCLOSE

monadic_operator :- MINUS 
                  | NOT

dyadic_operator :- PLUS 
                 | MINUS 
                 | MUL 
                 | FS
                 | BS
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

def monadic_operator(toklist,tokpos):
    if(expecttok(toklist,tokpos,"MINUS")):
       return ({"monadic_operator":"MINUS"},tokpos+1)
    elif(expecttok(toklist,tokpos,"NOT")):
       return ({"monadic_operator":"NOT"},tokpos+1)

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
             if(tok := simple_process(toklist,tokpos)):
                tokpos = tok[1]
                if(expecttok(toklist,tokpos,"CCLOSE")):
                   tokpos = tokpos + 1
                   if(expecttok(toklist,tokpos,"DECLEND")):
                      append_unique(nam[0],formallist)
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
                  exprlist,varlist = legalize_exprs(typ[0])
                  return ({"decleration":[nam[0],exprlist,varlist]},typ[1])

def specification(toklist,tokpos):
    if (decl := decleration(toklist,tokpos)):
       return ({"specifiation":decl[0]},decl[1])
    elif (defn := definition(toklist,tokpos)):
       return ({"specifiation":defn[0]},defn[1])

def simple_specification(toklist,tokpos):
    if (decl := qdecleration(toklist,tokpos)):
       return ({"specifiation":decl[0]},decl[1])

def operand(toklist,tokpos):
    if (lit := element(toklist,tokpos)):
       return ({"operand":lit[0]},lit[1])
    elif (lit := literal(toklist,tokpos)):
       return ({"operand":lit[0]},lit[1])
    elif (expecttok(toklist,tokpos,"POPEN")):
       if(exp := expr(toklist,tokpos)):
          tokpos = exp[1]
          if (expecttok(toklist,tokpos,"PCLOSE")):
             return ({"operand":["(",exp[0],")"]},exp[1])

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
                 return ({"expr":[op1[0],dop[0],op2[0]]},op2[1])
           else:
              return ({"expr":op1[0]},op1[1])

def element(toklist,tokpos):
    if(tok := expecttok(toklist,tokpos,"NAME")):
      tokpos = tokpos+1
      if(expecttok(toklist,tokpos,"SQOPEN")):
         tokpos = tokpos+1
         if (lit := literal(toklist,tokpos)):
             tokpos = lit[1]
             if(expecttok(toklist,tokpos,"SQCLOSE")):
               tokpos = tokpos + 1
               return ({"element":[tok,lit[0]]},tokpos+1)
      return ({"element":[tok]},tokpos)

def channel(toklist,tokpos):
    if (lit := element(toklist,tokpos)):
       return ({"channel":lit[0]},lit[1])

def variable(toklist,tokpos):
    if (lit := element(toklist,tokpos)):
       return ({"variable":lit[0]},lit[1])

def literal(toklist,tokpos):
    if(tok := expecttok(toklist,tokpos,"INTEGER")):
      return ({"literal":["INTEGER", int(tok)]}, tokpos+1)

def primitive_type(toklist,tokpos):
    if(expecttok(toklist,tokpos,"INT")):
       return ({"ptype":"INT"},tokpos+1)
    elif(expecttok(toklist,tokpos,"CHAN")):
       return ({"ptype":"CHAN"},tokpos+1)

def type(toklist,tokpos):
    if (ptype := primitive_type(toklist,tokpos)):
        return ({"type":ptype[0]['primitive_type']},ptype[1])
    elif(expecttok(toklist,tokpos,"SQOPEN")):
         tokpos = tokpos+1
         if (lit := literal(toklist,tokpos)):
             tokpos = lit[1]
             if(expecttok(toklist,tokpos,"SQCLOSE")):
               tokpos = tokpos + 1
               if (ptype := primitive_type(toklist,tokpos)):
                   return ({"type":[lit[0],ptype[0]['primitive_type']]},ptype[1]) #type can be encoded directly for now

def par(toklist,tokpos):
    if(expecttok(toklist,tokpos,"PAR")):
       tokpos = tokpos+1
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := simple_process(toklist,tokpos)):
              tokpos = proc[1]
              proclist= [proc[0]]
              while(proc := simple_process(toklist,tokpos)):
                  tokpos = proc[1]
                  proclist.append(proc[0])
              if(expecttok(toklist,tokpos,"CCLOSE")):
                  return({"par":proclist},tokpos+1)

def loop(toklist,tokpos):
    if(expecttok(toklist,tokpos,"WHILE")):
       tokpos = tokpos+1
       if(bul:=expr(toklist,tokpos)):
          tokpos = bul[1]
          exprlist,varlist = legalize_exprs(bul[0])
          if(expecttok(toklist,tokpos,"COPEN")):
             tokpos=tokpos+1
             if (proc := simple_process(toklist,tokpos)):
                 tokpos =  proc[1]
                 if(expecttok(toklist,tokpos,"CCLOSE")):
                    return({"loop":[exprlist,varlist,proc[0]]},tokpos+1)

def choice(toklist,tokpos):
    if(bul:=expr(toklist,tokpos)):
       exprlist,varlist = legalize_exprs(bul[0])
       tokpos = bul[1]
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := simple_process(toklist,tokpos)):
              tokpos =  proc[1]
              if(expecttok(toklist,tokpos,"CCLOSE")):
                  return({"choice":[exprlist,varlist,proc[0]]},tokpos+1)

def cond(toklist,tokpos):
    if(expecttok(toklist,tokpos,"IF")):
       tokpos = tokpos+1
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := choice(toklist,tokpos)):
              tokpos = proc[1]
              proclist = []
              proclist.append(proc[0])
              while(proc := choice(toklist,tokpos)):
                   tokpos = proc[1]
                   proclist.append(proc[0])
              if(expecttok(toklist,tokpos,"CCLOSE")):
                  return({"cond":proclist},tokpos+1)

def seq(toklist,tokpos):
    if(expecttok(toklist,tokpos,"SEQ")):
       tokpos = tokpos+1
       if(expecttok(toklist,tokpos,"COPEN")):
          tokpos = tokpos+1
          if (proc := simple_process(toklist,tokpos)):
              tokpos = proc[1]
              proclist= [proc[0]]
              while(proc := simple_process(toklist,tokpos)):
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

'''
we exclude alt for now
    elif(al := alt(toklist,tokpos)):
       return ({"construction":alt[0]},alt[1])
'''

def output(toklist,tokpos):

    if(chan := channel(toklist,tokpos)):
       tokpos = chan[1]
       if(expecttok(toklist,tokpos,"CHANOUT")):
           tokpos = tokpos + 1
           if(exp := expr(toklist,tokpos)):
               exprlist,varlist = legalize_exprs(bul[0])
               return ({"output":[chan[0],exprlist,varlist]},exp[1])

def input(toklist,tokpos):

    if(chan := channel(toklist,tokpos)):
       tokpos = chan[1]
       if(expecttok(toklist,tokpos,"CHANIN")):
           tokpos = tokpos + 1
           if(var := variable(toklist,tokpos)):
               return ({"input":[chan[0],var[0]]},var[1])

def assignment(toklist,tokpos):

    if(var := variable(toklist,tokpos)):
       tokpos = var[1]
       if(expecttok(toklist,tokpos,"EQ")):
           tokpos = tokpos + 1
           if(exp := expr(toklist,tokpos)):
               exprlist,varlist = legalize_exprs(bul[0])
               return ({"assignment":[var[0],exprlist,varlist]},exp[1])

def action(toklist,tokpos):

    if(assgn := assignment(toklist,tokpos)):
       return ({"action":assgn[0]},assgn[1])

    elif(inp := input(toklist,tokpos)):
       return ({"action":inp[0]},inp[1])

    elif(op := output(toklist,tokpos)):
       return ({"action":op[0]},op[1])

def process(toklist,tokpos):
    global procuuids
    uuid = make_uuid()
    procuuids.append(uuid)
    if(expecttok(toklist,tokpos,"STOP")):
       tokpos = tokpos + 1
       return ({"process":{"uuid":uuid,"STOPPROC":"STOP"}},tokpos)

    elif(expecttok(toklist,tokpos,"SKIP")):
       tokpos = tokpos + 1
       return ({"process":{"uuid":uuid,"SKIPPROC":"SKIP"}},tokpos)

    elif(actionres := action(toklist,tokpos)):
       return ({"process":{"uuid":uuid,"actionproc":actionres[0]}},actionres[1])

    elif(conres := construction(toklist,tokpos)):
       return ({"process":{"uuid":uuid,"constructionproc":conres[0]}},conres[1])

    elif(inres := instance(toklist,tokpos)):
       return ({"process":{"uuid":uuid,"instanceproc":inres[0]}},inres[1])

    elif(spec := specification(toklist,tokpos)):
       tokpos = spec[1]
       if (proc := process(toklist,tokpos)):
          return ({"process":{"uuid":uuid,"specproc":[spec[0],proc[0]]}},proc[1])

def simple_process(toklist,tokpos):
    global procuuids
    uuid = make_uuid()
    procuuids.append(uuid)
    if(expecttok(toklist,tokpos,"STOP")):
       tokpos = tokpos + 1
       return ({"process":{"uuid":uuid,"STOPPROC":"STOP"}},tokpos)

    elif(expecttok(toklist,tokpos,"SKIP")):
       tokpos = tokpos + 1
       return ({"process":{"uuid":uuid,"SKIPPROC":"SKIP"}},tokpos)

    elif(actionres := action(toklist,tokpos)):
       return ({"process":{"uuid":uuid,"actionproc":actionres[0]}},actionres[1])

    elif(conres := construction(toklist,tokpos)):
       return ({"process":{"uuid":uuid,"constructionproc":conres[0]}},conres[1])

    elif(inres := instance(toklist,tokpos)):
       return ({"process":{"uuid":uuid,"instanceproc":inres[0]}},inres[1])

    elif(spec := simple_specification(toklist,tokpos)):
       tokpos = spec[1]
       if (proc := simple_process(toklist,tokpos)):
          return ({"process":{"uuid":uuid,"specproc":[spec[0],proc[0]]}},proc[1])

def handle_parse(toklist):
    return simple_process(toklist,0) #avoid proc for now

dependency_graph = {}
uuid_declarations = {}
visited = set()

def traverse_process_populate_dat(node, parent_uuid = None,parent_declarations=None):
    if node is None:
        return
    uuid = node["process"]["uuid"]
    if uuid not in dependency_graph:
        dependency_graph[uuid] = []
    if parent_uuid is not null:
        dependency_graph[parent_uuid].append(uuid)
    if uuid in visited:
        return
    visited.add(uuid)

    if "actionproc" in proc_data:
        action = proc_data["actionproc"]
        if "input" in action:
            channel_name = action["input"][0]
            uuid_declarations[uuid]["channel_io"][channel_name] = "input"
        if "output" in action:
            channel_name = action["output"][0]
            uuid_declarations[uuid]["channel_io"][channel_name] = "output"

    if "instanceproc" in node["process"]:
        instance = node["process"]["instanceproc"]
        referenced_name = instance[0]
        dependency_graph[uuid].append(referenced_name)

    if "constructionproc" in node["process"]:
        construction = node["process"]["constructionproc"]
        for child in get_child_processes(construction):
            traverse_process(child, uuid)

    if "specproc" in node["process"]:
        spec = node["process"]["specproc"]
        declaration_part = spec[0]
        next_process = spec[1]
        if "decleration" in declaration_part:
            decl_list = declaration_part["decleration"]
            if isinstance(decl_list[0], dict) and "type" in decl_list[0]:
                first_elem = decl_list[0]
                name = decl_list[1]
                uuid_declarations[uuid][name] = first_elem["type"]
            elif isinstance(decl_list[1], list):
                name = decl_list[0]
                expression_part = decl_list[1]
                uuid_declarations[uuid][name] = {"type": "INT", "value": [decl_list[1],decl_list[2]]}
        traverse_process(next_process, uuid)
        del spec[0]


detached_blocks = {}

def detangle_par_blocks(node):
    if node is None:
        return
    if isinstance(node, list):
        for child_node in node:
            detangle_par_blocks(child_node)

    elif "constructionproc" in node["process"] and "par" in node["process"]["constructionproc"]:
        par_list = node["process"]["constructionproc"]["par"]
        new_par_list = []
        for child_proc in par_list:
            child_uuid = child_proc["process"]["uuid"]
            detached_blocks[child_uuid] = child_proc
            new_par_list.append(child_uuid)
            detangle_par_blocks(child_proc)

        node["process"]["constructionproc"]["par"] = new_par_list
    elif "constructionproc" in node["process"]:
        construction = node["process"]["constructionproc"]
        for key in construction:
            detangle_par_blocks(construction[key])
    elif "specproc" in node["process"]:
        next_process = node["process"]["specproc"][1]
        detangle_par_blocks(next_process)

def get_child_processes(construction):
    children = []
    if "seq" in construction:
        children = construction["seq"]
    elif "par" in construction:
        children = construction["par"]
    elif "cond" in construction:
        children = [construction["cond"]["choice"]]
    elif "loop" in construction:
        children = [construction["loop"][1]]
    return children

cops = {
  "MINUS": "-",
  "NOT": "!",
  "PLUS": "+",
  "MUL": "*",
  "FS": "/",
  "BS": "%",
  "EQQ": "==",
  "LT": "<",
  "LE": "<=",
  "GT": ">",
  "GE": ">=",
  "NEQ": "!=",
  "AND": "&&",
  "OR": "||",
}

'''
operand :- element
          | literal
          | POPEN expr PCLOSE

expr :- monadic_operator operand
       | operand dyadic_operator operand
       | operand
'''

def _legalize_exprs(node,exprlist,varuse):
    if 'expr' in node:
        _legalize_exprs(node['expr'], exprlist, varuse)
    elif 'dyadic_operator' in node:
        _legalize_exprs(node[0], exprlist, varuse)
        exprlist.append(cops[node[1]['dyadic_operator']])
        _legalize_exprs(node[2], exprlist, varuse)
    elif 'monadic_operator' in node.get(0, {}):
        exprlist.append(cops[node[0]['monadic_operator']])
        _legalize_exprs(node[1], exprlist, varuse)
    elif 'operand' in node:
        if isinstance(operand_value, list) and len(operand_value) == 3 and operand_value[0] == '(':
            exprlist.append('(')
            _legalize_exprs(node['operand'][1], exprlist, varuse)
            exprlist.append(')')
        else:
            _legalize_exprs(operand_value, exprlist, varuse)
    elif 'literal' in node:
        exprlist.append(node['literal'][1])
    elif 'element' in node:
        element_value = node['element']
        if isinstance(element_value, str):
            exprlist.append(element_value)
            varuse.append(element_value)
        elif isinstance(element_value, list) and len(element_value) == 2:
            exprlist.append(f"{element_value[0]}[")
            varuse.append(element_value[0])
            _legalize_exprs(element_value[1], exprlist, varuse)
            exprlist.append(']')
    return (exprlist, varuse)

def legalize_exprs(node):
    return _legalize_exprs(node,[],[])

def check_channel_usage(declarations):
    global_channel_io = {}

    for uuid, proc_data in declarations.items():
        channel_io = proc_data.get("channel_io", {})
        input_channels = set()
        output_channels = set()
        for name, io_type in channel_io.items():
            if io_type == "input":
                input_channels.add(name)
            elif io_type == "output":
                output_channels.add(name)
        both_io = input_channels.intersection(output_channels)
        if both_io:
            print(f"Error: Channel(s) {both_io} used for both input and output in process '{uuid}'")
        for channel_name, io_type in channel_io.items():
            if channel_name in global_channel_io:
                if global_channel_io[channel_name] == io_type:
                    print(f"Error: Channel '{channel_name}' is used as '{io_type}' in both processes '{global_channel_io[channel_name + '_uuid']}' and '{uuid}'. A channel can only be referred to as one type of I/O.")
            else:
                global_channel_io[channel_name] = io_type
                global_channel_io[channel_name + '_uuid'] = uuid

main_tree = None

partial_legal_list = []
type_mapping = {
    "INT": "int",
    "CHAN": "Channel"
}
#type represent for which machine we are generating c code for
#default linux

def legalize_to_c(node,type=1,parent_node_type = None,proc_uuids):
    uuid = ""
    if "process" in node:
       uuid = node["process"]["uuid"]
       proc_uuids.append(uuid)
       #some stuff to fetch decls using uuid and legalize later
       legalize_to_c(= [k for k in my_dict.keys() if k != "uuid"][0],type,parent_node_type)

    if "STOP" in node:
       if(type == 1):
          partial_legal_list.append("kill(getpid(), SIGSTOP);")
       else:
          partial_legal_list.append("vTaskSuspend(NULL);")

    if "actionproc" in node:
        action = node["actionproc"]["action"]
        if "assignment" in action:
           assgn = action["assignment"]
           partial_legal_list.append(assgn[0]," = ",''.join(str(item) for item in assgn[1])
        elif "input" in action:
           pass #type dependent
        elif "output" in action:
           pass #type dependent

    if "constructionproc" in node:
        construction = node["process"]["constructionproc"]
        if "seq" in construction:
           partial_legal_list.append("{")
           for proc in construction["seq"]:
               legalize_to_c(proc,type,"seq",proc_uuids)
           partial_legal_list.append("}")

        if "par" in construction:
           partial_legal_list.append("{")
           for proc in construction["par"]:
               partial_legal_list.append(
           partial_legal_list.append("}")
           if 'seq' == parent_node_type:
              partial_legal_list.append({"synchronize":construction["par"])

    if "specproc" in node:
       decls = uuid_declarations[uuid]
       proc = node["specproc"]
       for name in decls:
           decl = decls[name]
           typ = decl["type"]

           if isinstance(typ, str):
              c_type = type_mapping.get(typ, typ)
              if "value" in decl:
                 cexprlist = decl["value"][0]
                 expr = ''.join(cexprlist)
                 print(f"{c_type} {name} = {expr};")
              else:
                 partial_legal_list.append(f"{c_type} {name};")

           elif isinstance(typ, list):
                size = typ[0]
                type_name = typ[1]
                c_type = type_mapping.get(type_name, type_name)
                partial_legal_list.append((f"{c_type} {name}[{size}];")

if isinstance(typ, str):
    c_type = type_mapping.get(typ, typ)
    if "value" in decl:
        cexprlist = decl["value"][0]
        expr = ''.join(cexprlist)
        print(f"{c_type} {name} = {expr};")
    else:
        print(f"{c_type} {name};")

elif isinstance(typ, list):
    size = typ[0]
    type_name = typ[1]
    c_type = type_mapping.get(type_name, type_name)
    print(f"{c_type} {name}[{size}];")

def main():
    filestr = ""
    toklist = []
    if(len(sys.argv) > 1):
       with open(sys.argv[1],"r") as file:
             filestr = file.read()
       while filestr:
          tok =  get_token(filestr)
          if tok:
             filestr = tok[2]
             print(filestr)
             toklist.append((tok[0],tok[1]))
          else:
             break

       if DEBUG:
          print(toklist)

       treejson = handle_parse(toklist)
       print(nlines)
       if(treejson):
         print(json.dumps(treejson[0], indent=2))
         traverse_process_populate_dat(treejson[0])
         #collapse_seq_par(treejson[0]) #exclude for now trivial to do
         detangle_par_blocks(treejson[0])
         check_channel_usage(uuid_declarations)
         #check_par_usage
         legalize_to_c(treejson[0])
         #legalize communication etc

       else:
         print("parse error")

if __name__ == "__main__":
    main()

