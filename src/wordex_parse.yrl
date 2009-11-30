%%
%%

%% Non-Terminals

Nonterminals 
spadina_file
module_def module_body module_decls module_decl 
type_def
edit_type_def
enum_def enum_def_list
domain_def domain_body domain_decls domain_decl
entity_def entity_body entity_decls entity_decl
populate_decl
domain_edit_apl
norm_def
attr_def pkey_def ukey_def fkey_def
param_list params
param_decl_list param_decls
names literal
expr expr_100 expr_110 expr_150 expr_170 expr_190 expr_200
function
records record 
strings
comp_op
.

Terminals
define apply for of in as values
module type edit enum domain normalizer entity attribute populate
primary unique foreign key
name
variable
string integer float pattern
or and '(' ')' '==' '<>' '<=' '<' '>=' '>' '!' matches
'{' '}' ',' '.'
dot
.

Rootsymbol spadina_file.

spadina_file -> module_def : '$1'.

expr -> expr_100 : {expr, '$1'}.

expr_100 -> expr_110 or expr_100 : {'or', '$1', '$3'}.
expr_100 -> expr_110 : '$1'.

expr_110 -> expr_150 and expr_110 : {'and', '$1', '$3'}.
expr_110 -> expr_150 : '$1'.

expr_150 -> expr_170 comp_op expr_150 : {'$2', '$1', '$3'}.
expr_150 -> expr_170  : '$1'.

expr_170 -> '!' expr_170 : {'nt', '$2'}.
expr_170 -> expr_190  : '$1'.

expr_190 -> expr_200 '.'expr_190 : {deref, '$1', '$3'}.
expr_190 -> expr_200  : '$1'.

expr_200 -> function  : '$1'.
expr_200 -> variable  : '$1'.
expr_200 -> literal  : '$1'.
expr_200 -> name : unwrap('$1').
expr_200 -> '(' expr_100 ')'  : '$2'.

function -> name param_list  : {function, '$1', '$2'}.

comp_op -> '==' : 'eq'.
comp_op -> '<>' : 'ne'.
comp_op -> '<=' : 'le'.
comp_op -> '<'  : 'lt'.
comp_op -> '>=' : 'ge'.
comp_op -> '>'  : 'gt'.
comp_op -> matches  : matches.

literal -> string   : {string, unwrap('$1')}.
literal -> float    : {float, unwrap('$1')}.
literal -> integer  : {integer, unwrap('$1')}.
literal -> pattern  : {pattern, unwrap('$1')}.

module_def -> for expr define module name module_body
                                : {module_def, unwrap('$5'), '$2', '$6'}.
module_def -> define module name module_body  : {module_def, unwrap('$3'), '$4'}.

module_body -> '{' '}'                : [].
module_body -> '{' module_decls '}'   : '$2'.

module_decls -> module_decl                 : ['$1']. 
module_decls -> module_decl module_decls    : ['$1'|'$2'].

module_decl -> type_def                     : '$1'.
module_decl -> edit_type_def                : '$1'.
module_decl -> enum_def                     : '$1'.
module_decl -> domain_def                   : '$1'.
module_decl -> entity_def                   : '$1'.

type_def -> define type name param_decl_list dot : {type_def, unwrap('$3'), '$4'}.

edit_type_def -> define edit type name param_decl_list as expr dot  : {edit_type_def, unwrap('$4'), '$5', '$7'}.

enum_def -> define enum name enum_def_list : {enum_def, unwrap('$3'), '$4'}.
enum_def_list -> '{' names '}'                 : '$2'.

domain_def -> define domain name param_decl_list of type name param_list domain_body
                                                : {domain_def, unwrap('$3'), '$4', {type, unwrap('$7'), '$8'}, '$9'}.

domain_def -> define domain name param_decl_list of domain name param_list domain_body
                                                : {domain_def, unwrap('$3'), '$4', {domain, unwrap('$7'), '$8'}, '$9'}.

domain_body -> dot        : [].
domain_body -> '{' '}'    : [].
domain_body -> '{' domain_decls '}'    : '$2'.

domain_decls -> domain_decl                 : ['$1']. 
domain_decls -> domain_decl domain_decls    : ['$1'|'$2'].

domain_decl -> norm_def : '$1'.
domain_decl -> domain_edit_apl : '$1'.

norm_def -> for expr define normalizer as expr dot
                                               : {norm_def, '$6', '$2'}.
norm_def -> define normalizer as expr dot
                                               : {norm_def, '$4'}.


domain_edit_apl -> for expr apply domain edit name param_list of type name dot
                                               : {domain_edit_def, '$6', '$7', '$10', '$2'}.
domain_edit_apl -> apply domain edit name param_list of type name dot
                                               : {domain_edit_def, '$4', '$5', '$8'}.

entity_def -> define entity name entity_body   : {entity_def, '$3', '$4'}.

entity_body -> dot       : [].
entity_body -> '{' '}'   : [].
entity_body -> '{' entity_decls '}'  : '$2'.

entity_decls -> entity_decl   : ['$1'].
entity_decls -> entity_decl entity_decls   : ['$1'|'$2'].

entity_decl -> attr_def : '$1'.
entity_decl -> pkey_def : '$1'.
entity_decl -> ukey_def : '$1'.
entity_decl -> fkey_def : '$1'.
entity_decl -> populate_decl : '$1'.

attr_def -> define attribute name in domain name param_list dot : {attr_def, '$3', '$6', '$7'}.
pkey_def -> define primary key of names dot : {pkey_def, '$5'}.
ukey_def -> define unique key name of names dot : {ukey_def, '$4', '$6'}.
fkey_def -> define foreign key name of expr dot : {fkey_def, '$4', '$6'}.

populate_decl -> populate '(' names ')' values '{' records '}'  : {populate, '$3', '$7'}.

records -> record   : ['$1'].
records -> record ',' records : ['$1'|'$3'].

record -> '(' strings ')'     : {record, '$2'}.

strings -> string                       : [unwrap('$1')].
strings -> string ',' strings           : [unwrap('$1')|'$3'].

names -> name                           : [unwrap('$1')].
names -> name ',' names                 : [unwrap('$1')|'$3'].

param_decl_list -> '(' ')'                 : [].
param_decl_list -> '(' param_decls ')'     : '$2'.

param_decls -> variable                    : ['$1'].
param_decls -> variable ',' param_decls    : ['$1'|'$3'].


param_list -> '(' ')'                   : [].
param_list -> '(' params ')'            : '$2'.

params -> expr                          : ['$1'].
params -> expr ',' params               : ['$1'|'$3'].

Erlang code.

unwrap({_,_,V}) -> V.
