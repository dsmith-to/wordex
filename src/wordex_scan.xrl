%%% File    : wordex_scan.xrl
%%% Author  : David Smith
%%% Purpose : Token definitions for Word Index gr

Definitions.
OD	= [0-7]
DD	= [0-9]
HD	= [0-9a-fA-F]
ACU	= [A-Z]
ACL	= [a-z]
AC	= ({ACU}|{ACL}|{DD}|_)
WS	= [\000-\s]

COMMENT = \(\*\(*([^*)]|[^*]\)|\*[^)])*\**\*\)
DQUOTED = "(\\\^.|\\.|[^"])*"
SQUOTED = '(\\\^.|\\.|[^'])*'
QUOTED  = (SQUOTED|DQUOTED)

Rules.
{DD}+\.{DD}+((E|e)(\+|\-)?{DD}+)? :
                    {token,{float,TokenLine,list_to_float(TokenChars)}}.
{DD}+#{HD}+       : base(TokenLine, TokenChars).
{DD}+             : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.

%%\#{AC}+         : {token, {num_var, TokenLine, lists:sublist(TokenChars, 2, TokenLen - 1)}}.
%%\${AC}+         : {token, {str_var, TokenLine, lists:sublist(TokenChars, 2, TokenLen - 1)}}.
%%\?{AC}+         : {token, {log_var, TokenLine, lists:sublist(TokenChars, 2, TokenLen - 1)}}.
%%\!{AC}+         : {token, {log_var_not, TokenLine, lists:sublist(TokenChars, 2, TokenLen - 1)}}.
%%\<{AC}+\>       : {token, {enum_var, TokenLine, lists:sublist(TokenChars, 2, TokenLen - 2)}}.

{ACL}{AC}*        : Name = list_to_atom(TokenChars),
                    { token, case reserved_word(Name) of
                                true  -> {Name, TokenLine};
                                false -> {name, TokenLine, Name}
                    end }.

({ACU}|_){AC}*	:    {token,{variable,TokenLine,list_to_atom(TokenChars)}}.

{DQUOTED}       : S = lists:sublist(TokenChars, 2, TokenLen - 2),
                  {token,{string,TokenLine,string_gen(S)}}.

{SQUOTED}       : S = lists:sublist(TokenChars, 2, TokenLen - 2),
                  {token,{pattern,TokenLine,string_gen(S)}}.

\$(\\{O}{O}{O}|\\\^.|\\.|.) :
		    {token,{integer,TokenLine,cc_convert(TokenChars)}}.

==              :    {token,{'==',TokenLine}}.
<>              :    {token,{'<>',TokenLine}}.
>=              :    {token,{'>=',TokenLine}}.
=<              :    {token,{'=<',TokenLine}}.
<=              :    {token,{'<=',TokenLine}}.

[]()[}{|!?/;:,.*+#<>=-] :
		    {token,{list_to_atom(TokenChars),TokenLine}}.

\.{WS}          :    {end_token,{dot,TokenLine}}.
{WS}+           :    skip_token.
{COMMENT}       :    skip_token.

Erlang code.

-export([reserved_word/1]).

reserved_word('define') -> true;
reserved_word('apply') -> true;
reserved_word('for') -> true;
reserved_word('of') -> true;
reserved_word('in') -> true;
reserved_word('populate') -> true;
reserved_word('if') -> true;
reserved_word('as') -> true;
reserved_word('values') -> true;

reserved_word('module') -> true;
reserved_word('type') -> true;
reserved_word('edit') -> true;
reserved_word('enum') -> true;
reserved_word('domain') -> true;
reserved_word('predicate') -> true;
reserved_word('normalizer') -> true;
reserved_word('entity') -> true;
reserved_word('attribute') -> true;
reserved_word('key') -> true;
reserved_word('unique') -> true;
reserved_word('primary') -> true;
reserved_word('foreign') -> true;
reserved_word('scope') -> true;

reserved_word('not') -> true;
reserved_word('and') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;

reserved_word('matches') -> true;

reserved_word(_) -> false.

base(L, Cs) ->
    H = string:chr(Cs, $#),
    case list_to_integer(string:substr(Cs, 1, H-1)) of
	B when B > 16 -> {error,"illegal base"};
	B ->
	    case base(string:substr(Cs, H+1), B, 0) of
		error -> {error,"illegal based number"};
		N -> {token,{integer,L,N}}
	    end
    end.

base([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) -> error;
base([], Base, N) -> N.

cc_convert([$$,$\\|Cs]) ->
    hd(string_escape(Cs));
cc_convert([$$,C]) -> C.

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\000, C =< $\s ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.
