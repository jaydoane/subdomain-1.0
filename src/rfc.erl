-module(rfc).

-export([is_valid_domain_label/1,
         is_valid_local_part/1]).

-include_lib("eunit/include/eunit.hrl").

%% http://tools.ietf.org/html/rfc3696

-define(disallowed_label_chars, "[^a-z0-9-]"). % only allow a-z, 0-9, or "-"
-define(disallowed_local_part_chars, "[^a-zA-Z0-9!#$%&'*+-/=?^_`{|}~.]").
-define(multiple_consecutive_periods, "[.]{2,}").

starts_with(Str, SubStr) ->
    case string:str(Str, SubStr) of 
        1 -> 
            true;
        _ ->
            false
    end.
    
ends_with(Str, SubStr) ->
    Len = string:len(Str),
    case string:rstr(Str, SubStr) of 
        Len -> 
            true;
        _ ->
            false
    end.

%% http://en.wikipedia.org/wiki/Domain_name

is_valid_domain_label(Str) when not is_list(Str) ->
    false;
is_valid_domain_label(Str) when length(Str) < 1; length(Str) > 63 ->
    false;
is_valid_domain_label(Str) ->
    S = string:to_lower(Str),
    case re:run(S, ?disallowed_label_chars) of
        {match, _} ->
            false; % contains illegal characters
        nomatch ->
            case starts_with(S, "-") or ends_with(S, "-") of
                true ->
                    false; % begins or ends with hyphen
                false ->
                    true
            end
    end.

%% http://en.wikipedia.org/wiki/E-mail_address

is_valid_local_part(Str) when not is_list(Str) ->
    false;
is_valid_local_part(Str) when length(Str) < 1; length(Str) > 64 ->
    false;
is_valid_local_part(Str) ->
    case re:run(Str, ?disallowed_local_part_chars) of
        {match, _} ->
            false; % contains illegal characters
        nomatch ->
            case starts_with(Str, ".") or ends_with(Str, ".") of
                true ->
                    false; % begins or ends with period
                false ->
                    case re:run(Str, ?multiple_consecutive_periods) of
                        {match, _} ->
                            false;  % contains two or more consecutive periods
                        nomatch ->
                            true
                    end
            end
    end.

-define(string_of_len_63, 
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").
-define(string_of_len_64, 
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").
-define(string_of_len_65, 
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").

rfc_test_() ->   
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   fun(_) ->
           [
            ?_assert(false == starts_with("-", "")),
            ?_assert(true == starts_with("-", "-")),
            ?_assert(true == ends_with("-", "-")),
            ?_assert(true == starts_with("--", "-")),
            ?_assert(true == ends_with("--", "-")),
            ?_assert(false == ends_with("a-a", "-")),
            ?_assert(false == ends_with("- ", "-")),

            ?_assert(false == is_valid_domain_label(1)),
            ?_assert(false == is_valid_domain_label(a)),
            ?_assert(false == is_valid_domain_label("")),
            ?_assert(true == is_valid_domain_label(?string_of_len_63)),
            ?_assert(false == is_valid_domain_label(?string_of_len_64)),
            ?_assert(true == is_valid_domain_label("a")),
            ?_assert(false == is_valid_domain_label("_")),
            ?_assert(false == is_valid_domain_label("-")),
            ?_assert(true == is_valid_domain_label("a-a")),
            ?_assert(false == is_valid_domain_label("a_a")),

            ?_assert(false == is_valid_local_part(1)),
            ?_assert(false == is_valid_local_part(a)),
            ?_assert(false == is_valid_local_part("")),
            ?_assert(true == is_valid_local_part(?string_of_len_64)),
            ?_assert(false == is_valid_local_part(?string_of_len_65)),
            ?_assert(true == is_valid_local_part("a")),
            ?_assert(true == is_valid_local_part("_")),
            ?_assert(false == is_valid_local_part(".")),
            ?_assert(false == is_valid_local_part("_.")),
            ?_assert(true == is_valid_local_part("a-a")),
            ?_assert(false == is_valid_local_part("a..a")),
            ?_assert(false == is_valid_local_part("(")),
            ?_assert(true == is_valid_local_part("a-zA-Z0-9!#$%&'*+-/=?^_`{|}.~"))
           ]

   end}.
