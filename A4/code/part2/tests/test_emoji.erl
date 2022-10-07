-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server()
       , test_shortcode_smiley()
       , test_alias_happy()
       , test_lookup_alias_happy()
       , test_lookup_alias_poop()
       , test_lookup_alias_glad()
       , test_lookup_nested_alias_glad()
       , test_lookup_emoji_small()
       , test_lookup_emoji_medium()
       , test_lookup_emoji_medium2()
       , test_many_news_and_alias()
       ]
      }
    ].

test_start_server() ->
    {"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_shortcode_smiley() ->
    {"Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
     end }.
test_alias_happy() -> 
  {"Register new alias",
  fun() -> 
    {ok, S} = emoji:start([]),
    ok = emoji:new_shortcode(S, "smiley",
                            <<240,159,152,131>>),
    ?assertEqual(ok, emoji:alias(S, "smiley", "happy"))
    end}.
  test_lookup_alias_happy() -> 
    {"Lookup Short", 
    fun() ->
      {ok, S} = emoji:start([]),
      ok = emoji:new_shortcode(S, "smiley",
                            <<240,159,152,131>>),
      emoji:alias(S, "smiley", "happy"),
      ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "happy"))
      end}.
  test_lookup_alias_poop() -> 
        {"Lookup Non Existing Alias", 
        fun() ->
          {ok, S} = emoji:start([]),
          ok = emoji:new_shortcode(S, "smiley",
                                <<240,159,152,131>>),
          emoji:alias(S, "smiley", "happy"),
          ?assertEqual({error, no_emoji}, emoji:lookup(S, "poop"))
          end}.
  test_lookup_alias_glad() -> 
        {"Lookup Mulitple Alias", 
          fun() ->
            {ok, S} = emoji:start([]),
            ok = emoji:new_shortcode(S, "smiley",
                              <<240,159,152,131>>),
            emoji:alias(S, "smiley", "happy"),
            emoji:alias(S, "smiley", "glad"),
            ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "glad"))
           end}.
  test_lookup_nested_alias_glad() -> 
    {"Lookup Mulitple Alias", 
      fun() ->
        {ok, S} = emoji:start([]),
        ok = emoji:new_shortcode(S, "smiley",
                        <<240,159,152,131>>),
        emoji:alias(S, "smiley", "happy"),
        emoji:alias(S, "happy", "glad"),
        ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "glad"))
        end}.
  test_lookup_emoji_small() ->
    {"Lookup for small set of emojis",
      fun() -> 
        {ok, S} = emoji:start(someemoji:small()),
        ?assertEqual({ok, <<"🔄"/utf8>>}, emoji:lookup(S, "arrows_counterclockwise"))
        end}.
  test_lookup_emoji_medium() ->
    {"Lookup for medium set of emojis",
      fun() -> 
        {ok, S} = emoji:start(someemoji:medium()),
        ?assertEqual({ok, <<240,159,152,143>>}, emoji:lookup(S, "smirking face"))
        end}.  
  test_lookup_emoji_medium2() ->
    {"Lookup for medium set of emojis",
      fun() -> 
        {ok, S} = emoji:start(someemoji:medium()),
        ?assertEqual({ok, 
        <<240,159,145,174,240,159,143,191,226,128,141,226,153,130,239,184,143>>}, 
        emoji:lookup(S, "man police officer: dark skin tone"))
        end}.
  test_many_news_and_alias() -> 
    {"Register shortcodes to large server.",
      fun() -> 
        {ok, S} = emoji:start(someemoji:medium()),
        ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
        ok = emoji:new_shortcode(S, "clever smiley", <<"🤓"/utf8>>),
        ok = emoji:alias(S, "sweet", "cute"),
        ok = emoji:alias(S, "clever smiley", "smart"),
        ?assertEqual({ok, <<"🤓"/utf8>>}, emoji:lookup(S, "smart"))
        end}.



