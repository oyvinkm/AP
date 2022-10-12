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
       , test_analytics()
       , test_get_analytics()
       , test_alias_analytics()
       , test_multiple_analytics()
       , test_lookup_update()
       , test_lookup_fail()
       , test_badfunc_analytics()
       , test_delete()
       , test_delete_alias()
       , test_delete_original()
       , test_remove_analytics()
       , test_analytics_error()
       ]
      }
    ].

test_start_server() ->
    {"+ We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_shortcode_smiley() ->
    {"+ Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
     end }.
test_alias_happy() -> 
  {"+ Register new alias",
  fun() -> 
    {ok, S} = emoji:start([]),
    ok = emoji:new_shortcode(S, "smiley",
                            <<240,159,152,131>>),
    ?assertEqual(ok, emoji:alias(S, "smiley", "happy"))
    end}.
  test_lookup_alias_happy() -> 
    {"+ Lookup Short", 
    fun() ->
      {ok, S} = emoji:start([]),
      ok = emoji:new_shortcode(S, "smiley",
                            <<240,159,152,131>>),
      emoji:alias(S, "smiley", "happy"),
      ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "happy"))
      end}.
  test_lookup_alias_poop() -> 
        {"+ Lookup Non Existing Alias", 
        fun() ->
          {ok, S} = emoji:start([]),
          ok = emoji:new_shortcode(S, "smiley",
                                <<240,159,152,131>>),
          emoji:alias(S, "smiley", "happy"),
          ?assertEqual( no_emoji, emoji:lookup(S, "poop"))
          end}.
  test_lookup_alias_glad() -> 
        {"+ Lookup Mulitple Alias", 
          fun() ->
            {ok, S} = emoji:start([]),
            ok = emoji:new_shortcode(S, "smiley",
                              <<240,159,152,131>>),
            emoji:alias(S, "smiley", "happy"),
            emoji:alias(S, "smiley", "glad"),
            ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "glad"))
           end}.
  test_lookup_nested_alias_glad() -> 
    {"+ Lookup Mulitple Alias", 
      fun() ->
        {ok, S} = emoji:start([]),
        ok = emoji:new_shortcode(S, "smiley",
                        <<240,159,152,131>>),
        emoji:alias(S, "smiley", "happy"),
        emoji:alias(S, "happy", "glad"),
        ?assertEqual({ok, <<240,159,152,131>>}, emoji:lookup(S, "glad"))
        end}.
  test_lookup_emoji_small() ->
    {"+ Lookup for small set of emojis",
      fun() -> 
        {ok, S} = emoji:start(someemoji:small()),
        ?assertEqual({ok, <<"🔄"/utf8>>}, 
        emoji:lookup(S, "arrows_counterclockwise"))
        end}.
  test_lookup_emoji_medium() ->
    {"+ Lookup for medium set of emojis",
      fun() -> 
        {ok, S} = emoji:start(someemoji:medium()),
        ?assertEqual({ok, <<240,159,152,143>>}, 
        emoji:lookup(S, "smirking face"))
        end}.  
  test_lookup_emoji_medium2() ->
    {"+ Lookup for medium set of emojis",
      fun() -> 
        {ok, S} = emoji:start(someemoji:medium()),
        ?assertEqual({ok, 
        <<240,159,145,174,240,159,143,191,226,128,141,226,153,130,239,184,143>>}, 
        emoji:lookup(S, "man police officer: dark skin tone"))
        end}.
  test_many_news_and_alias() -> 
    {"+ Register shortcodes to large server.",
      fun() -> 
        {ok, S} = emoji:start(someemoji:medium()),
        ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
        ok = emoji:new_shortcode(S, "clever smiley", <<"🤓"/utf8>>),
        ok = emoji:alias(S, "sweet", "cute"),
        ok = emoji:alias(S, "clever smiley", "smart"),
        ?assertEqual({ok, <<"🤓"/utf8>>}, emoji:lookup(S, "smart"))
        end}.
  test_analytics() -> 
    {"+ Test simple analytics.",
      fun() -> {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
      ?assertEqual(ok, 
      emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Counter", 0))
      end}.
  test_get_analytics() -> 
    {"+ Test simple get analytics.",
      fun() -> {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
      ok = emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertEqual({ok, [{"Counter", 0}]}, emoji:get_analytics(S, "sweet"))
      end}.
  test_alias_analytics() -> 
    {"+ Test simple alias analytics.",
      fun() -> {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
      ok = emoji:alias(S, "sweet", "cute"),
      ok = emoji:analytics(S, "cute", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertEqual({ok, [{"Counter", 0}]}, emoji:get_analytics(S, "cute"))
      end}.
  test_multiple_analytics() ->
    {"+ Test multiple analytics.",
      fun() -> {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
      ok = emoji:alias(S, "sweet", "cute"),
      ok = emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Counter", 0),
      ok = emoji:analytics(S, "sweet", fun(X,_) -> X end, "Last", "sweet"),
      ?assertEqual({ok, [{"Counter", 0}, {"Last", "sweet"}]}, 
      emoji:get_analytics(S, "cute"))
      end}.
  test_lookup_update() -> 
    {"+ Test lookup affects all alias'.",
      fun() -> {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
      ok = emoji:alias(S, "sweet", "cute"),
      ok = emoji:alias(S, "sweet", "pen"),
      ok = emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Counter", 0),
      {ok, _} = emoji:lookup(S, "sweet"),
      {ok, _} = emoji:lookup(S, "cute"),
      {ok, _} = emoji:lookup(S, "cute"),
      ?assertEqual({ok, [{"Counter", 3}]}, emoji:get_analytics(S, "cute"))
      end}.
  test_lookup_fail() ->
    {"- aTest lookup analytic should fail due to recursion.",
      fun() -> {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
      ok = emoji:alias(S, "sweet", "cute"),
      ok = emoji:alias(S, "sweet", "pen"),
      ok = emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Last", "sweet"),
      ok = emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Counter", 0),
      {ok, _} = emoji:lookup(S, "sweet"),
      {ok, _} = emoji:lookup(S, "cute"),
      {ok, _} = emoji:lookup(S, "cute"),
      {ok, _} = emoji:lookup(S, "pen"),
      {ok, _} = emoji:lookup(S, "pen"),
      ?assertEqual({ok, [{"Last", "pen"}, {"Counter", 5}]}, 
      emoji:get_analytics(S, "cute"))
      end}.
  test_badfunc_analytics() -> 
    {"+ Tests that function that throw error works.",
      fun() -> {ok, S} = emoji:start(someemoji:medium()),
      ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
      ok = emoji:analytics(S, "sweet", fun(D, _) -> throw(D) end, "Throw", "s"),
      ?assertEqual({ok,  <<"😏"/utf8>>}, emoji:lookup(S, "sweet"))
      end}.
  test_delete() -> 
    {"+ Test simple delete", 
    fun() -> {ok, S} = emoji:start(someemoji:medium()),
    ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
    ok = emoji:delete(S, "sweet"),
    ?assertEqual(no_emoji, emoji:lookup(S, "sweet"))
    end}.
  test_delete_alias() -> 
    {"+ Test delete alias with original.", 
    fun() -> {ok, S} = emoji:start(someemoji:medium()),
    ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
    ok = emoji:alias(S, "sweet", "cute"),
    ok = emoji:delete(S, "sweet"),
    ?assertEqual(no_emoji, emoji:lookup(S, "sweet"))
    end}.
  test_delete_original() -> 
    {"+ Test delete original with alias.", 
    fun() -> {ok, S} = emoji:start(someemoji:medium()),
    ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
    ok = emoji:alias(S, "sweet", "cute"),
    ok = emoji:delete(S, "sweet"),
    ?assertEqual(no_emoji, emoji:lookup(S, "cute"))
    end}.
  test_remove_analytics() ->
    {"+ Test remove analytics", 
    fun() -> {ok, S} = emoji:start(someemoji:medium()),
    ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
    ok = emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Counter", 0),
    ok = emoji:remove_analytics(S, "sweet", "Counter"),
    ?assertEqual({ok, []}, emoji:get_analytics(S, "sweet"))
    end}.
    test_analytics_error() ->
      {"-Test wrong analytics functionality",
        fun() -> {ok, S} = emoji:start(someemoji:medium()),
        ok = emoji:new_shortcode(S, "sweet", <<"😏"/utf8>>),
        ok = emoji:analytics(S, "sweet", fun(_, N) -> N+1 end, "Counter", 0),
        ok = emoji:analytics(S, "sweet", fun(X,_) -> throw(X) end, "Throw", "sweet"),
        ok = emoji:lookup(S, "sweet"),
        ok = emoji:lookup(S, "sweet"),
        ok = emoji:lookup(S, "sweet"),
        ?assertEqual({ok, [{"Counter", 3}, {"Throw", none}]}, 
        emoji:get_analytics(S, "sweet"))
        end}.
  



  



