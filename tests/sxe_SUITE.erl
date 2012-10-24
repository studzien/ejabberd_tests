%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(sxe_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(MUC_HOST, <<"sxe.localhost">>).
-define(MUC_CLIENT_HOST, <<"localhost/res1">>).
-define(PASSWORD, <<"password">>).
-define(SUBJECT, <<"subject">>).

-define(NS_MUC_REQUEST, <<"http://jabber.org/protocol/muc#request">>).
-define(NS_MUC_ROOMCONFIG, <<"http://jabber.org/protocol/muc#roomconfig">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          {group, basic},
          {group, moderated}
         ].

groups() -> [
             {basic, [sequence], [
                            basic_emptystate,
                            basic_newnode,
                            basic_removenode,
                            basic_editnode,
                            basic_removeorphans
                         ]},
             {moderated, [sequence], [
                            moderated_simple,
                            moderated_denied
                        ]}
            ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(basic, Config) ->
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, <<"testroom">>, <<"testowner">>,
        [{persistent, true}]);

init_per_group(moderated, Config) ->
      RoomName = <<"testroom">>,
      RoomNick = <<"testowner">>,
      Config1 = escalus:create_users(Config),
      [Alice | _] = ?config(escalus_users, Config1),
      start_room(Config1, Alice, RoomName, RoomNick,
                         [{persistent, true}, {allow_change_subj, false}, {moderated, true},
                                   {members_by_default, false}]);

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(basic, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(moderated, Config) ->
      destroy_room(Config),
      escalus:delete_users(Config);

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    timer:sleep(1000),
    escalus:end_per_testcase(CaseName, Config).


basic_emptystate(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        Stanzas = escalus:wait_for_stanzas(Alice, 3),
        escalus:assert_many([fun ?MODULE:is_groupchat_message/1,
                             fun escalus_pred:is_presence/1,
                             fun(Stanza) ->
                                is_sxe_message(Stanza) andalso has_state(Stanza)
                             end
                            ], Stanzas)
    end).

basic_newnode(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 3),
        El1 = stanza_new_node({element, <<"line-1">>, <<"line">>, <<"document">>}),
        El2 = stanza_new_node({text, <<"line-2">>, <<"line">>, <<"document">>, <<"This is text">>}),
        Packet = stanza_nodes([El1, El2]),
        escalus:send(Alice, stanza_to_room(Packet, ?config(room, Config))),
        escalus:wait_for_stanza(Alice),

        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        Msg = get_message(escalus:wait_for_stanzas(Bob, 4)),
        true = has_state_element(Msg, El1),
        true = has_state_element(Msg, El2)
    end).

basic_removenode(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 3),
        El = stanza_remove(<<"line-2">>),
        Packet = stanza_nodes([El]),
        escalus:send(Alice, stanza_to_room(Packet, ?config(room, Config))),
        Msg2 = escalus:wait_for_stanza(Alice),
        true = has_element(Msg2, El),

        El1 = stanza_new_node({element, <<"line-1">>, <<"line">>, <<"document">>}),
        El2 = stanza_new_node({text, <<"line-2">>, <<"line">>, <<"document">>, <<"This is text">>}),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        Msg = get_message(escalus:wait_for_stanzas(Bob, 4)),
        true = has_state_element(Msg, El1),
        false = has_state_element(Msg, El2)
    end).

basic_editnode(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice, Bob, Mike) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 3),
        
        El = stanza_new_node({text, <<"line-3">>, <<"line">>, <<"document">>, <<"Magic!">>}),
        ElAfter = stanza_new_node({text, <<"line-3">>, <<"line">>, <<"line-1">>, <<"Magic!">>}),
        Packet = stanza_nodes([El]),
        escalus:send(Alice, stanza_to_room(Packet, ?config(room, Config))),
        escalus:wait_for_stanza(Alice),
        
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        Msg = get_message(escalus:wait_for_stanzas(Bob, 4)),
        true = has_state_element(Msg, El),
        false = has_state_element(Msg, ElAfter),
        
        El2 = stanza_set(<<"line-3">>, [{<<"parent">>, <<"line-1">>}]),
        escalus:send(Alice, stanza_to_room(stanza_nodes([El2]), ?config(room, Config))),
        escalus:wait_for_stanza(Alice),
        Msg3 = escalus:wait_for_stanza(Alice),
        true = has_element(Msg3, El2),
        Msg4 = escalus:wait_for_stanza(Bob), 
        true = has_element(Msg4, El2),   

        timer:sleep(1000),
        escalus:send(Mike, stanza_muc_enter_room(?config(room, Config), <<"mike">>)),
        Msg2 = get_message(escalus:wait_for_stanzas(Mike, 5)),
        false = has_state_element(Msg2, El),
        true = has_state_element(Msg2, ElAfter)

    end).

basic_removeorphans(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice, Bob, Mike) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 3),
        El0 = stanza_new_node({element, <<"line-1">>, <<"line">>, <<"document">>}),
        El1 = stanza_new_node({text, <<"line-4">>, <<"line">>, <<"line-1">>, <<"Well, hey, this is text too!">>}),
        El2 = stanza_new_node({text, <<"line-5">>, <<"line">>, <<"line-4">>, <<"This is text">>}),
        El3 = stanza_new_node({text, <<"line-6">>, <<"line">>, <<"document">>, <<"True magic">>}),
        Packet = stanza_nodes([El1, El2, El3]),
        escalus:send(Alice, stanza_to_room(Packet, ?config(room, Config))),
        Msg2 = escalus:wait_for_stanza(Alice),
        true = has_element(Msg2, El1),
        true = has_element(Msg2, El2),
        true = has_element(Msg2, El3),

        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        Stanzas = escalus:wait_for_stanzas(Bob, 4),
        Msg = get_message(Stanzas),
        true = has_state_element(Msg, El0),
        true = has_state_element(Msg, El1),
        true = has_state_element(Msg, El2),
        true = has_state_element(Msg, El3),

        RemEl0 = stanza_remove(<<"line-1">>),
        escalus:send(Alice, stanza_to_room(stanza_nodes([RemEl0]), ?config(room, Config))),
        Msg3 = get_message(escalus:wait_for_stanzas(Alice, 2)),
        true = has_element(Msg3, RemEl0),
        Msg4 = escalus:wait_for_stanza(Bob),
        true = has_element(Msg4, RemEl0),

        timer:sleep(1000),
        escalus:send(Mike, stanza_muc_enter_room(?config(room, Config), <<"mike">>)),
        Msg1 = get_message(escalus:wait_for_stanzas(Mike, 5)),
        false = has_state_element(Msg1, El0),
        false = has_state_element(Msg1, El1),
        false = has_state_element(Msg1, El2),
        true = has_state_element(Msg1, El3)
    end).

moderated_simple(Config) ->        
      escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room      
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 3),
        %% Bob joins room             
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 4),
        %% Skip Bob's presence        
        escalus:wait_for_stanza(Alice),

        %% Alice grants voice to Bob  
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
                                             [{<<"bob">>,<<"participant">>}])),

        %% Alice receives success information and new Bob's presence
        Pred = fun(Stanza) ->         
            is_presence_with_role(Stanza, <<"participant">>) andalso
            escalus_pred:is_stanza_from(
              room_address(?config(room, Config), <<"bob">>), Stanza)
        end,                      
        escalus:assert_many([is_iq_result, Pred],
                            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob should receive his new presence
        escalus:assert(Pred, escalus:wait_for_stanza(Bob)),
        
        El1 = stanza_new_node({element, <<"line-1">>, <<"line">>, <<"document">>}),
        El2 = stanza_new_node({text, <<"line-2">>, <<"line">>, <<"document">>, <<"This is text">>}),
        Packet = stanza_nodes([El1, El2]),
        escalus:send(Bob, stanza_to_room(Packet, ?config(room, Config))),
        Msg = escalus:wait_for_stanza(Bob),
        true = has_element(Msg, El1),
        true = has_element(Msg, El2),
        Msg1 = escalus:wait_for_stanza(Alice),
        true = has_element(Msg1, El1),
        true = has_element(Msg1, El2)
    end).

moderated_denied(Config) ->        
      escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room      
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 3),
        %% Bob joins room             
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 4),
        %% Skip Bob's presence        
        escalus:wait_for_stanza(Alice),

        El1 = stanza_new_node({element, <<"line-1">>, <<"line">>, <<"document">>}),
        El2 = stanza_new_node({text, <<"line-2">>, <<"line">>, <<"document">>, <<"This is text">>}),
        Packet = stanza_nodes([El1, El2]),
        escalus:send(Bob, stanza_to_room(Packet, ?config(room, Config))),
        Err = escalus:wait_for_stanza(Bob),
        escalus_assert:is_error(Err, <<"auth">>, <<"forbidden">>),
        escalus_assert:has_no_stanzas(Bob),
        escalus_assert:has_no_stanzas(Alice)
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
get_message(Stanzas) ->
    [Msg] = lists:filter(fun(#xmlelement{name = <<"message">>} = El) ->
                            case exml_query:attr(El, <<"type">>) of
                                <<"sxe">> ->  true;
                                _ -> false
                            end;
                            (_) -> false
                         end, Stanzas),
    Msg.

stanza_set(TargetId, Props) ->
    #xmlelement{name = <<"set">>,
                attrs = [{<<"target">>, TargetId} | Props]}.

stanza_remove(Id) ->
    #xmlelement{name = <<"remove">>,
                attrs = [{<<"target">>, Id}]}.

stanza_new_node({text, Id, Name, Parent, Text}) ->
    #xmlelement{name = <<"new">>,
                attrs = [{<<"type">>, <<"text">>},
                         {<<"name">>, Name},
                         {<<"parent">>, Parent},
                         {<<"rid">>, Id},
                         {<<"chdata">>, Text}]};

stanza_new_node({element, Id, Name, Parent}) ->
    #xmlelement{name = <<"new">>,
                attrs = [{<<"type">>, <<"element">>},
                         {<<"name">>, Name},
                         {<<"parent">>, Parent},
                         {<<"rid">>, Id}]}.

stanza_nodes(Nodes) ->
    SXE = #xmlelement{name = <<"sxe">>,
                      attrs = [{<<"xmlns">>, <<"urn:xmpp:sxe:0">>}],
                      children = Nodes},
    #xmlelement{name = <<"message">>,
                attrs = [{<<"type">>, <<"sxe">>}],
                children = [SXE]}.

is_sxe_message(Msg) ->
    exml_query:attr(Msg, <<"type">>) == <<"sxe">> andalso
        exml_query:path(Msg, [{element, <<"sxe">>}]) /= undefined.


has_state(Msg) ->
    State = exml_query:path(Msg, [{element, <<"sxe">>}, {element, <<"state">>}]),
    DocBeg = exml_query:subelement(State, <<"document-begin">>),
    DocEnd = exml_query:subelement(State, <<"document-end">>),
    exml_query:attr(DocBeg, <<"prolog">>) /= undefined andalso
        exml_query:attr(DocEnd, <<"last-sender">>) /= undefined andalso
        exml_query:attr(DocEnd, <<"last-id">>) /= undefined andalso
        exml_query:subelement(State, <<"new">>) /= undefined.

has_state_element(Msg, El) ->
    Elements = (exml_query:path(Msg, [{element, <<"sxe">>}, {element, <<"state">>}]))#xmlelement.children,
    lists:any(fun(Item) -> 
                Item#xmlelement.name =:= El#xmlelement.name andalso
                Item#xmlelement.attrs -- El#xmlelement.attrs =:= []
              end, Elements).

has_element(Msg, El) ->
    Elements = (exml_query:subelement(Msg, <<"sxe">>))#xmlelement.children,
    lists:any(fun(Item) -> 
                Item#xmlelement.name =:= El#xmlelement.name andalso
                Item#xmlelement.attrs -- El#xmlelement.attrs =:= []
              end, Elements).

%Basic MUC protocol
stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlelement{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        Room, Nick).

start_room(Config, User, Room, Nick, Opts) ->
    From = generate_rpc_jid(User),
    escalus_ejabberd:rpc(mod_sxe, create_instant_room,
        [<<"localhost">>, Room, From, Nick,
            Opts]),
    [{nick, Nick}, {room, Room} | Config].

destroy_room(Config) ->
    case escalus_ejabberd:rpc(ets, lookup, [muc_online_room,
        {?config(room, Config), <<"muc.localhost">>}]) of
        [{_,_,Pid}|_] -> gen_fsm:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

room_address(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.

%%--------------------------------------------------------------------
%% Helpers (stanzas)
%%--------------------------------------------------------------------

stanza_message_to_room(Room, Payload) ->
    stanza_to_room(#xmlelement{name = <<"message">>, children = Payload}, Room).

stanza_change_availability(NewStatus, Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence( <<"available">>,
                                [
                                #xmlelement{ name = <<"show">>,children=[ #xmlcdata{content=[<<"xa">>]}]},
                                #xmlelement{ name = <<"status">>,children=[ #xmlcdata{content=[NewStatus]}]}
                                ]),
        Room, Nick).

stanza_muc_enter_room_history_setting(Room, Nick, Setting, Value) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlelement{ name = <<"x">>, 
                    						  attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}],
											  children = [#xmlelement{name= <<"history">>, attrs=[{Setting, Value}]}]}]),
        Room, Nick).

stanza_room_subject(Room, Subject) ->
    stanza_to_room(#xmlelement{name = <<"message">>,
        attrs = [{<<"type">>,<<"groupchat">>}],
        children = [#xmlelement{
            name = <<"subject">>,
            children = [exml:escape_cdata(Subject)]
        }]
    }, Room).

stanza_mediated_invitation(Room, Invited) ->
    Payload = [ #xmlelement{name = <<"invite">>,
        attrs = [{<<"to">>, escalus_utils:get_short_jid(Invited)}]} ],
    stanza_to_room(#xmlelement{name = <<"message">>,
        children = [ #xmlelement{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
            children = Payload }
        ]}, Room).

stanza_mediated_invitation_decline(Room,Sender) ->
    Payload = [ #xmlelement{name = <<"decline">>,
        attrs = [{<<"to">>, escalus_utils:get_short_jid(Sender)}]} ],
    stanza_to_room(#xmlelement{name = <<"message">>,
        children = [ #xmlelement{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
            children = Payload }
        ]}, Room).

stanza_set_roles(Room, List) ->
    Payload = lists:map(fun({Nick, Role}) ->
        #xmlelement{name = <<"item">>,
        attrs = [{<<"nick">>, Nick}, {<<"role">>, Role}]};
    ({Nick, Role, Reason}) ->
        #xmlelement{name = <<"item">>,
        attrs = [{<<"nick">>, Nick}, {<<"role">>, Role}],
        children = [#xmlelement{
            name = <<"reason">>,
            children = #xmlcdata{content = Reason}}
        ]}
    end, List),
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

stanza_set_affiliations(Room, List) ->
    Payload = lists:map(fun({JID, Affiliation}) ->
        #xmlelement{name = <<"item">>,
        attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}]};
    ({JID, Affiliation, Reason}) ->
        #xmlelement{name = <<"item">>,
        attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}],
        children = [#xmlelement{
            name = <<"reason">>,
            children = #xmlcdata{content = Reason}}
        ]}
    end, List),
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

stanza_role_list_request(Room, Role) ->
    Payload = [ #xmlelement{name = <<"item">>,
        attrs = [{<<"role">>, Role}]} ],
    stanza_to_room(escalus_stanza:iq_get(?NS_MUC_ADMIN, Payload), Room).

stanza_affiliation_list_request(Room, Affiliation) ->
    Payload = [ #xmlelement{name = <<"item">>,
        attrs = [{<<"affiliation">>, Affiliation}]} ],
    stanza_to_room(escalus_stanza:iq_get(?NS_MUC_ADMIN, Payload), Room).

stanza_ban_list_request(Room) ->
    stanza_affiliation_list_request(Room, <<"outcast">>).

stanza_ban_user(User, Room) ->
  stanza_set_affiliations(Room, [{escalus_utils:get_short_jid(User), <<"outcast">>}]).

stanza_ban_user(User, Room, Reason) ->
  stanza_set_affiliations(Room, [{escalus_utils:get_short_jid(User), <<"outcast">>, Reason}]).

stanza_join_room(Room, Nick) ->
    stanza_to_room(#xmlelement{name = <<"presence">>, children =
        #xmlelement{
            name = <<"x">>,
            attrs = [{<<"xmlns">>,<<"http://jabber.org/protocol/muc">>}]
        }
    },Room, Nick).

stanza_voice_request_form(Room) ->
    Payload = [ form_field({<<"muc#role">>, <<"participant">>, <<"text-single">>}) ],
    stanza_message_to_room(Room, stanza_form(Payload, ?NS_MUC_REQUEST)).

stanza_voice_request_approval(Room, JID, Nick) ->
    Items = [{<<"muc#role">>, <<"participant">>, <<"text-single">>},
        {<<"muc#jid">>, JID, <<"jid-single">>},
        {<<"muc#roomnick">>, Nick, <<"text-single">>},
        {<<"muc#request_allow">>, <<"true">>, <<"boolean">>}],
    Payload = [ form_field(El) || El <- Items],
    stanza_message_to_room(Room, stanza_form(Payload, ?NS_MUC_REQUEST)).

stanza_voice_request_approval_nonick(Room, JID) ->
    Items = [{<<"muc#role">>, <<"participant">>, <<"text-single">>},
        {<<"muc#jid">>, JID, <<"jid-single">>},
        {<<"muc#request_allow">>, <<"true">>, <<"boolean">>}],
    Payload = [ form_field(El) || El <- Items],
    stanza_message_to_room(Room, stanza_form(Payload, ?NS_MUC_REQUEST)).

stanza_configuration_form(Room, Params) ->
    DefaultParams = [],
    FinalParams = lists:foldl(
        fun({Key,_Val,_Type},Acc) ->
            lists:keydelete(Key,1,Acc)
        end,
        DefaultParams, Params) ++ Params,
    Payload = [ form_field(FieldData) || FieldData <- FinalParams ],
    stanza_to_room(escalus_stanza:iq_set(
          ?NS_MUC_OWNER, stanza_form(Payload, ?NS_MUC_ROOMCONFIG)), Room).

stanza_cancel(Room) ->
    Payload = #xmlelement{
        name = <<"x">>,
        attrs = [{<<"xmlns">>,<<"jabber:x:data">>}, {<<"type">>,<<"cancel">>}]
    },
    stanza_to_room(escalus_stanza:iq_set(
          ?NS_MUC_OWNER, Payload), Room).

stanza_form(Payload, Type) ->
    #xmlelement{
        name = <<"x">>,
        attrs = [{<<"xmlns">>,<<"jabber:x:data">>}, {<<"type">>,<<"submit">>}],
        children = [form_field({<<"FORM_TYPE">>, Type, <<"hidden">>}) | Payload]
    }.

form_field({Var, Value, Type}) ->
    #xmlelement{ name  = <<"field">>,
                 attrs = [{<<"type">>, Type},{<<"var">>, Var}],
                 children  = [#xmlelement{ name = <<"value">>,
                                       children = [#xmlcdata{content = Value}] }] }.

stanza_instant_room(Room) ->
    X = #xmlelement{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_DATA_FORMS},
                                             {<<"type">>, <<"submit">>}]},
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_OWNER, [X]), Room).

stanza_reserved_room(Room) ->
    escalus_stanza:to(escalus_stanza:iq_get(?NS_MUC_OWNER, []), Room).

stanza_destroy_room(Room) ->
    Payload = [ #xmlelement{name = <<"destroy">>} ],
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_OWNER, Payload), Room).

stanza_enter_room(Room, Nick) ->
    stanza_to_room(#xmlelement{name = <<"presence">>}, Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_get_rooms() ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%   id='zb8q41f4'
    %%   to='chat.shakespeare.lit'
    %%   type='get'>
    %% <query xmlns='http://jabber.org/protocol/disco#items'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        ?MUC_HOST).

stanza_get_features() ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='lx09df27'
    %%     to='chat.shakespeare.lit'
    %%     type='get'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_INFO, []), <<"to">>,
        ?MUC_HOST).

stanza_get_services(Config) ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='h7ns81g'
    %%     to='shakespeare.lit'
    %%     type='get'>
    %%   <query xmlns='http://jabber.org/protocol/disco#items'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        escalus_config:get_config(ejabberd_domain, Config)).

%%--------------------------------------------------------------------
%% Helpers (assertions)
%%--------------------------------------------------------------------

invite_has_reason(Stanza) ->
    exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"reason">>}, cdata]) =/= undefined.

has_reason(Stanza) ->
    exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"item">>},
        {element, <<"reason">>}]) =/= undefined.

is_message_form(Stanza) ->
    exml_query:path(Stanza,[{element,<<"x">>},
        {attr, <<"xmlns">>}]) =:= ?NS_DATA_FORMS.

is_form(Stanza) ->
    exml_query:path(Stanza,[{element, <<"query">>}, {element,<<"x">>},
        {attr, <<"xmlns">>}]) =:= ?NS_DATA_FORMS.

is_groupchat_message(Stanza) ->
    escalus_pred:is_message(Stanza) andalso
    escalus_pred:has_type(<<"groupchat">>, Stanza).

is_subject_message(Stanza) ->
    is_groupchat_message(Stanza) andalso
    exml_query:subelement(Stanza, <<"subject">>) /= undefined.

is_subject_message(Stanza, Subject) ->
    is_groupchat_message(Stanza) andalso
    exml_query:path(Stanza, [{element,<<"subject">>},cdata]) == Subject.

is_unavailable_presence(Stanza, Status) ->
    escalus_pred:is_presence_with_type(<<"unavailable">>,Stanza) andalso
    is_presence_with_status_code(Stanza, Status).

is_membership_presence(Stanza, Affiliation, Role) ->
    is_presence_with_affiliation(Stanza, Affiliation) andalso
    is_presence_with_role(Stanza, Role).

is_invitation(Stanza) ->
    escalus:assert(is_message, Stanza),
    #xmlelement{} = exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"invite">>}]).

is_invitation_decline(Stanza) ->
    escalus:assert(is_message, Stanza),
    #xmlelement{} = exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"decline">>}]).

is_presence_with_role(Stanza, Role) ->
    is_with_role(exml_query:subelement(Stanza, <<"x">>), Role).

is_iq_with_role(Stanza, Role) ->
    is_with_role(exml_query:subelement(Stanza, <<"query">>), Role).

is_with_role(Stanza, Role) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    lists:any(fun(Item) ->
        exml_query:attr(Item, <<"role">>) =:= Role
    end, Items).

is_presence_with_nick(Stanza, Nick) ->
    escalus_pred:is_presence(Stanza) andalso
    exml_query:path(Stanza,[{element, <<"x">>},
        {element, <<"item">>}, {attribute, <<"nick">>}]) == Nick.

is_presence_with_affiliation(Stanza, Affiliation) ->
    is_affiliation(exml_query:subelement(Stanza, <<"x">>), Affiliation).

is_iq_with_affiliation(Stanza, Affiliation) ->
    is_affiliation(exml_query:subelement(Stanza, <<"query">>), Affiliation).

is_affiliation(Stanza, Affiliation) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    lists:any(fun(Item) ->
        exml_query:attr(Item, <<"affiliation">>) =:= Affiliation
    end, Items).

is_presence_with_jid(Stanza, User) ->
    is_jid(exml_query:subelement(Stanza, <<"x">>), User).

is_presence_with_full_jid(Stanza, User) ->
    is_full_jid(exml_query:subelement(Stanza, <<"x">>), User).

is_iq_with_jid(Stanza, User) ->
    is_jid(exml_query:subelement(Stanza, <<"query">>), User).

is_full_jid(Stanza, User) ->
    Item = exml_query:subelement(Stanza, <<"item">>),
    JID = escalus_utils:get_jid(User),
    JID = exml_query:attr(Item, <<"jid">>).

is_jid(Stanza, User) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    JID = escalus_utils:get_jid(User),
    lists:any(fun(Item) -> exml_query:attr(Item, <<"jid">>) =:= JID end, Items).

is_presence_with_short_jid(Stanza, User) ->
    is_short_jid(exml_query:subelement(Stanza, <<"x">>), User).

is_iq_with_short_jid(Stanza, User) ->
    is_short_jid(exml_query:subelement(Stanza, <<"query">>), User).

is_short_jid(Stanza, User) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    JID = escalus_utils:get_short_jid(User),
    lists:any(fun(Item) -> exml_query:attr(Item, <<"jid">>) =:= JID end, Items).

is_presence_with_status_code(Presence, Code) ->
    escalus:assert(is_presence, Presence),
    Code == exml_query:path(Presence, [{element, <<"x">>}, {element, <<"status">>},
        {attr, <<"code">>}]).

is_message_with_status_code(Message, Code) ->
    escalus_pred:is_message(Message) andalso
    Code == exml_query:path(Message, [{element, <<"x">>}, {element, <<"status">>},
        {attr, <<"code">>}]).

has_status_codes(Stanza, CodeList) ->
    StatusList = exml_query:subelements(exml_query:subelement(Stanza, <<"x">>), <<"status">>),
    StanzaCodes = lists:map(fun(Status) ->
                    exml_query:attr(Status, <<"code">>)
        end, StatusList),
    true = lists:all(fun (Code) ->
                        lists:member(Code, StanzaCodes)
            end, CodeList).


has_feature(Stanza, Feature) ->
    Features = exml_query:path(Stanza, [{element, <<"query">>}, {elements, <<"feature">>}]),
    true = lists:any(fun(Item) ->
                        exml_query:attr(Item, <<"var">>) == Feature
                     end,
                     Features).

was_destroy_presented(#xmlelement{children = [Items]} = Presence) ->
    #xmlelement{} = exml_query:subelement(Items, <<"destroy">>),
    <<"unavailable">> = exml_query:attr(Presence, <<"type">>).

was_room_destroyed(Query) ->
    <<"result">> = exml_query:attr(Query, <<"type">>).

was_room_created(Stanza = #xmlelement{children = [X]}) ->
    has_status_codes(Stanza, [<<"201">>, <<"110">>]),
    <<"owner">> = exml_query:path(X, [{element, <<"item">>},
                                      {attr, <<"affiliation">>}]),
    <<"moderator">> = exml_query:path(X, [{element, <<"item">>},
                                          {attr, <<"role">>}]).

has_room(JID, #xmlelement{children = [ #xmlelement{children = Rooms} ]}) ->
    %% <iq from='chat.shakespeare.lit'
    %%   id='zb8q41f4'
    %%   to='hag66@shakespeare.lit/pda'
    %%   type='result'>
    %% <query xmlns='http://jabber.org/protocol/disco#items'>
    %%    <item jid='heath@chat.shakespeare.lit'
    %%         name='A Lonely Heath'/>
    %%    <item jid='coven@chat.shakespeare.lit'
    %%         name='A Dark Cave'/>
    %%    <item jid='forres@chat.shakespeare.lit'
    %%         name='The Palace'/>
    %%     <item jid='inverness@chat.shakespeare.lit'
    %%         name='Macbeth&apos;s Castle'/>
    %%   </query>
    %% </iq>

    RoomPred = fun(Item) ->
        exml_query:attr(Item, <<"jid">>) == JID
    end,
    true = lists:any(RoomPred, Rooms).

count_rooms(#xmlelement{children = [ #xmlelement{children = Rooms} ]}, N) ->
    N = length(Rooms).

has_features(#xmlelement{children = [ Query ]}) ->
    %%<iq from='chat.shakespeare.lit'
    %%  id='lx09df27'
    %%  to='hag66@shakespeare.lit/pda'
    %%  type='result'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'>
    %%    <identity
    %%      category='conference'
    %%      name='Shakespearean Chat Service'
    %%      type='text'/>
    %%      <feature var='http://jabber.org/protocol/muc'/>
    %%  </query>
    %%</iq>

    Identity = exml_query:subelement(Query, <<"identity">>),
    <<"conference">> = exml_query:attr(Identity, <<"category">>),
    #xmlelement{name = _Name, attrs = _Attrs, children = _Body} = exml_query:subelement(Query, <<"feature">>).


is_room_locked(Stanza) ->
    escalus_pred:is_presence(Stanza)
    andalso  escalus_pred:is_error(<<"cancel">>, <<"item-not-found">>, Stanza).

generate_rpc_jid({_,User}) ->
  {username, Username} = lists:keyfind(username, 1, User),
  {server, Server} = lists:keyfind(server, 1, User),
  {jid, Username, Server, <<"rpc">>, Username, Server, <<"rpc">>}.
