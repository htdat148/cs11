%%%-------------------------------------------------------------------
%%% @author dathuynh
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2024 16:51
%%%-------------------------------------------------------------------
-module(rss_parse).
-author("dathuynh").

-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([is_rss2_feed/1,
        get_feed_items/1,
        get_item_time/1,
        compare_feed_items/2]).

-export([extract_xml/1]).


-spec is_rss2_feed(#xmlElement{} | {#xmlElement{}, list()}) -> boolean().
is_rss2_feed(#xmlElement{}=Element) -> is_rss2_feed({Element, []});
is_rss2_feed({#xmlElement{} = Element, Other}) when is_list(Other) ->
    SearchFun = fun(#xmlAttribute{name = 'version', value = "2.0"}) -> 'true';
                    (_) -> 'false'
                end,
    case lists:search(SearchFun, Element#xmlElement.attributes) of
        'false' -> 'false';
        {'value', _} -> 'true'
    end;
is_rss2_feed(_Invalid) -> 'invalid_format'.

-spec get_feed_items(#xmlElement{}) -> list().
get_feed_items(#xmlElement{} = Element) ->
        case is_rss2_feed(Element) of
            'true' ->
                get_feed_items_helper_v1(Element);
            'false' ->
                'invalid_version'
        end.

-spec get_feed_items_helper_v1(#xmlElement{}) -> list().
get_feed_items_helper_v1(#xmlElement{content = Content}) ->
    Fun = fun(#xmlElement{name = 'item'} = Element, Acc) -> Acc ++ [Element];
             (#xmlElement{} = Element, Acc) -> get_feed_items_helper_v1(Element) ++ Acc;
             (_, Acc) -> Acc
          end,
    lists:foldl(Fun,
                [],
                Content).

%%<pubDate> sub-element of <item>
%%<pubDate>Sun, 19 May 2002 15:21:36 GMT</pubDate>
-spec get_item_time(#xmlElement{}) -> integer() | 'bad_date'.
get_item_time(#xmlElement{} = Element) ->
    PubDate = get_element_from_item_feed('pubDate', Element),
    get_item_time_helper(PubDate).

get_item_time_helper(#xmlElement{name = 'pubDate', content = []}) -> 'bad_date';
get_item_time_helper(#xmlElement{name = 'pubDate', content = [#xmlText{value = Value}]}) ->
    case httpd_util:convert_request_date(Value) of
        'bad_date' -> 'bad_date';
        Datetime ->
            calendar:datetime_to_gregorian_seconds(Datetime)
    end.

%% the idea is a bit different from the post's suggestions
%% check title then check link/guid
-spec compare_feed_items(#xmlElement{}, #xmlElement{}) -> 'same' | 'different' | 'updated'.
compare_feed_items(OldItem = #xmlElement{name = 'item'}, NewItem = #xmlElement{name = 'item'}) ->
    compare_feed_items_helper(extract_xml(OldItem), extract_xml(NewItem)).

-spec get_search_fun_for_item_element('pubDate' | 'guid' | 'title' | 'link') -> fun() | 'undefined'.
get_search_fun_for_item_element(ElementName) ->
    case lists:member(ElementName, ['guid', 'title', 'link', 'pubDate']) of
        'true' ->
            fun (#xmlElement{name = Name}) when Name == ElementName -> 'true';
                (_Other) -> 'false'
            end;
        'false' -> 'undefined'
    end.

-spec has_element_in_item_feed('pubDate' | 'guid' | 'title' | 'link', #xmlElement{}) -> boolean().
has_element_in_item_feed(ElementName, #xmlElement{content = Content, name = 'item'}) ->
    Fun = get_search_fun_for_item_element(ElementName),
    case lists:search(Fun, Content) of
        {'value', _} -> 'true';
        'false' -> 'false'
    end.

-spec get_element_from_item_feed('pubDate' | 'guid' | 'title' | 'link', #xmlElement{}) -> 'undefined' | #xmlElement{}.
get_element_from_item_feed(ElementName, #xmlElement{content = Content, name = 'item'}) ->
    Fun = get_search_fun_for_item_element(ElementName),
     case lists:search(Fun, Content) of
         {'value', FoundElement} -> FoundElement;
         'false' -> 'undefined'
     end.


compare_feed_items_helper(OldItem = #xmlElement{name = 'item'}, OldItem) -> 'same';
compare_feed_items_helper(OldItem = #xmlElement{name = 'item'}, NewItem = #xmlElement{name = 'item'}) ->
    case maybe_compare_title(OldItem, NewItem) of
        'different' -> 'different';
        'maybe_updated' ->
            check_link_and_guild(OldItem, NewItem)
    end.

check_link_and_guild(OldItem = #xmlElement{name = 'item'}, NewItem = #xmlElement{name = 'item'}) ->
    HasGuid = has_element_in_item_feed('guid', OldItem) andalso has_element_in_item_feed('guid', NewItem),
    HasLink = has_element_in_item_feed('link', OldItem) andalso has_element_in_item_feed('link', NewItem),
    
    case {HasGuid, HasLink} of
        {'true', 'true'} ->
            'updated';
        {'true', _} ->
            compare_element_content('guid', OldItem, NewItem);
        {_, 'true'} ->
            compare_element_content('link', OldItem, NewItem);
        _ ->
            'different'
    end.

maybe_compare_title(OldItem, NewItem) ->
    OldElement = get_element_from_item_feed('title', OldItem),
    [#xmlText{value = OldValue}] = OldElement#xmlElement.content,
    NewElement = get_element_from_item_feed('title', NewItem),
    [#xmlText{value = NewValue}] = NewElement#xmlElement.content,
    case OldValue == NewValue of
        'true' -> 'maybe_updated';
        'false' -> 'different'
    end.

-spec compare_element_content('guid' | 'title' | 'link', #xmlElement{}, #xmlElement{}) -> boolean().
compare_element_content(Name, OldItem = #xmlElement{name = 'item'}, NewItem = #xmlElement{name = 'item'}) ->
    OldElement = get_element_from_item_feed(Name, OldItem),
    [#xmlText{value = OldValue}] = OldElement#xmlElement.content,
    NewElement = get_element_from_item_feed(Name, NewItem),
    [#xmlText{value = NewValue}] = NewElement#xmlElement.content,
    case OldValue =/= NewValue of
        'true' -> 'updated';
        'false' -> 'different'
    end.


% @private
% @doc Given an XML element of some kind, this helper function will go through
%      and remove any details about other XML elements, for example in the
%      "parents" or "pos" fields.
%
% @spec extract_xml(Node::xmlAny()) -> xmlAny()
%
extract_xml(Elem = #xmlElement{}) ->
    Elem#xmlElement{parents=[],
                    pos=0,
                    content=lists:map(fun extract_xml_helper/1, Elem#xmlElement.content),
                    attributes=lists:map(fun extract_xml_helper/1, Elem#xmlElement.attributes)}.

extract_xml_helper(Attr = #xmlAttribute{}) ->
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml_helper(Text = #xmlText{}) ->
    Text#xmlText{parents=[], pos=0};
extract_xml_helper(Comment = #xmlComment{}) ->
    Comment#xmlComment{parents=[], pos=0};
extract_xml_helper(Element = #xmlElement{}) ->
    extract_xml(Element);
extract_xml_helper(Other) ->
    Other.