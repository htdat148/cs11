%%%-------------------------------------------------------------------
%%% @author dathuynh
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2024 20:19
%%%-------------------------------------------------------------------
-module(rss_parse_test).
-author("dathuynh").

-import(rss_parse, [is_rss2_feed/1,
        get_feed_items/1,
        get_item_time/1,
        compare_feed_items/2,
        extract_xml/1]).
%% API
-export([sample_rss_feed/0,
        rss_version_test/0,
        get_feed_items_test/0,
        get_item_time_test/0,
        extract_xml_test/0,
        compare_feed_items_test/0]).

-include_lib("xmerl/include/xmerl.hrl").

sample_rss_feed() ->
    "<?xml version=\"2.0\" encoding=\"UTF-8\"?>
    <rss version=\"2.0\">
    <channel>
      <title>W3Schools Home Page</title>
      <link>https://www.w3schools.com</link>
      <description>Free web building tutorials</description>
      <item>
        <title>RSS Tutorial</title>
        <link>https://www.w3schools.com/xml/xml_rss.asp</link>
        <description>New RSS tutorial on W3Schools</description>
        <pubDate>Sun, 01 Sep 2024 23:35:18 +0700</pubDate>
      </item>
      <item>
        <title>XML Tutorial</title>
        <link>https://www.w3schools.com/xml</link>
        <description>New XML tutorial on W3Schools</description>
        <pubDate>Wed, 02 Oct 2002 08:00:00 EST</pubDate>
      </item>
    </channel>
    </rss>".

rss_version_test() ->
    Parsed_1 = xmerl_scan:string(
            "<?xml version=\"2.0\" encoding=\"UTF-8\"?>
            <rss version=\"2.0\">
                Tove
            </rss>"),
    'true' = is_rss2_feed(Parsed_1),

    Parsed_2 = xmerl_scan:string(
            "<?xml version=\"2.0\" encoding=\"UTF-8\"?>
            <note>
                Tove
            </note>"),
    'false' = is_rss2_feed(Parsed_2),

    Parsed_3 = xmerl_scan:string(
            "<?xml version=\"2.0\" encoding=\"UTF-8\"?>
            <rss version=\"1.0\">
                Tove
            </rss>"),
    'false' = is_rss2_feed(Parsed_3),

    'ok'.

get_feed_items_test() ->
    RSS = sample_rss_feed(),
    {Element, _} = xmerl_scan:string(RSS),
    2 = length(get_feed_items(Element)),
    ok.

get_item_time_test() ->
    Rss =
"<rss version=\"2.0\">
    <channel>
      <item>
        <title>RSS Tutorial</title>
        <link>https://www.w3schools.com/xml/xml_rss.asp</link>
        <description>New RSS tutorial on W3Schools</description>
        <pubDate>Sun, 01 Sep 2024 23:35:18 +0700</pubDate>
      </item>
      <item>
        <title>XML Tutorial</title>
        <link>https://www.w3schools.com/xml</link>
        <description>New XML tutorial on W3Schools</description>
        <pubDate>Wed, 02 Oct 2002 08:00:00 EST</pubDate>
      </item>
    </channel>
</rss>",
    {Element, []} = xmerl_scan:string(Rss),
    [Item1, Item2] = get_feed_items(Element),
    Seconds1 = calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date("Sun, 01 Sep 2024 23:35:18 +0700")),
    Seconds1 = get_item_time(Item1),

    Seconds2 = calendar:datetime_to_gregorian_seconds(httpd_util:convert_request_date("Wed, 02 Oct 2002 08:00:00 EST")),
    Seconds2 = get_item_time(Item2),
    'ok'.

extract_xml_test() ->
    {Element, []} = xmerl_scan:string("<title>XML Tutorial</title>"),
    extract_xml(Element),

    {Element_1, []} = xmerl_scan:string("<item>
        <title>RSS Tutorial</title>
        <link>https://www.w3schools.com/xml/xml_rss.asp</link>
        <description>New RSS tutorial on W3Schools</description>
        <pubDate>Sun, 01 Sep 2024 23:35:18 +0700</pubDate>
      </item>"),
    [Element_1, extract_xml(Element_1)].


compare_feed_items_test() ->
    {Element_1, []} = xmerl_scan:string(
        "<item>
            <title>RSS Tutorial</title>
            <link>https://www.w3schools.com/xml/xml_rss.asp</link>
            <description>New RSS tutorial on W3Schools</description>
            <pubDate>Sun, 01 Sep 2024 23:35:18 +0700</pubDate>
        </item>"),

    {Element_2, []} = xmerl_scan:string(
        "<item>
            <title>XML Tutorial</title>
            <link>https://www.w3schools.com/xml</link>
            <description>New XML tutorial on W3Schools</description>
            <pubDate>Wed, 02 Oct 2002 08:00:00 EST</pubDate>
        </item>"),

    {Element_3, []} = xmerl_scan:string(
        "<item>
            <title>RSS Tutorial</title>
            <link>https://www.w3schools.com/xml/xml_rss_v1.asp</link>
            <description>New RSS tutorial on W3Schools</description>
            <pubDate>Sat, 04 Sep 2024 23:35:18 +0700</pubDate>
        </item>"),

    'different' = compare_feed_items(Element_1, Element_2),

    'updated' = compare_feed_items(Element_1, Element_3),

    ok.