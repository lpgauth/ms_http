

# Module ms_http_json #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-ms_req">ms_req()</a> ###


<pre><code>
ms_req() = #ms_req{timestamp = <a href="erlang.md#type-timestamp">erlang:timestamp()</a> | undefined, method = binary() | undefined, path = binary() | undefined, found = boolean(), key = binary() | undefined, value = binary() | undefined}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#req-1">req/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="req-1"></a>

### req/1 ###

<pre><code>
req(Req::<a href="#type-ms_req">ms_req()</a>) -&gt; iolist()
</code></pre>
<br />

