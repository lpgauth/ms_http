

# Module ms_http_cowboy #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#onrequest-1">onrequest/1</a></td><td></td></tr><tr><td valign="top"><a href="#onresponse-4">onresponse/4</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="onrequest-1"></a>

### onrequest/1 ###

<pre><code>
onrequest(Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>) -&gt; <a href="cowboy_req.md#type-req">cowboy_req:req()</a>
</code></pre>
<br />

<a name="onresponse-4"></a>

### onresponse/4 ###

<pre><code>
onresponse(Status::<a href="cowboy.md#type-http_status">cowboy:http_status()</a>, Headers::<a href="cowboy.md#type-http_headers">cowboy:http_headers()</a>, Body::iodata(), Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>) -&gt; <a href="cowboy_req.md#type-req">cowboy_req:req()</a>
</code></pre>
<br />

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; ok
</code></pre>
<br />

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok
</code></pre>
<br />

