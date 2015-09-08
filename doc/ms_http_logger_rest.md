

# Module ms_http_logger_rest #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#allowed_methods-2">allowed_methods/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_types_accepted-2">content_types_accepted/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_types_provided-2">content_types_provided/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#put-2">put/2</a></td><td></td></tr><tr><td valign="top"><a href="#resource_exists-2">resource_exists/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="allowed_methods-2"></a>

### allowed_methods/2 ###

<pre><code>
allowed_methods(Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>, State::term()) -&gt; {[binary()], <a href="cowboy_req.md#type-req">cowboy_req:req()</a>, term()}
</code></pre>
<br />

<a name="content_types_accepted-2"></a>

### content_types_accepted/2 ###

<pre><code>
content_types_accepted(Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>, State::term()) -&gt; {[{binary(), atom()}], <a href="cowboy_req.md#type-req">cowboy_req:req()</a>, term()}
</code></pre>
<br />

<a name="content_types_provided-2"></a>

### content_types_provided/2 ###

<pre><code>
content_types_provided(Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>, State::term()) -&gt; {[{binary(), atom()}], <a href="cowboy_req.md#type-req">cowboy_req:req()</a>, term()}
</code></pre>
<br />

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(Transport::{tcp, http}, Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>, Opts::[]) -&gt; {upgrade, protocol, cowboy_rest}
</code></pre>
<br />

<a name="put-2"></a>

### put/2 ###

<pre><code>
put(Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>, Logger::term()) -&gt; {boolean(), <a href="cowboy_req.md#type-req">cowboy_req:req()</a>, term()}
</code></pre>
<br />

<a name="resource_exists-2"></a>

### resource_exists/2 ###

<pre><code>
resource_exists(Req::<a href="cowboy_req.md#type-req">cowboy_req:req()</a>, State::term()) -&gt; {boolean(), <a href="cowboy_req.md#type-req">cowboy_req:req()</a>, term()}
</code></pre>
<br />

