<!doctype html>
<html>

<head>
  <meta charset="utf-8">
  <title><pageTitle /> :: Grocery List</title>
  <meta name="description" content="">
  <meta name="author" content="">
  <meta name="HandheldFriendly" content="True">
  <meta name="MobileOptimized" content="320"/>
  <meta name="viewport" content="width=320px, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <meta name="apple-mobile-web-app-status-bar-style" content="black">
  <!--<link rel="apple-touch-startup-image" href="img/l/splash.png">-->
  <link rel="stylesheet" href="/css/style.css" type="text/css" media="screen" charset="utf-8" />
  <link rel="stylesheet" href="/css/uniform.aristo.css" type="text/css" media="screen" charset="utf-8" />

  <script src="//ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.js"></script>
  <script src="/js/libs/modernizr-custom.js"></script>
  <script src="/js/jquery.uniform.min.js" type="text/javascript"></script>
  <script src="/js/mylibs/helper.js" type="text/javascript"></script>
  <activate-async/>

  <script type="text/javascript">
    $(function(){ $("select, input").uniform(); });
  </script>
</head>

<body>

  <div id="container" class="clearfix">
    <header class="clearfix">
      <h1><a href="$(routeHome)" title="Home">Grocery List</a></h1>
      <navigation />
    </header>
      <flashMessages>
        <messages>
          <div id="flash-messages">
            <message><div class="panel flash"><msg /></div></message>
          </div>
        </messages>
      </flashMessages>
    <content />

  </div>
  <script>
    MBP.scaleFix();
    yepnope({
      test : Modernizr.mq('(min-width)'),
      nope : ['js/libs/respond.min.js']
    });
  </script>
  
  <script>
    var _gaq=[["_setAccount","UA-XXXXX-X"],["_trackPageview"]];
    (function(d,t){var g=d.createElement(t),s=d.getElementsByTagName(t)[0];g.async=1;
    g.src=("https:"==location.protocol?"//ssl":"//www")+".google-analytics.com/ga.js";
    s.parentNode.insertBefore(g,s)}(document,"script"));
  </script>

</body>
</html>
