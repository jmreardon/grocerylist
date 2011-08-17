<!doctype html>
<html>

<head>
  <meta charset="utf-8">
  <title><pageTitle /> :: Grocery List</title>
  <meta name="description" content="">
  <meta name="author" content="">
  <meta name="HandheldFriendly" content="True">
  <meta name="MobileOptimized" content="320"/>
  <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
  <meta name="apple-mobile-web-app-capable" content="yes" />
  <meta name="apple-mobile-web-app-status-bar-style" content="black">
  <!--<link rel="apple-touch-startup-image" href="img/l/splash.png">-->
  <link rel="stylesheet" href="/css/style.css" type="text/css" media="screen" charset="utf-8" />
  <link rel="stylesheet" href="/css/uniform.aristo.css" type="text/css" media="screen" charset="utf-8" />

  <script src="//ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.js"></script>
  <script src="/js/libs/modernizr-custom.js"></script>
  <script src="/js/jquery.uniform.min.js" type="text/javascript"></script>
  <activate-async/>

  <script type="text/javascript">
    $(function(){ $("select, input").uniform(); });
  </script>
</head>

<body>

  <div id="container" class="clearfix">
    <header class="clearfix">
      <h1><a href="$(routeHome)" title="Home">Grocery List</a></h1>
      <nav>
        <loggedOut>
          <apply template="button">
            <bind tag="title">Login</bind>
            <bind tag="link"><routeLogin /></bind>
          </apply>
          <div class="button-right">
            <apply template="button">
              <bind tag="title">Signup</bind>
              <bind tag="link"><routeSignup /></bind>
            </apply>
          </div>
        </loggedOut>
        <loggedIn>
          <apply template="button">
            <bind tag="title">List</bind>
            <bind tag="link"><routeList /></bind>
          </apply>
          <apply template="button">
            <bind tag="title">Items</bind>
            <bind tag="link"><routeItems /></bind>
          </apply>
          <div class="button-right">
            <apply template="button">
              <bind tag="title">Logout</bind>
              <bind tag="link"><routeLogout /></bind>
            </apply>
          </div>
        </loggedIn>
      </nav>
    </header>
    <div id="main" role="main">
      <h2><pageTitle /></h2>
      <content />
    </div>
    <div id="sidebar">
      <sidebar />
    </div>
    
    
    <footer>

    </footer>
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
