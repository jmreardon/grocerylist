<apply template="master">
  <bind tag="navigation">
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
        <div class="button-right">
          <apply template="button">
            <bind tag="title"><img src="/img/account.png" width="16px" height="16px" /></bind>
            <bind tag="titleMsg">Account</bind>
            <bind tag="link"><routeAccount /></bind>
          </apply>
        </div>
      </loggedIn>
    </nav>
  </bind>
  <div id="main" role="main">
    <h2><pageTitle /></h2>
    <content />
  </div>
  <div id="sidebar">
    <sidebar />
    </div>
  
  
  <footer>

  </footer>
</apply>
