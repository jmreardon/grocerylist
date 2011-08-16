<apply template="default">
  <bind tag="pageTitle">Login</bind>
  <loggedIn>You are already logged in.</loggedIn>
  <loggedOut>
    <loginRequired>
      <div class="login_required">
        You must be logged in to view that page. Please log in and try again.
      </div>
    </loginRequired>
    <loginForm>
      <form enctype="$(encType)" method="POST" action="$(route)">
        <rForm />
         <div class="center-button"><input type="submit" value="Login" /></div>
      </form>
    </loginForm>
  </loggedOut>
</apply>
