<apply template="default">
   <bind tag="pageTitle">Signup</bind>
   <loggedIn>You are already logged in.</loggedIn>
   <loggedOut>
     <signupForm>
       <form enctype="$(encType)" method="POST" action="$(route)">
         <div class="clearfix form"><rForm /></div>
         <div class="center-button"><input type="submit" value="Signup" /></div>
       </form>
     </signupForm>
   </loggedOut>
</apply>
