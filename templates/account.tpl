<apply template="default">
  <bind tag="pageTitle">Account</bind>
  <h3>Update Account Information</h3>
  <accountForm>
    <form enctype="$(encType)" method="POST" action="$(route)">
      <div class="clearfix form"><rForm /></div>
      <input type="submit" name="updateAccount" value="Update" />
    </form>
  </accountForm>

  <bind tag="sidebar">
    <h3>Change Password</h3>
    <changePasswordForm>
      <form enctype="$(encType)" method="POST" action="$(route)">
        <rForm />
        <input type="submit" name="changePassword" value="Change Password" />
      </form>
    </changePasswordForm>
  </bind>
</apply>
