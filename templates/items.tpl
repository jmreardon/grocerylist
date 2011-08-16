<apply template="default">
  <bind tag="pageTitle">Items</bind>
  <bind tag="sidebar">
    <itemList>
        <apply template="tag-bar" />
        <h3>Needed Within</h3>
        <div class="panel">
          <ul class="tag-list">
            <weekChoice>
              <choiceOn>
                <li class="choice-on"><choiceName /></li>
              </choiceOn>
              <choiceOff>
                <li class="choice-off"><a href="$(choiceLink)"><choiceName /></a></li>
              </choiceOff>
            </weekChoice>
          </ul>
        </div>
        <filtersOn>
          <apply template="center-button">
            <bind tag="title">Clear All Filters</bind>
            <bind tag="link"><routeItems /></bind>
          </apply>
        </filtersOn>
        <filtersOff>
          <div class="center-button">
            <div class="button disabled">
              <span>Clear Purchases</span>
            </div>
          </div>
        </filtersOff>
    </itemList>
  </bind>
  <addItemForm>
    <form enctype="$(encType)" method="POST" action="$(route)">
      <rForm />
      <input type="submit" name="submit" value="Add" />
    </form>
  </addItemForm>
  <itemList>
    <noItems>Your list is empty. You should add some items.</noItems>
    <hasItems>
      <div class="panel">
        <ul id="item-list">
          <item><li class="$(rowClass)"><apply template="item" /></li></item>
        </ul>
      </div>
    </hasItems>
  </itemList>
</apply>
