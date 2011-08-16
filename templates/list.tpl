<apply template="default">
  <bind tag="pageTitle">List</bind>
  <bind tag="sidebar">
    <itemList>
      <hasItems>
        <apply template="tag-bar" />
        <activeTags>
          <apply template="center-button">
            <bind tag="title">Clear All Filters</bind>
            <bind tag="link"><routeList /></bind>
          </apply>
        </activeTags>
        <noActiveTags>
          <div class="center-button">
            <div class="button disabled">
              <span>Clear Purchases</span>
            </div>
          </div>
        </noActiveTags>
      </hasItems>
    </itemList>
  </bind>
  <itemList>
    <noItems><div class="empty-message">Your list is empty. You should add some items.</div></noItems>
    <hasItems>
      <div class="panel">
        <ul id="item-list">
          <item><li class="$(rowClass)"><apply template="list-item" /></li></item>
        </ul>
      </div>
      <apply template="center-button">
        <bind tag="title">Clear Purchases</bind>
        <bind tag="link"><clearRoute /></bind>
      </apply>
    </hasItems>
  </itemList>
</apply>
