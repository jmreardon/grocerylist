<div-async name="item-$(itemId)">
  <span class="item-name"><itemName /></span>
  <span class="item-tags"><itemTags /></span>
  <div class="add-remove-link" id="item-link-$(itemId)">
    <itemOnList>
      <a-async href="$(removeLink)" data-loading-div="#item-link-$(itemId)" class="list-remove-link item-link"> </a-async>
    </itemOnList>
    <itemOffList>
      <a-async href="$(addLink)" data-loading-div="#item-link-$(itemId)" class="list-add-link item-link"> </a-async>
    </itemOffList>
  </div>
</div-async>
