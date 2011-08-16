<div-async name="item-$(itemId)">
  <itemName />
  <div class="check-uncheck-link" id="item-link-$(itemId)">
    <itemUnchecked>
      <a-async href="$(checkLink)" data-loading-div="#item-link-$(itemId)" class="check-link item-link"> </a-async>
    </itemUnchecked>
    <itemChecked>
      <a-async href="$(uncheckLink)" data-loading-div="#item-link-$(itemId)" class="uncheck-link item-link"> </a-async>
    </itemChecked>
  </div>
</div-async>
