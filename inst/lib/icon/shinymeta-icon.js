$(function () {
  $(".shinymeta-icon").click(function() {
    var $el = $(this);
    var val = $el.data('val') || 0;
    $el.data('val', val + 1);
    var output_id = $el.parent().find(".shiny-bound-output").attr("id");
    var event_id = output_id + "_shinymeta_icon";
    Shiny.onInputChange(event_id, $el.data('val'));
  });
});


