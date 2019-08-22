$(function () {
  $(".shinymeta-output-code button").click(function() {
    var $el = $(this);
    var val = $el.data('val') || 0;
    $el.data('val', val + 1);
    var output_id = $(".shinymeta-output-code").find(".shiny-bound-output").attr("id");
    var event_id = output_id + "_output_code";
    Shiny.onInputChange(event_id, $el.data('val'));
  });
});


