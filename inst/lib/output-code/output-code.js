$(function () {
  // Invalidate `input$id_output_code` whenever the button of
  // shinymeta::outputCodeButton(plotOutput("id")) is pressed
  $(".shinymeta-output-code button").click(function() {
    var id = $(this).parents(".shinymeta-output-code").find(".shiny-bound-output").attr("id");
    if (id) {
      Shiny.setInputValue(id + "_output_code", 'true', {priority: 'event'});
    }
  });
});
