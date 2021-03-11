$(document).on("click", ".shinymeta-output-code button", function() {
  var id = $(this).parents(".shinymeta-output-code").find(".shiny-bound-output").attr("id");
  // Invalidate `input$id_output_code` whenever the button of
  // shinymeta::outputCodeButton(plotOutput("id")) is pressed
  if (id) {
    Shiny.setInputValue(id + "_output_code", 'true', {priority: 'event'});
  }
});
