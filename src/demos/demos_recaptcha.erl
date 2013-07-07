%% vim: ts=4 sw=4 et
-module (demos_recaptcha).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file="./templates/demos46.html" }.

title() -> "Recaptcha".

headline() -> "Recaptcha".

left() -> 
    [
        "
        <p> 
        You can minimize the effects of automated spam by employing the use of
        a CAPTCHA. While Nitrogen's postback mechanism itself helps to minimize
        automated POST-based spam, more aggressive Javascript enabled bots or 
        bots written targetting Nitrogen-based sites.
        <p>
        For this, you can integrate a Recaptcha element into your application's
        postbacks. You just need to add a #recaptcha{} element and a
        recaptcha_event/2 function to your page, and you're ready to go.
        <p>
        ", 
        linecount:render() 
    ].

right() -> 
    wf:wire(recaptcha_button, name, #validate{ validators=[
        #is_required{text="Name is required"}
    ]}),
    [
        % Set up the form...
        #label {text="Enter your name"},
        #textbox{ id=name, placeholder="Bruce Wayne", text=""},
        #br{},
        #recaptcha{ id=my_recaptcha, button_id=recaptcha_button, button_label="Tell us your human name", tag=human_verifier}
    ].

recaptcha_event(human_verifier, error) ->
    wf:wire(#alert{text="WAIT JUST A DARN MINUTE, THAT'S NOT RIGHT! TRY AGAIN, SILLY ROBOT!"}),
    ok;
recaptcha_event(human_verifier, ok) ->
    wf:wire(#alert{text="Congrats, you've verified that you are human"}),
    ok.
