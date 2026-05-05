%% vim: ts=4 sw=4 et
-module (demos_quickform).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).

main() -> #template { file=common:template_location("demos433.html") }.

title() -> "Quick Forms".

headline() -> "Quick Forms".

left() ->
    [
    <<"
        Nitrogen's <code>quickform{}</code> element lets you build forms very quickly
        using an abbreviated syntax. After you've made a bunch of forms, you
        can find the basic form functionality to be a little verbose. Quick
        forms solves that.
    ">>, 
    linecount:render() 
].

gift_types() ->
    [
        {flowers, "Flowers"},
        {fruit_basket, "Fruit Basket"},
        {singing_telegram, "Singing Telegram"}
    ].

form_fields() ->
    [
        {to_name, "Recipient's Name"},
        {from_name, "Your Name"},
        {gift_type, "Gift Type", {dropdown, gift_types()}},
        {ship_date, "Date you would like the gift shipped", date, [{placeholder, "Today"}]},
        {note, "What message would you like enclosed with your gift", textarea, [{columns, 20}, {rows, 5}]}
    ].


middle() -> 
    [
        #h2{text="Sending a Gift (blank form)"},
        #quickform{id=blank_form, fields=form_fields()},
        #button{text="Submit", vessel=blank_form, postback=submit}
    ].


right() -> 
    %% Tomorrow in ISO format (YYYY-MM-DD)
    Tomorrow = qdate:to_string("Y-m-d", qdate:add_days(1)),
    Data = #{
        to_name=>"Jean Grey",
        from_name=>"Logan",
        gift_type=>flowers,
        ship_date=>Tomorrow,
        note=>"Hey hey you you I don't like your boyfriend"
    },

    [
        #h2{text="Sending a Gift (pre-filled)"},
        #quickform{id=prefilled, fields=form_fields(), data=Data},
        #button{text="Submit", vessel=prefilled, postback=submit}
    ].

event(submit) ->
    FieldsToQuery = [element(1, F) || F <- form_fields()],
    Results = wf:q_map(FieldsToQuery),
    ResultText = wf:f("~p", [Results]),
    wf:wire(#modal{
        title_text="Submitted Data",
        body=[
            #pre{text=ResultText}
        ]
    }).

