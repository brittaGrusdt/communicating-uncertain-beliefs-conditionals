// In this file you initialize and configure your experiment using magpieInit

$("document")
  .ready(function () {
    // prevent scrolling when space is pressed
    window.onkeydown = function (e) {
      if (e.keyCode === 32 && e.target === document.body) {
        e.preventDefault();
      }
    };
    //add color vision views to alternating test views
    var views_test = _.flatten(_.zip(multi_slider_views, fridge_views))
    _.map(color_vision_views, function(view, idx){
      views_test.splice(4+idx*5, 0, view);
    });
    // calls magpieInit
    // in debug mode this returns the magpie-object, which you can access in the console of your browser
    // e.g. >> window.magpie_monitor or window.magpie_monitor.findNextView()
    // in all other modes null will be returned
    window.magpie_monitor = magpieInit({
      // You have to specify all views you want to use in this experiment and the order of them
      //*alternating trials of Exp1 (prior elicitation) and Exp2 (production)* in views_test!//
      views_seq: [
      intro,
      instructions_general,
      // * Training with sliders* //
      instructions_train_sliders_choices,
      attention_check_view,
      slider_choice_training,
      instructions_train_slider_ratings,
      instructions_train_sliders_procedure,
      animation_view_sliders,
      // Testing //
      instructions_fridge,
      fridge_train,
      instructions_fridge_reminder
    ].concat(views_test).concat([post_test, thanks]),
      // Here, you can specify all information for the deployment
      deploy: {
        experimentID: "54",
        serverAppURL: "https://mcmpact.ikw.uni-osnabrueck.de/magpie/api/submit_experiment/",
        // Possible deployment methods are:
        // "debug" and "directLink"
        // As well as "MTurk", "MTurkSandbox" and "Prolific"
        deployMethod: "Prolific",
        contact_email: "britta.grusdt@uni-osnabrueck.de",
        prolificURL: "https://app.prolific.co/submissions/complete?cc=36C6918E"
      },
      // Here, you can specify how the progress bar should look like
      progress_bar: {
        // list the view-names of the views for which you want a progress bar
        // multiple_slider.name,
        in: [slider_choice_training.name, animation_view_sliders.name].concat(
          _.map(views_test, 'name')
        ),
      // Possible styles are "default", "separate" and "chunks"
        style: "default",
        width: 100
      }
    });
  });
