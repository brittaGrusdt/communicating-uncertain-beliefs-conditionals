// In this file you can instantiate your views
// We here first instantiate wrapping views, then the trial views

/** Wrapping views below

* Obligatory properties

    * trials: int - the number of trials this view will appear
    * name: string

*Optional properties
    * buttonText: string - the text on the button (default: 'next')
    * text: string - the text to be displayed in this view
    * title: string - the title of this view

    * More about the properties and functions of the wrapping views - https://magpie-ea.github.io/magpie-docs/01_designing_experiments/01_template_views/#wrapping-views

*/

// Every experiment should start with an intro view. Here you can welcome your participants and tell them what the experiment is about
const intro = magpieViews.view_generator("intro", {
  trials: 1,
  name: "intro",
  // If you use JavaScripts Template String `I am a Template String`, you can use HTML <></> and javascript ${} inside
  text: `Thank you for your participation in our study!
         Your anonymous data makes an important contribution to our understanding of human language use.
          <br />
          <br />
          Legal information:
          By answering the following questions, you are participating in a study
          being performed by scientists from the University of Osnabrueck.
          <br />
          <br />
          You must be at least 18 years old to participate.
          <br />
          <br />
          Your participation in this research is voluntary.
          You may decline to answer any or all of the following questions.
          You may decline further participation, at any time, without adverse consequences.
          <br />
          <br />
          Your anonymity is assured; the researchers who have requested your
          participation will not receive any personal information about you.
          `,
  buttonText: "begin the experiment"
});

// const instructions_color_vision = magpieViews.view_generator("instructions", {
//   trials: 1,
//   name: "instructions_color_vision",
//   title: "General Instructions",
//   text: `In this experiment you are shown pictures of different arrangements of
//           blocks.
//          <br />
//          <br />
//          The experiment consists of two phases, a <b>training</b> and a <b>testing</b> phase.
//          In total, you will need about <b>15 minutes</b> to finish it.
//          <br/>
//          <br/>
//          We will start the training phase with a few simple questions concerning
//          the color of the blocks that we will show you in the experiment.
//          Please click on <b>CONITINUE</b> to proceed with the questions.`,
//   buttonText: "continue"
// });

const instructions_general = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_general",
  title: "General Instructions",
  text: `In this experiment you are shown different arrangements of blocks.
         <br />
         <br />
         The experiment consists of two phases, a <b>training</b> and a <b>testing</b> phase.
         In total, you will need about <b>25-30 minutes</b> to finish it.
         <br/>
         <br/>
         <b>Please note</b>:
         <br/>
         We recommend to use <b>Full Screen Mode</b> throughout the experiment
         (usually switched on/off with F11). However, depending on your device and browser, you may still need to scroll down to see all buttons.`,
  buttonText: "Go to Training instructions"
});


// For most tasks, you need instructions views
const instructions_train = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train",
  title: "Instructions Training",
  text: `For each presented scene you will be asked to <b>indicate whether you
  think that most likely both, none or respectively only one of two colored
  blocks will fall</b> (there may be other blocks with different colors than the
  two colored blocks asked for) by clicking on the button with the respective icon.
  <br />
  A falling block is represented by a skewed rectangle and a block that
  does not fall is represented by a rectangle with a line below.
  Here is an exemplary icon for the event:
  <br/>
  <i>The <b>green</b> block <b>falls</b>, but the <b>yellow</b> block
  <b>does not fall</b></i>.
  <br/>
  <br/>
  <img src='stimuli/img/icons/green.png'/>
  <img src='stimuli/img/icons/not-yellow.png' />
  <br/>
  <br/>
  <b>Please note</b>:
  <br/>
  A block is considered to <b>fall</b> as soon as it <b>drops off a platform</b> or
  <b>off another block</b> &mdash; that is, a block does not
  necessarily need to fall to the ground in order to count as falling.
  <br/>
  The colored blocks represent common toy blocks <b>without</b> any special
  properties and the different colors do <b>not have</b> any meaning.
     <br />
     <br />
  After you selected one of the four buttons (which will make their
  border turn green), click on <b>RUN</b> to see what happens.
  The button of the event that occurred will turn light green.
  <br/>
  After that, you will be able to proceed with the next trial by clicking on <b>NEXT</b>.`,
  buttonText: "CONTINUE"
});

const instructions_train_sliders_choices = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train_sliders_choices",
  title: "Instructions Training 1",
  text: `In the later parts of this experiment, you will see pictures showing colored blocks arranged in certain ways.  For each presented picture you are asked to indicate, using adjustable sliders, how likely it is that certain blocks will fall.
  <br/>
  <br/>
  Each event is represented by a different icon:
  <br />
  A falling block is represented by a skewed rectangle and a block that
  does not fall is represented by a rectangle with a line below, e.g.
  the following icon represents the event:
  <br/>
  <br/>
  '<i>The <b>green</b> block <b>falls</b>, but the <b>yellow</b> block
  <b>does not fall</b></i>'.
  <br/>
  <br/>
  <img src='stimuli/img/icons/green.png'/>
  <img src='stimuli/img/icons/not-yellow.png' />
  <br/>
  <br/>
  Before we start with the actual training trials, there will be <b>10 trials and 3 questions</b>
  which aim to <b>familiarize you with how to use the sliders</b> to indicate your beliefs.
  `,
  buttonText: "start 1st part training"
});

const instructions_train_slider_ratings = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train_sliders",
  title: "Training 2",
  text: `Wonderful &mdash; we will now proceed with the second part of the training phase
  where you should familiarize yourself with the pictures we will use in the
  later part of the experiment.
  <br/>
  You will see pictures showing various arrangements of colored blocks. For each
  presented arrangement we would like to know <b>how likely</b> you think it is for certain
  <b>blocks to fall</b> (<b>off a platform or off another block &mdash;
    not necessarily to the ground</b>).
  <br/><br/>
  The blocks represent common toy blocks <b>without</b> any special properties
  and the different colors do <b>not have</b> any meaning.
  Your task is to indicate how likely you think a certain event happens by
  <b>adjusting the corresponding sliders</b> (left: <i>impossible event</i>,
    right: <i>certain event</i>).
  <br/>
  <br/>
  After you have adjusted the sliders in such a way as to express your genuine
  belief about how likely each possible outcome is, click on <b>RUN</b> to start
  the animation and see what happens.
  <br/>
  <b>After a few seconds</b>, when the animation stops, you will get <b>feedback</b>
  about what actually happened.
  Afterwards, click on <b>NEXT SCENE</b> to proceed with the next trial.
  <br/>
  <br/>
  Before we start, we'd like to give you some more information about how to indicate your
  beliefs by adjusting the sliders.
  `,
  buttonText: "continue"
})

const instructions_train_sliders_procedure = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train_sliders_procedure",
  title: "Slider Ratings",
  text: `Remember the sliders you just saw in the first part of the training phase:
  <br/><br/>
  <img src="stimuli/img/instructions/slider.png" class="instruction_img"><br/><br/>
  <b>1</b>. Each slider indicates the <b>plausibility</b> that a given event will happen.
  Eventually, we want them to indicate the <b>percent chance</b> that the given event will happen, therefore all possible outcomes will have to sum to 100. You don't have to worry about this: <b>after you have moved all four sliders</b>, your ratings will be <b>readjusted automatically</b> in order to sum up to 100 &mdash; but they will <b>conserve their relative position to each other</b>.
  <br/>
  <br/>
  <b>2</b>. If you are not happy with the automatically readjusted values, <b>please correct them such that they represent your genuine beliefs</b>.
  To make it easier for you, you will additionally see two visualizations next to the sliders (piecharts to be precise) that indicate the <b>derived probability</b> that you assigned <b>in total</b> to the <b>event that the two colored blocks fall respectively</b>. In the following example  picture, the left piechart for instance indicates a quite large belief of the blue block to fall (in the experiment, <b>hover over piechart</b> to see the exact percentages) and a still larger belief of the green block to fall.
  <br/>
  <br/>
  <img src="stimuli/img/instructions/piecharts.png" class="instruction_img_small"><br/>
  <br/>
  <br/>
  <b>3</b>. Note that changing the position of a slider <b>after they were automatically readjusted to sum up to 100</b>, will have an effect on <b>ALL</b> other sliders as well. This is something you should get used to during this training phase.
  In particular, what happens is that, <b>when you move one slider in either direction, all other sliders will conserve their relative position to each other</b>. For example, if slider A indicated a percent chance that was two times higher than that of slider B, then adjusting a third slider C will cause the ratings of A and B to change in absolute terms, but it will preserve the fact that A was rated two times as likely as B.
  <br/>
  <br/>
  <b>4</b>. When you set all 4 sliders to 0, they will all be reset to 25 which means that each event is equally likely.`,
  buttonText: "start training"
});
// <img src=stimuli/img/icons/red.png>
// <img src=stimuli/img/icons/yellow.png>
// <br />
// <input type='range' id=ex_slider class='magpie-response-slider replied' min='0' max='100' value='40' oninput='ex_slider.value/100'/>
// <output name=ex_slider_out id=out_ex class="thick">0.40</output>
// <script>document.getElementById("ex_slider").disabled=true;</script>

const instructions_train3 = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_train3",
  title: "Instructions Training",
  text: `In the next training trial, we ask you to indicate <b>how likely</b> you think certain
  blocks <b>will or will not fall</b> by moving the sliders below the respective
  icons.
   <br />
   <br />
  The more certain you are that an event <b>will</b> occur, the more you should
  position the corresponding slider towards the
  <b>right end</b> (<i>certain</i>/) and the more certain you are that it
  <b>will not</b> occur, the more you should position its slider towards the
  <b>left end</b> (<i>will not happen</i>).
  <br />
  When you are rather <b>uncertain whether or not</b> an event will occur, you
  should position the corresponding slider somewhere close to the center around
  0.50.
  <br />
  <br />
  Note, that your estimates <b>may</b>, but <b>do not have to</b> sum up to 100.
  <br />
  <br />
  From now on, you won't get feedback anymore about what will actually happen.
  <br />
  That means, after you have moved all four sliders (they turn green when moved
  and clicked to fix its new position), you will directly proceed with the next
  trial by clicking on <b>NEXT</b>.
  <br />
  Please click on <b>CONTINUE</b> to see an example.
  `,
  buttonText: "continue"
});

const instructions_fridge = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_fridge",
  title: "Instructions Test Phase",
  text: `Great &mdash; we will now proceed with the test phase. Each trial now
  consists of two tasks. Task 1 is the same as in the second part of the training
  phase (but without feedback). In task 2 we ask you to
  <b>describe to a critical friend</b> what you think <b>most likely happens
  with the two colored blocks</b> in the shown picture of block arrangements.
  As your friend is critical, <b>you don't want to say what you are not sufficiently
  convinced of</b> &mdash; your friend relies on you to get the <b>best description</b>
  of what <b>you truly believ</b>e that happens with the blocks in the shown picture.
  <br/>
  <br/>
  You can select words by clicking on them &mdash; the resulting sentence will be
  shown in the box in the lower left corner.
  The sentences that you can produce are <b>limited</b>. You will see that the
  set of words you can select next depends on the
  previously chosen word (some words will be greyed out).
  <br/>
  The following types of sentences will be producible:
  <br/>
  <b>1.</b> <b>simple assertions</b>, e.g. <i>The green block
  falls</i>, <i>The blue block does not fall</i>, ...
  <br/>
  <b>2.</b> <b>simple assertions</b> combined with <b>'might'</b>, e.g.
  <i>The green block might fall</i>, ...
  <br/>
  <b>3.</b> <b>conjunctions</b>, e.g. <i>The green block and the blue block fall</i>,
  <i>The blue block falls but the green block does not fall</i>, ...
  <br/>
  <b>4.</b> <b>conditionals</b>, e.g. <i>If the green block falls, the blue block
  falls as well</i>, <i>If the green block does not fall, the blue block falls</i>, ...
  <br />
  <br />
  <b>Corrections</b> can be made by clicking on <b>DELETE LAST WORD</b> in
  the lower right of the screen.
  </br>
  When you are done, you can directly click on <b>SUBMIT SENTENCE</b> to proceed or
  if you think that none of the provided sentences is adequate enough, you can also first
  freely enter a sentence by clicking on <b>USE MY OWN WORDS</b> and then submit.
  </br>
  Then as usual, proceed with the next trial by clicking on <b>NEXT SCENE</b>.`,
  buttonText: "go to example for task 2"
});
const instructions_fridge_procedure = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_fridge_procedure",
  title: "Procedure Test Phase",
  text: `<b>1</b>. Only the <b>highlighted</b> words (orange frame) are <b>clickable</b>;
  these differ depending on the position in the sentence that you are
  about to produce.
  <br/>
  <b>2</b>. <b>Corrections</b> can be made by clicking on
  <b>DELETE LAST WORD</b> in the lower right of the screen.
  <br />
  <b>3</b>. When you have produced a grammatical sentence (<i>not all
    grammatical sentences are producible!</i>), which is shown
  word by word in a box in the lower left, you will be able to click on
  <b>SUBMIT SENTENCE</b> to continue.
  <br />
  <b>4</b>. If you think there is a sentence that better describes
  what happens in the scene, please click on <b>USE MY OWN WORDS</b> and
  type it into the box that will appear in the lower left corner.
  <br/>
  By clicking on <b>NEXT SCENE</b>, your sentence <b>will be saved and
  you will directly get to the next trial</b>.
  <br/>
  <b>5</b>. If you think that there is no better sentence for describing
  what happens in the scene, click on <b>NEXT SCENE</b>
  right away to continue straight with the next trial.`,
  buttonText: "go to example task 2"
});


const instructions_test = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_test",
  title: "Instructions Test Phase",
  text: `Great -  we'll now proceed with the test phase of the experiment.
    As in the training phase, you will be shown scenes of different block
    arrangements.
    <br />
    <br />
    There will now be <b>two tasks for each scene</b>. The first task is as in the
    training phase: you will be asked to indicate <b>how likely</b> you
    think certain blocks will or will not fall (left: <i>impossible</i>
    event, right: <i>certain</i> event).
    <br/>
    Note that <b>you will not get feedback</b> anymore about what will happen.
    <br />
    <br />
    Then in <b>task 2</b>, you will see the same scene again and we will ask you to
    <b>produce a sentence</b> which we will explain next.
    `,
  buttonText: "continue"
});

// <b>Please keep in mind</b>:
// <br />
// <b>1</b>. A block is considered to <b>fall</b> as soon as it <b>drops off
// a platform</b> or <b>off another block</b> - that is, a block does
// <b>not necessarily</b> need to fall to the ground in order to count as falling.
// <br/>
// <b>2</b>. The colors do not have any meaning, all colored blocks represent
// <b>common toy blocks</b>, they all have <b>the same properties</b> and they
// <b>behave as in the training trials</b>.
// <br/>
// <b>3</b>. Only after you have adjusted all four sliders (all circles on the
// sliders have to be green!), you will be able to proceed with the next trial.
//   </br>
//   </br>
// We will now start with the test phase which comprises in total <b>13</b>
// scenes of block arrangements and <b>6</b> simple color questions in between.`,

const instructions_fridge_reminder = magpieViews.view_generator("instructions", {
  trials: 1,
  name: "instructions_fridge_reminder",
  title: "Short reminder",
  text: `Awesome &mdash; We will now start with the test phase which comprises in
  total <b>13</b> scenes of block arrangements (task1 and task2 for each scene)
  and <b>6</b> simple color questions in between.
  <br/>
  <br/>
  <b>Please keep in mind</b>:
  <br/>
  <b>1</b>. A block is considered to <b>fall</b> as soon as it <b>drops
    off a platform</b> or <b>off another block</b> - that is, a block does not
    necessarily need to fall to the ground in order to count as falling.
    <br/>
  <br/>
  <b>2</b>. The colors do not have any meaning, all colored blocks represent
  <b>common toy blocks</b>, they all have <b>the same properties</b> and they
  <b>behave as in the training trials</b>.
  <br/><br/>
  <b>3</b>. In task 2, please describe the scene <b>as adequately as possible</b>
  with the given words &mdash; namely to a <b>critical friend</b> who <b>expects
  you to say only what you are confident about</b>.
  <br/>
  <br/>
  <b>A final note</b>:
  <br/>
  You may have noticed that <b>might/might not</b> cannot be combined
  with <b>and</b> or <b>if</b>, e.g. you may produce
  '<i>the green block might fall</i>' but you <b>won't be able</b> to
  produce '<i>the green block might fall but the blue block might not</i>'.
  <br/>`,
  buttonText: "start test phase"
});

const post_test = magpieViews.view_generator("post_test", {
  trials: 1,
  name: "post_test",
  title: "Additional information",
  text: "Answering the following questions is optional, but your answers will help us analyze our results.",
  comments_question: 'Further comments'
});


// The 'thanks' view is crucial; never delete it; it submits the results!
const thanks = magpieViews.view_generator("thanks", {
  trials: 1,
  name: "thanks",
  title: "Thank you very much for taking part in this experiment!",
  prolificConfirmText: "Press the button"
});


const slider_choice_training = magpieViews.view_generator('sentence_choice', {
  trials: slider_choice_trials.length,
  name: "slider_choice_training",
  data: slider_choice_trials
}, {
  handle_response_function: function(config, CT, magpie, answer_container_generator, startingTime) {
    $(".magpie-view").append(answer_container_generator(config, CT));
    // attaches an event listener to the yes / no radio inputs
    // when an input is selected a response property with a value equal
    // to the answer is added to the trial object
    // as well as a readingTimes property with value
    $("input[name=answer]").on("change", function() {
    const RT = Date.now() - startingTime;
    let response = $("input[name=answer]:checked").val();
    let trial_data = {
        trial_name: config.name,
        trial_number: CT + 1,
        response: response,
        RT: RT
    };
    trial_data = magpieUtils.view.save_config_trial_data(config.data[CT], trial_data);
    magpie.trial_data.push(trial_data);
    if (response === config.data[CT].expected) {
        $.Zebra_Dialog(`Your answer is correct! Yey!` + config.data[CT].correct_statement,
        { type: "confirmation",
          keyboard: true,
          custom_class: "trainResult",
          title: "",
          position: ['center', 'bottom - 50'],
          onClose: function(caption, prompt) {
              magpie.findNextView();
          }
        });
    } else {
      $.Zebra_Dialog(`Sorry you answer was wrong.` + config.data[CT].correct_statement,
      { type: "error",
        keyboard: true,
        custom_class: "trainResult",
        title: "",
        position: ['center', 'bottom - 50'],
        onClose: function(caption, prompt) {
            magpie.findNextView();
        }
      });
    }
    // magpie.findNextView();
  });
  }
});

// experimental phase trials Experiment1
const multiple_slider = magpieViews.view_generator(
  "slider_rating", {
    // This will use all trials specified in `data`, you can use a smaller value
    // (for testing), but not a larger value
    trials: TEST_TRIALS.length,
    // name should be identical to the variable name
    name: "multiple_slider",
    data: TEST_TRIALS
  },
  // you can add custom functions at different stages through a view's life cycle
  {
    stimulus_container_generator: multi_slider_generator.stimulus_container_gen,
    answer_container_generator: multi_slider_generator.answer_container_gen,
    handle_response_function: multi_slider_generator.handle_response_function
  }
);

const multiple_slider_train = magpieViews.view_generator(
  "slider_rating", {
    trials: TRAIN_SLIDER_TRIALS.length,
    name: "multiple_slider_train",
    data: TRAIN_SLIDER_TRIALS
  },
  {
    stimulus_container_generator: multi_slider_generator.stimulus_container_gen,
    answer_container_generator: multi_slider_generator.answer_container_gen,
    handle_response_function: multi_slider_generator.handle_response_function
  }
)

multi_slider_view = function(idx){
  let view = magpieViews.view_generator(
    "slider_rating", {
      // This will use all trials specified in `data`, you can use a smaller value
      // (for testing), but not a larger value
      trials: 1,
      // name should be identical to the variable name
      name: "multiple_slider" + idx,
      data: TEST_TRIALS.slice(idx, idx+1)
    },
    // you can add custom functions at different stages through a view's life cycle
    {
      stimulus_container_generator: multi_slider_generator.stimulus_container_gen,
      answer_container_generator: multi_slider_generator.answer_container_gen,
      handle_response_function: multi_slider_generator.handle_response_function
    }
  );
  return(view)
}

const instruction_slider_example = magpieViews.view_generator(
  "slider_rating", {
    trials: 1,
    name: "instruction_slider_example",
    data: INSTRUCTION_SLIDER
  }, {
    stimulus_container_generator: multi_slider_generator.example_text_container_gen,
    answer_container_generator: multi_slider_generator.example_answer_container_gen,
    handle_response_function: multi_slider_generator.example_handle_response_function
  }
);

const attention_check_view = magpieViews.view_generator('dropdown_choice', {
    trials: 3,
    name: "attention_check",
    data: attention_check_trials
  }, {
    stimulus_container_generator: dropdown_choice_generator.stimulus_container_gen_attention,
    answer_container_generator: dropdown_choice_generator.answer_container_gen_attention,
    handle_response_function: dropdown_choice_generator.handle_response_function_attention
  });

fridge_view = function(idx){
  let view = magpieViews.view_generator(
    "slider_rating", {
      trials: 1,
      name: "fridge_view" + idx,
      data: FRIDGE_TRIALS.slice(idx, idx+1)
    }, {
      stimulus_container_generator: fridge_generator.stimulus_container_gen,
      answer_container_generator: fridge_generator.answer_container_gen,
      handle_response_function: fridge_generator.handle_response_function
    }
  );
  return(view)
}

color_vision_view = function(idx){
  let dropdown_choice_custom = magpieViews.view_generator('dropdown_choice', {
    trials: 1,
    name: "color-vision" + idx,
    data: [COLOR_VISION_TRIALS[idx]]
  }, {
    stimulus_container_generator: dropdown_choice_generator.stimulus_container_gen,
    answer_container_generator: dropdown_choice_generator.answer_container_gen,
    handle_response_function: dropdown_choice_generator.handle_response_function
  });
  return(dropdown_choice_custom)
}

let fridge_views = [];
let multi_slider_views = [];
let color_vision_views = [];
_.map(_.range(FRIDGE_TRIALS.length), function(idx){
  fridge_views.push(fridge_view(idx));
});
_.map(_.range(TEST_TRIALS.length), function(arr){
  multi_slider_views.push(multi_slider_view(arr));
});
_.map(_.range(0, color_vision_trials.length), function(idx){
  color_vision_views.push(color_vision_view(idx));
});

const fridge_train = magpieViews.view_generator(
  "slider_rating", {
    trials: TRAIN_FRIDGE_TRIALS.length,
    name: "fridge_train",
    data: TRAIN_FRIDGE_TRIALS
  }, {
    stimulus_container_generator: fridge_generator.stimulus_container_gen,
    answer_container_generator: fridge_generator.answer_container_gen,
    handle_response_function: fridge_generator.handle_response_function
  }
);

// const instructions_pretest = magpieViews.view_generator("instructions", {
//   trials: 1,
//   name: "instructions_pretest",
//   title: "Instructions Test Phase",
//   text: `In this short experiment, we ask you to indicate <b>how likely</b> you think
//   a shown toy block <b>will or will not fall</b> by moving a slider.
//    <br />
//   The larger your belief is that the block <b>will</b> fall,
//   the more you should position the slider towards the right end (<i>certainly</i>/100%).
//   <br />
//   The larger your belief is that it <b>will not</b> fall, the more you
//   should position the slider towards its left (<i>impossible</i>/0%).
//   <br/>
//   If you are quite <b>undecided</b> whether or not the block will fall, but you judge
//   it <i>a bit <b>more</b> likely that it <b>won't</b></i> fall, you should for instance move the slider a bit to the left.
//     <br />
//     <br />
//   Click on the <b>NEXT</b> button, which will appear after you have given your estimate by changing the position of the slider, to proceed with the next trial.
//   `
// });
//
// const instructions_train_pretest = magpieViews.view_generator("instructions", {
//   trials: 1,
//   name: "instructions_train_pretest",
//   title: "Instructions Train Phase",
//   text: `We will now start with the training phase
//           which consists of 8 trials. You will see block arrangements such that
//           you are able to develop intuitions about the physical properties and get
//           familiar with the stimuli.
//           <br/>
//           <br/>
//         For each presented scene you will be asked to indicate which of the
//          colored blocks you think will fall by clicking on the button with the
//          respective icons.
//          A falling block is represented by a skewed rectangle
//         and a resting block that <i>does not fall</i> is represented by a rectangle
//         with a line below. Here is an exemplary icon for the event
//         <br/>
//         <i>The <b>green</b> block <b>falls</b>, but the <b>yellow</b> block
//         <b>does not fall</b></i>:
//         <br/>
//         <br/>
//         <img src='stimuli/img/icons/green.png'/>
//         <img src='stimuli/img/icons/not-yellow.png' />
//          <br/>
//          <br/>
//          A block is considered to <b><i>fall</i></b> <b>as soon as it <i>topples
//          over</i> or <i>drops</i> from a platform or from another block.</b>
//          <br/>
//          The colored blocks represent common toy blocks, they do not have any
//          special properties and they are only distinguishable by
//          their color.
//              <br />
//              <br />
//          After you selected one of the four buttons (whose border will turn green),
//          you may click on RUN to see what actually happens. If you were wrong,
//          the selected button will turn red and the correct one will turn light
//          green.
//          Then, you can proceed with the next trial.`,
//   buttonText: "CONTINUE"
// });

// const slider_rating_pretest = magpieViews.view_generator("slider_rating", {
//   trials: pretest_trials.length,
//   name: "pretest",
//   data: pretest_trials
// }, {
//   stimulus_container_generator: function (config, CT) {
//     return `<div class='magpie-view'>
//         <h1 class='magpie-view-title'>${config.title}</h1>
//         <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
//         <div class='stimulus' id='stimulus-pic'>
//           <img src=${config.data[CT].picture}>
//         </div>
//       </div>`;
//   }
// });
