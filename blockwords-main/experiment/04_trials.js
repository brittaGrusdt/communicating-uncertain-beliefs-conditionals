// In this file you can specify the trial data for your experiment

// color vision with dropdown choice
const color_vision_trials = [
    {
        picture: 'stimuli/img/color_vision/green_blue.png',
        QUD: "What's the color of the <b>upper</b> block on the picture?",
        question_left_part: "The upper block is ",
        question_right_part: ".",
        option1: 'black',
        option2: 'blue',
        option3: 'green',
        option4: 'grey',
        option5: 'purple',
        option6: 'red',
        option7: 'yellow',
        expected: 'green',
        id: 'green-blue'
    },
    {
        picture: "stimuli/img/color_vision/blue_green.png",
        QUD: "What's the color of the <b>upper</b> block on the picture?",
        question_left_part: "The upper block is ",
        question_right_part: ".",
        option1: 'black',
        option2: 'blue',
        option3: 'green',
        option4: 'grey',
        option5: 'purple',
        option6: 'red',
        option7: 'yellow',
        expected: 'blue',
        id: 'blue-green'
    },
    {
        picture: "stimuli/img/color_vision/blue.png",
        QUD: "What's the color of the block on the picture?",
        question_left_part: "The block is ",
        question_right_part: ".",
        option1: 'black',
        option2: 'blue',
        option3: 'green',
        option4: 'grey',
        option5: 'purple',
        option6: 'red',
        option7: 'yellow',
        expected: 'blue',
        id: 'blue'
    },
    {
        picture: "stimuli/img/color_vision/green.png",
        QUD: "What's the color of the block on the picture?",
        question_left_part: "The block is ",
        question_right_part: ".",
        option1: 'black',
        option2: 'blue',
        option3: 'green',
        option4: 'grey',
        option5: 'purple',
        option6: 'red',
        option7: 'yellow',
        expected: 'green',
        id: 'green'
    },
    {
        picture: "stimuli/img/color_vision/red_yellow.png",
        QUD: "What's the color of the <b>upper</b> block on the picture?",
        question_left_part: "The upper block is ",
        question_right_part: ".",
        option1: 'black',
        option2: 'blue',
        option3: 'green',
        option4: 'grey',
        option5: 'purple',
        option6: 'red',
        option7: 'yellow',
        expected: 'red',
        id: 'red-yellow'
    },
    {
        picture: "stimuli/img/color_vision/yellow_red.png",
        QUD: "What's the color of the <b>upper</b> block on the picture?",
        question_left_part: "The upper block is ",
        question_right_part: ".",
        option1: 'black',
        option2: 'blue',
        option3: 'green',
        option4: 'grey',
        option5: 'purple',
        option6: 'red',
        option7: 'yellow',
        expected: 'yellow',
        id: 'yellow-red'
    }
];

const attention_check_trials = [
  {
    picture: 'stimuli/img/icons/not-red-yellow.png',
    QUD: "What event does this icon represent?",
    question_left_part: "The icon represents the event that ",
    question_right_part: ".",
    option1: "rny",
    option2: "ynr",
    option3: "none",
    option4: "both",
    expected: "ynr",
    id: 'not-red-yellow'
  },
  {
    picture: 'stimuli/img/icons/red-not-yellow.png',
    QUD: "What event does this icon represent?",
    question_left_part: "The icon represents the event that ",
    question_right_part: ".",
    option1: "rny",
    option2: 'both',
    option3: "ynr",
    option4: 'none',
    expected: 'rny',
    id: 'not-yellow-red'
  },
  {
    picture: 'stimuli/img/icons/red-yellow.png',
    QUD: "What event does this icon represent?",
    question_left_part: "The icon represents the event that ",
    question_right_part: ".",
    option1: 'none',
    option2: 'rny',
    option3: 'ynr',
    option4: 'both',
    expected: 'both',
    id: 'red-yellow'
  }
]

// ----- TEST TRIALS PROBABILITIES (ex.1)---- //
let test_ids = [];
_.map(PRIORS_IDS, function(ids, key){
    ids = ids.map(function(val){return key + "_" + val});
    test_ids = test_ids.concat(ids)
});

var slider_rating_trials = [];
test_ids.forEach(function(id) {
    slider_rating_trials.push({
      id: id,
      picture: "stimuli/img/group/" + id + ".png",
      QUD: "Task 1: Please indicate how likely you think the shown events will occur.",
      icon1: id2IconTest.ac,
      icon2: id2IconTest.a,
      icon3: id2IconTest.c,
      icon4: id2IconTest.none,
      question1: abbreviateQuestion(text_test_sliders.ac, BLOCK_COLS_SHORT.test),
      question2: abbreviateQuestion(text_test_sliders.a,  BLOCK_COLS_SHORT.test),
      question3: abbreviateQuestion(text_test_sliders.c,  BLOCK_COLS_SHORT.test),
      question4: abbreviateQuestion(text_test_sliders.none,  BLOCK_COLS_SHORT.test),
      optionLeft: "impossible",
      optionRight: "certain",
      expected: ''
    });
});
// adapt path to pictures depending on colour group in each trial
// add group and id separately
let n = slider_rating_trials.length;
// here I changed code to save changes into slider_rating_trials 22.5. Malin
_.map(slider_rating_trials, function (trial) {
  let group = _.sample(["group1", "group2"]);
  trial.picture = trial.picture.replace("group", group);
  trial.group = group;
});

// ----- TRAINING TRIALS (Buttons) for exp1 + exp2 ---- //
// the data of the training stimuli is always the same,
// the 4 buttons are always shown in same order
let TRAIN_TRIALS = [];
let train_ids = _.map(TrainStimuli.list_all, 'id');
let QUD = ANIM_ANSWERS == "sliders" ?
  "Please indicate how likely you think the shown events will occur. Click on RUN to see!" :
  "Which block(s) do you think will fall? Click on RUN to see!";
train_ids
  .forEach(function (id) {
    let comment = ''
    // if(id == 'ssw0') {comment = 'Note: the red block fell off another block.'}
    let data = {
      QUD: QUD,
      id: id,
      icon1: id2IconTrain.ac,
      icon2: id2IconTrain.a,
      icon3: id2IconTrain.c,
      icon4: id2IconTrain.none,
      question1: text_train_buttons.short.ac,
      question2: text_train_buttons.short.a,
      question3: text_train_buttons.short.c,
      question4: text_train_buttons.short.none,
      question: '',
      expected: TrainExpectations[id],
      optionLeft: 'impossible',
      optionRight: 'certain',
      group: '',
      picture: '',
      comment: comment
    };
    TRAIN_TRIALS.push(data);
  });

// one of the training trials is used with sliders/fridge view as in test phase
// instead of buttons
let id_slider = 'ind2';
let train_trials_cloned = _.cloneDeep(TRAIN_TRIALS)
TRAIN_SLIDER_TRIALS = _.filter(train_trials_cloned, function(trial){
  return trial.id == id_slider
});
TRAIN_SLIDER_TRIALS[0].QUD =
  "Task 1: Please indicate how likely you think the shown events will occur.";
TRAIN_SLIDER_TRIALS[0].picture = "stimuli/img/train/" + id_slider + "_test_colors.jpg";
TRAIN_SLIDER_TRIALS[0].icon1 = id2IconTest.ac
TRAIN_SLIDER_TRIALS[0].icon2 = id2IconTest.a
TRAIN_SLIDER_TRIALS[0].icon3 = id2IconTest.c
TRAIN_SLIDER_TRIALS[0].icon4 = id2IconTest.none
TRAIN_SLIDER_TRIALS[0].question1 = text_test_buttons.short.ac
TRAIN_SLIDER_TRIALS[0].question2 = text_test_buttons.short.a
TRAIN_SLIDER_TRIALS[0].question3 = text_test_buttons.short.c
TRAIN_SLIDER_TRIALS[0].question4 = text_test_buttons.short.none
TRAIN_SLIDER_TRIALS[0].group = "group1"

let INSTRUCTION_SLIDER = [{
  picture: '',
  optionLeft: "impossible",
  optionRight: "certain",
  icon1: id2IconTest.ac,
  icon2: id2IconTest.a,
  icon3: id2IconTest.c,
  icon4: id2IconTest.none,
  question1: text_test_buttons.short.ac,
  question2: text_test_buttons.short.a,
  question3: text_test_buttons.short.c,
  question4: text_test_buttons.short.none,
  expected: '',
  group: '',
  // QUD: `Let us assume, that you are pretty <b>certain</b> that the <b>red
  // block</b> falls, but <b>rather uncertain whether or not</b> the <b>yellow
  // block</b> also falls.
  // <br />
  // The following slider positions are an example for representing these beliefs.`,
  QUD: `In the next training trial, we ask you to indicate <b>how likely</b> you think certain
  blocks <b>will or will not fall</b> by moving the sliders below the respective
  icons.
   <br />
   <br />
  The more certain you are that an event <b>will</b> occur (e.g. both blocks
    will fall), the more you should position the corresponding slider towards the
  <b>right end</b> (<i>will happen</i>) and the more certain you are that it
  <b>will not</b> occur, the more you should position its slider towards the
  <b>left end</b> (<i>will not happen</i>).
  When you are rather <b>uncertain whether or not</b> an event will occur, you should position the corresponding slider in the center, around 0.5.
  <br/>
  <br />
  Here is an example which indicates that you are quite <b>certain</b> that the <b>red</b>
  block <b>will fall</b> (2 lower sliders where <i>red does not fall</i> rated <i>unlikely</i>), but <b>uncertain</b> whether or not the <b>yellow</b> block falls (2 upper sliders where red falls and yellow falls/does not fall have a <i>similar, moderate value</i>).
  <br />
  `,
  question: `Note that your estimates <b>do not necessarily</b> have to <i>sum to 1</i>
  as we are interested in your ratings relative to each other. Also note that you
  will <b>not</b> get feedback about what happens anymore.`
  // question: `Alternatively, the <b>same beliefs</b> can also be represented by setting
  // <b>the upper two sliders to roughly 0.50</b> and the <b>lower two sliders</b>
  // again to <b>small values near 0</b>.
  // Either way is fine - it only depends on your preferences.
  // <br />
  // Please click on <b>CONTINUE</b> to get to the last trial of the training phase.`
}];
// ----- FRIDGE TRIALS ---- //
// fridge trials have the same input data slider_rating_trials
let fridge_trials = _.cloneDeep(slider_rating_trials)
fridge_trials = _.map(fridge_trials, function (trial, i) {
  ['question1', 'question2', 'question3', 'question4',
   'optionLeft', 'optionRight', 'expected'].forEach(function(key){
    trial[key] = '';
  });
  trial.QUD = "Task 2: Please describe to a critical friend as adequately as possible <br/>what happens with the blue and the green block in the picture.",
  trial.sentence = "";
  trial = _.omit(trial, ['icon1', 'icon2', 'icon3', 'icon4']);
  return trial
});

let fridge_ex = Object.assign({}, fridge_trials[0])
fridge_ex.picture = "stimuli/img/train/ind2_test_colors.png";
fridge_ex.id = id_slider
fridge_ex.QUD = `TRY OUT EXAMPLE &mdash;` + fridge_trials[0].QUD;
const TRAIN_FRIDGE_TRIALS = [fridge_ex];

// let slider_choice_ids1 = ["all-equal", "both-or-none"]
let slider_choice_ids1 = ["sc_yes0", "sc_yes1"]
let slider_choice_ids2 = [
  // ["both-or-none-probably-both", "both-or-none-rather-none"],
  // ["probably-red-but-not-yellow", "probably-yellow-but-not-red"],
  // ["red-maybe-yellow", "yellow-maybe-red"],
  // ["red-probably-not-yellow", "yellow-probably-not-red"],
  // ["red-probably-yellow", "yellow-probably-red"]
];
slider_choice_ids2 = _.map(_.range(2,7), function(i){
  return(["sc_yes" + i + "_0", "sc_yes" + i + "_1"])
});

let part1 = `The sliders represent the beliefs of a person who <br/>` ;
let sc_questions1 = [
  part1 + `is <b>completely uncertain</b> whether the blocks fall, that is the person has <b>no tendency</b> towards any of the four events.`,
  part1 + `<b>thinks</b> that <b>either both blocks</b> or <b>none</b> of the two blocks fall.`
]
let sc_questions2 = [
  [part1 + `<b>thinks</b> that <b>either both</b> or <b>none</b> of the two blocks fall <br/>with a <b>tendency</b> towards the event that <b>both blocks fall</b>.`,
  part1 + `<b>thinks</b> that <b>either both</b> or <b>none</b> of the two blocks fall <br/>with a <b>tendency</b> towards the event that <b>no block falls</b>.`
  ],
  [part1 + `is <b>pretty certain</b> that the <b>red block falls</b> but <b>not the yellow</b>.`,
   part1 + `is <b>pretty certain</b> that the <b>yellow block falls</b> but <b>not the red</b>.`
  ],
  [part1 + `<b>thinks</b> that <b>red falls</b> but is <b>uncertain whether or not yellow falls</b>.`,
   part1 + `<b>thinks</b> that <b>yellow falls</b> but is <b>uncertain whether or not red falls</b>.`
  ],
  [part1 + `<b>thinks</b> that <b>red falls</b> but <b>probably not yellow</b>.`,
   part1 + `<b>thinks</b> that <b>yellow falls </b>but <b>probably not red</b>.`
  ],
  [part1 + `<b>thinks</b> that <b>red falls</b> and that <b>yellow</b> is <b>more likely to fall than not to fall</b>.`,
  part1 + `<b>thinks</b> that <b>yellow falls</b> and that <b>red</b> is <b>more likely to fall than not to fall</b>.`]
];
// randomly choose colors in question
let indices = _.map(_.range(0, 5), function(i){
  return(Math.round(Math.random()))
})
let qs = sc_questions1.concat(_.map(indices, function(i, idx) {
  return(sc_questions2[idx][i])
}));
let slider_choice_ids = slider_choice_ids1.concat(_.map(indices, function(i, idx) {
  return(slider_choice_ids2[idx][i])
}));
// questions 'yes' expected
let slider_choice_trials = _.map(_.range(slider_choice_ids.length), function (i) {
  let trial = {
    QUD: "Please click on the button with the correct answer.",
    question: qs[i],
    picture: "stimuli/img/slider-choices/" + slider_choice_ids[i] + ".png",
    option1: "yes",
    option2: "no",
    expected: "yes",
    // id: 'sc_yes' + i,
    id: slider_choice_ids[i],
    correct_statement: "</br>" + qs[i]
  }
  return trial
});
// question 'no' expected: add 3 (particular) trials change text not picture
let qs_no = [];
let ids_no = [];
let correct_res = [];
for(idx in [0, 1, 3]){
  let i = indices[idx]
  let i_wrong = i==0 ? 1 : 0
  let id = slider_choice_ids2[idx][i_wrong]
  id = idx==0 ? id + "-not0" : id;
  // opposite picture, (for both-or-none-probably-both/rather-none,
  // picture is slightly different, other two options have non-zero probability)
  ids_no.push(id)
  let q = sc_questions2[idx][i].replace("<b>thinks</b>", "<b>is very confident</b>");
  qs_no.push(q + "</b>") // but same question
  let rc = sc_questions2[idx][i_wrong].replace("<b>thinks</b>", "<b>is very confident</b>")
  rc = rc.replace("yellow", "<i>YELLOW</i>")
  rc = rc.replace("red", "<i>RED</i>")
  correct_res.push("</br>" + rc)
}
let choice_expected_no = _.map(_.range(0, ids_no.length), function(i){
  return {
    QUD: "Please click on the button with the correct answer.",
    question: qs_no[i],
    picture: "stimuli/img/slider-choices/" + ids_no[i] + ".png",
    option1: "yes",
    option2: "no",
    expected: "no",
    id: "no_" + ids_no[i],
    correct_statement: correct_res[i]
  }
});
slider_choice_trials.splice(1, 0, choice_expected_no[0]);
slider_choice_trials.splice(5, 0, choice_expected_no[1]);
slider_choice_trials.splice(8, 0, choice_expected_no[2]);

// order slider choice trials considering difficulty
let sc_low = _.shuffle(['sc_yes0', 'sc_yes1', 'sc_yes3', 'sc_yes5'])
let sc_middle = _.shuffle(['sc_no1', 'sc_yes2', 'sc_yes4', 'sc_yes6'])
let sc_hard = _.shuffle(['sc_no0', 'sc_no2'])
let sc_ids = _.flatten(_.zip(sc_low.slice(0,2), sc_middle.slice(0,2),
                             sc_low.slice(2,4), sc_middle.slice(2,4)))
sc_ids.splice(4, 0, sc_hard[0])
sc_ids.push(sc_hard[1])

function compare_sc(o1, o2) {
  if (sc_ids.indexOf(o1.id) > sc_ids.indexOf(o2.id)){return 1
  } else{
    return -1
  }
}
slider_choice_trials.sort(compare_sc);
