sliderTexts = function (cols) {
  let id2Text = {};
  id2Text['ac'] = "<b>" + cols[0] + " will</b> and <b>" + cols[1] + " will</b> fall.";
  id2Text['a'] = "<b>" + cols[0] + " will </b> and <b>" + cols[1] + " will not</b> fall.";
  id2Text['c'] = "<b>" + cols[0] + " will not</b> and <b>" + cols[1] + " will</b> fall.";
  id2Text["none"] = "<b>" + cols[0] + " will not</b> and <b>" + cols[1] + " will not</b> fall.";
  return id2Text;
}

sliderIcons = function (cols) {
  let id2Html = {}
  id2Html['ac'] = `<div id=` + cols[0] + cols[1] + ` class=div-border>` +
    `<img src=stimuli/img/icons/` + cols[0] + `.png>` +
    `<img src=stimuli/img/icons/` + cols[1] + `.png>` +
    `</div>`;

  id2Html['a'] = `<div id=` + cols[0] + ` class=div-border>` +
    `<img src=stimuli/img/icons/` + cols[0] + `.png>` +
    `<img src=stimuli/img/icons/not-` + cols[1] + `.png>` +
    `</div>`;

  id2Html['c'] = `<div id=` + cols[1] + ` class=div-border>` +
    `<img src=stimuli/img/icons/not-` + cols[0] + `.png>` +
    `<img src=stimuli/img/icons/` + cols[1] + `.png>` +
    `</div>`;

  id2Html["none"] = `<div id=none class=div-border>` +
    `<img src=stimuli/img/icons/not-` + cols[0] + `.png>` +
    `<img src=stimuli/img/icons/not-` + cols[1] + `.png>` +
    `</div>`;
  return id2Html;
}

let id2IconTest = sliderIcons(BLOCK_COLS.test);
let text_test_sliders = sliderTexts(BLOCK_COLS.test);

let id2IconTrain = sliderIcons(BLOCK_COLS.train);
// let id2IconTrain = sliderTexts(BLOCK_COLS.train);
let text_train_buttons = {
  'short': {ac: BLOCK_COLS_SHORT.train.join(''), a: BLOCK_COLS_SHORT.train[0],
            c: BLOCK_COLS_SHORT.train[1], none: 'none'},
  'long': {'ac': BLOCK_COLS.train[0] + " and " + BLOCK_COLS.train[1],
    'a': BLOCK_COLS.train[0] + " but <b>not</b> " + BLOCK_COLS.train[1],
    'c': "<b>Not </b>" + BLOCK_COLS.train[0] + " but " + BLOCK_COLS.train[1],
    'none': "<b>Neither </b>" + BLOCK_COLS.train[0] + " <b>nor</b> " + BLOCK_COLS.train[1]
  }
};
let text_test_buttons = {
  'short': {ac: BLOCK_COLS_SHORT.test.join(''), a: BLOCK_COLS_SHORT.test[0],
            c: BLOCK_COLS_SHORT.test[1], none: 'none'}
}

// function to randomly order the four utterences, given per trial
function shuffleQuestionsAllTrials(questions, slider_rating_trials = [{}]) {
  for (var i = 0; i < slider_rating_trials.length; i++) {
    let utterances = _.shuffle(questions);
    slider_rating_trials[i].question1 = utterances[0];
    slider_rating_trials[i].question2 = utterances[1];
    slider_rating_trials[i].question3 = utterances[2];
    slider_rating_trials[i].question4 = utterances[3];
  }
  return slider_rating_trials;
}

_htmlSliderQuestion = function (idx_question) {
  let o = `<q` + idx_question +
    ` class='magpie-view-question grid-question' id ='question` +
    idx_question + `'>`;
  let c = `</q` + idx_question + `>`;
  return {
    open: o,
    close: c
  };
}

_htmlSlider = function (idxSlider, utterance, options, value) {
  let sliderID = "slider" + idxSlider
  let responseID = "response" + idxSlider
  let answerID = "answer" + idxSlider
  let outputID = "output" + idxSlider
  let outputName = "outputSlider" + idxSlider
  value = value == "" ? VAL_START_SLIDERS : value;

  let start = "<s" + idxSlider + " class='magpie-grid-slider' id=" + sliderID + ">";
  let end = "</s" + idxSlider + ">";
  let qSlider = _htmlSliderQuestion(idxSlider);
  let html_question = qSlider.open + utterance + qSlider.close;
  let html_slider = start +
    `<span class='magpie-response-slider-option optionWide thick'>` + options.left + `</span>
     <input type='range' id=` + responseID + ` name=` + answerID +
    ` class='magpie-response-slider slider-width' min='0' max='100' step='1' value='` + value +
    `'>` +
    `<span class='magpie-response-slider-option optionWide thick'>` + options.right + `</span>
    <output name='` + outputName + `' id=` + outputID + ` class="thick">` + value  + `</output>` +
    end;
  return '<div class="qa-block">' + html_question + html_slider + '</div>'
}

htmlSliderAnswers = function (trial_data, values=["", "", "", ""]) {
  let utterances = [trial_data.icon1, trial_data.icon2,
                    trial_data.icon3, trial_data.icon4];
  let ids = trial_data.picture == '' ? ['ry', 'r', 'y', 'none'] : ['bg', 'b', 'g', 'none'];
  let html_str = `<div class='magpie-multi-slider-grid' id='answerSliders'>`;
  _.range(1, 5)
    .forEach(function (i) {
      let h = _htmlSlider(i, utterances[i - 1], {
        left: trial_data.optionLeft,
        right: trial_data.optionRight
      }, values[i-1]);
      html_str += h;
    });
  html_str += `</div>`
  return html_str;
}

htmlButtonAnswers = function () {
  return `<bttns id=TrainButtons class=buttonContainer>
    <button id="` + BLOCK_COLS_SHORT.train.join('') +
    `" class="unselected styled-button">` + id2IconTrain.ac + `</button>
    <div class="divider"/>
    <button id="` + BLOCK_COLS_SHORT.train[0] +
    `" class="unselected styled-button">` + id2IconTrain.a + `</button>
    <div class="divider"/>
    <button id="` + BLOCK_COLS_SHORT.train[1] +
    `" class="unselected styled-button">` + id2IconTrain.c + `</button>
    <div class="divider"/>
    <button id="none" class="unselected styled-button">` + id2IconTrain.none + `</button>
  </bttns>
  <p id=comment class=comment></p>`;
}

htmlRunNextButtons = function () {
  let htmlBttns =
    `<div id=parentRunNext class="magpie-buttons-grid">
      <run>
        <button id='runButton' class='grid-button magpie-view-button'>RUN</button>
      </run>
      <next>
        <button id='buttonNextAnimation' class='grid-button magpie-view-button'>NEXT SCENE</button>
      </next>
    </div>`;
  return htmlBttns;
}
