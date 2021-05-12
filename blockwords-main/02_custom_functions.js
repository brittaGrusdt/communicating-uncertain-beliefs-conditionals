// Here, you can define all custom functions, you want to use and initialize some variables

// You can determine global (random) parameters here

// Declare your variables here

/* For generating random participant IDs */
// https://stackoverflow.com/questions/1349404/generate-random-string-characters-in-javascript
// dec2hex :: Integer -> String
const dec2hex = function (dec) {
  return ("0" + dec.toString(16))
    .substr(-2);
};
// generateId :: Integer -> String
const generateID = function (len) {
  let arr = new Uint8Array((len || 40) / 2);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, this.dec2hex)
    .join("");
};

// Error feedback if participants exceeds the time for responding
const time_limit = function (data, next) {
  if (typeof window.timeout === "undefined") {
    window.timeout = [];
  }
  // Add timeouts to the timeoutarray
  // Reminds the participant to respond after 5 seconds
  window.timeout.push(
    setTimeout(function () {
      $("#reminder")
        .text("Please answer more quickly!");
    }, 5000)
  );
  next();
};

// compares the chosen answer to the value of `option1`
// check_response = function (data, next) {
//   $("input[name=answer]")
//     .on("change", function (e) {
//       if (e.target.value === data.correct) {
//         alert("Your answer is correct! Yey!");
//       } else {
//         alert(
//           "Sorry, this answer is incorrect :( The correct answer was " +
//           data.correct
//         );
//       }
//       next();
//     });
// };
// custom parameters:
const DURATION_ANIMATION = 7000; // in ms
const KEY2SELECTANSWER = "y";

const NB_TRAIN_TRIALS = TrainStimuli.list_all.length;
const TRAIN_BTTN_IDS = [BLOCK_COLS_SHORT.train.join('')].concat(
  BLOCK_COLS_SHORT.train).concat(['none']);

// const VAL_START_SLIDERS = 0
const VAL_START_SLIDERS = 25

//globally initialize
const EVENT_MAP = {"rny": "the red block falls, but not the yellow",
                  "ynr": "the yellow block falls, but not the red",
                  "none": "none of the two blocks falls",
                  "both": "both blocks fall"}
total_moves = VAL_START_SLIDERS === 0 ? 0 : 4;

// custom functions:
toggleNextIfDone = function (button, condition) {
  if (condition) {
    button.removeClass("grid-button");
  }
};

automaticallySelectAnswer = function (responseID, button2Toggle) {
  document.getElementById(responseID)
    .value = Math.floor(Math.random() * 101);
  $("#" + responseID)
    .addClass('moved');
}

addKeyToMoveSliders = function (button2Toggle) {
  let counter = 0;
  document.addEventListener("keydown", event => {
    var keyName = event.key;
    if (keyName === KEY2SELECTANSWER && counter <= 3) {
      var id_nb = counter + 1;
      automaticallySelectAnswer("response" + id_nb, button2Toggle);
      counter += 1;
    }
    toggleNextIfDone(button2Toggle, nbMoved() === 4);
    return keyName;
  });
}

// When the user clicks on the button, open the modal
showChart = function() {
  var modal = document.getElementById("chartModal");
  modal.style.display = "block";
}
// When the user clicks on <span> (x), close the modal
closeChart = function() {
  var modal = document.getElementById("chartModal");
  modal.style.display = "none";
}

fillChart = function(slider_ratings, color){
  let chart = color == "blue" ? chart_blue : color == "green" ? chart_green :
              color == "red" ? chart_red : chart_yellow;
  let p_pos = 0;
  let col = color=="blue" ? cols["royal"] : cols[color];
  _.map(slider_ratings, function(obj){
      if((color=="blue" || color=="red") && (obj.id=="response1" || obj.id=="response2")){
        p_pos = p_pos + obj.val
      }
      if((color=="green" || color=="yellow") && (obj.id=="response1" || obj.id=="response3")){
        p_pos = p_pos + obj.val
      }
  });
  chart.data = [{val: p_pos, category: color + " falls"},
                {val: 100-p_pos, category: color + " does not fall"}];
}

drawChart = function(slider_ratings,color){
  var chart = am4core.create(
    "chartdiv_" + color,
    am4charts.PieChart
  );
  chart.innerRadius = am4core.percent(40);
  var pieSeries = chart.series.push(new am4charts.PieSeries());

  let col = color=="blue" ? cols["royal"] : cols[color];
  var colorSet = new am4core.ColorSet();
  colorSet.list = [col, "#E3DFDC"].map(function(color) {
    return new am4core.color(color);
  });
  pieSeries.colors = colorSet;
  pieSeries.dataFields.value = "val";
  pieSeries.dataFields.category = "category";
  pieSeries.labels.template.disabled = true;
  pieSeries.ticks.template.disabled = true;
  return(chart)
}

_slidersAdjusted = function(){
  return(_.some([$("#response1").hasClass('adjusted'),
                 $("#response2").hasClass('adjusted'),
                 $("#response3").hasClass('adjusted'),
                 $("#response4").hasClass('adjusted')]))
}

nbMoved = function(){
  return(($("#response1").hasClass('moved')) +
         ($("#response2").hasClass('moved')) +
         ($("#response3").hasClass('moved')) +
         ($("#response4").hasClass('moved')));
}


sumResponses = function(){
  return(parseInt($("#response1").val()) + parseInt($("#response2").val()) +
         parseInt($("#response3").val()) + parseInt($("#response4").val()));
}

idx2Event = function(idx){
  let cat = "";
  if(idx+1 == 1) { cat = "blue and green fall"}
  else if(idx+1 == 2) { cat = "only blue falls"}
  else if(idx+1 == 3) { cat = "only green falls"}
  else { cat = "nothing happens"};
  return(cat)
}

_computeAdjustedCells = function(button2Toggle, responseIDS, n_moved, sumTo) {
   //  go through sliders in the order that they were moved!
   let order = _.map(responseIDS, function(id){
     let idx = _.last(id)
     let cat = idx2Event(idx-1);
     return({i_time: parseInt($("#" + id).attr('iReplied')), id, category: cat})
   });
   order = order.filter(function(obj){
     return(obj.i_time !== undefined && !isNaN(obj.i_time) && obj.i_time !== 0)
   });
   order = _.sortBy(order, 'i_time')
   let ratios = _.map(_.range(0, n_moved-1), function(idx){
     let id_current = order[idx]["id"]
     let id_next = order[idx+1]["id"]
     var next_val = parseInt($("#" + id_next).val())
     var this_val = parseInt($("#" + id_current).val())
     return(next_val/this_val)
   });
   let prods = _.map(ratios, function(val, idx){
     return(ratios.slice(0, idx+1).reduce(function(i, acc){return(i*acc)}, 1))
   });
   let total = prods.reduce(function(val, acc){return(acc+val)}, 0);
   // *100 as we output nbs from 0 to 100 not decimals
   let cell1_adj = sumTo /(1+total)
   let id1 = order[0].id
   let obj1 = {val: cell1_adj, id: id1, idxSlider: _.last(id1), category: order[0].category}
   let adjusted = [obj1].concat(
     _.map(prods, function(fct, idx){
       let id = order[idx+1]["id"]
       let new_val = {val: fct * cell1_adj, id: order[idx+1]["id"],
       idxSlider: _.last(id), category: order[idx+1]["category"]}
       return(new_val)
     }));
  return(adjusted)
}

_setSliderVals = function(normed_vals, button2Toggle, toggleCondition){
  let adjusted = _.sortBy(normed_vals, 'idxSlider')
  adjusted.forEach(function(obj){
    $("#response" + obj.idxSlider).val(obj.val);
    $("#response" + obj.idxSlider).addClass('adjusted');
    $("#output" + obj.idxSlider).val(Math.round(obj.val));
  });
  toggleNextIfDone(button2Toggle, toggleCondition);
}

_onChangeResponseFn = function(id, button2Toggle, col1, col2){
    $("#" + id).addClass('moved');
    total_moves = total_moves + 1;
    let sliderIDS = ["response1", "response2", "response3", "response4"];
    let sliders = _.map(sliderIDS, function(id){return($("#" + id))});
    slider_ratings = _.map(sliders, function(s){return(parseInt(s.val()))});

     $("#" + id).attr('iReplied', total_moves);
    let s = sumResponses()
    let ratings;
    if(nbMoved() >= 4){
      let sids = sliderIDS.filter(function(id){
        return($("#" + id).val() != 0)
      });
      // if all set to 0 set all to 25 otherwise
      // only sliders !=0 are updated (0 remains 0)
      if(sids.length == 0){
        ratings = _.map(_.range(1,5), function(idx){
          return({val: 25, id: "response" + (idx),
                  idxSlider: idx, category: idx2Event(idx-1)});
        });
      } else {
        ratings = _computeAdjustedCells(button2Toggle, sids, sids.length, 100)
        // if last moved slider was set 0, its new value (0) must be shown correctly!
        let values = _.map(sliderIDS, function(id){
          return {"id": id, "i_time": parseInt($("#" + id).attr('iReplied')),
                  "val": parseInt($("#" + id).val()),
                  "idxSlider": _.last(id), "category": idx2Event(_.last(id)-1)
                }
        })
        let last_slider = _.sortBy(values, 'i_time')[3]
        if(last_slider.val == 0){
          ratings.push(last_slider)
        }
      }
      _setSliderVals(ratings, button2Toggle, true);
      fillChart(ratings, col1);
      fillChart(ratings, col2);
    } else {
      // not yet moved all sliders, simply change slider + output values
      ratings = _.map(slider_ratings, function(val, idx){
        return({val: val, id: "response" + (idx+1),
        idxSlider: idx+1, category: idx2Event(idx)});
      });
      _setSliderVals(ratings, button2Toggle, false);
    }
    // toggleNextIfDone($("#runButton"), nbMoved()==4);
    // toggleNextIfDone($("#smallMarginNextButton"), nbMoved()==4);
}

_checkSliderResponse = function (id, button2Toggle, test) {
  let col1 = test ? "blue" : "red";
  let col2 = test ? "green" : "yellow";
  $("#" + id).on("change", function(){
    _onChangeResponseFn(id, button2Toggle, col1, col2);
  });
  $("#" + id).on("click", function(){
    _onChangeResponseFn(id, button2Toggle, col1, col2);
  });
  // show values while moving slider
  $("#" + id).on("input", function(){
    $("#output" + _.last(id)).val(parseInt($("#" + id).val()));
  });
}

addCheckSliderResponse = function (button2Toggle, test) {
  _.range(1, 5)
    .forEach(function (i) {
      _checkSliderResponse("response" + i, button2Toggle, test);
    });
}

abbreviateQuestion = function (question, symbols) {
  let q_words = [];
  question.split(' ')
    .forEach(function (w) {
      w = w.trim()
        .replace('<b>', '');
      w = w.replace('</b>', '');
      if (w === "will" || w === "not") {
        q_words.push(w)
      }
    });
  let w = q_words.join(' ')
  let q_short = w === 'will will' ? symbols.join('') :
    w === 'will will not' ? symbols[0] :
    w === 'will not will' ? symbols[1] : 'none';
  return q_short.toLowerCase();
}

iconHtml2Utterance = function (icon_html) {
  let words = icon_html.trim()
    .split('/');
  let utt1 = words[3].split('.')[0]
  let utt2 = words[6].split('.')[0]
  let utt;
  if (utt1.includes('not')) {
    utt = utt2.includes('not') ? 'none' : utt2[0]; //'not-blue-red';
  } else {
    utt = utt2.includes('not') ? utt1[0] : utt1[0] + utt2[0] // blue-not-red : 'blue-red';
  }
  return {
    short: utt,
    long: [utt1, utt2].join('-')
  };
}

getButtonResponse = function() {
  let responses = []
  let trial_data = {}
  TRAIN_BTTN_IDS.forEach(function(id, i){
    let response = $('#' + id).hasClass('selected');
    trial_data['response' + (i+1)] = response
    responses.push(response)
  });
  return Object.assign(trial_data, {
    'response': responses
  });
}

getSliderResponse = function () {
  let responses = [];
  let trial_data = {}
  _.range(1, 5)
    .forEach(function (i) {
      let response = $("#response" + i)
        .val();
      responses.push(response)
      trial_data['response' + i] = response
    });
  trial_data = Object.assign(trial_data, {
    'response': responses
  });
  return trial_data;
}


showAnimationInTrial = function (CT, html_answers, progress_bar = true) {
  let html_bar = progress_bar ? `<div class='progress-bar-container'>
       <div class='progress-bar'></div>
      </div>` : ``;
  const view_template = html_bar +
    `<div class='magpie-view-stimulus-grid'>
      <animationTitle class='stimulus'>
        <h2>${TRAIN_TRIALS[CT].QUD}</h2>
      </animationTitle>
      <animation id='animationDiv'></animation>
    </div>` +
    html_answers //+
    // htmlRunNextButtons();

  $('#main')
    .html(view_template);

  let stimulus = SHUFFLED_TRAIN_STIMULI[CT];
  if (DEBUG) {
    console.log(stimulus.id)
  }

  let worldElems = createWorld();
  let engine = worldElems.engine;
  let render = worldElems.render;
  let add_bottom = stimulus.id === "uncertain_2" ? false : true;
  addObjs2World(stimulus.objs, engine, add_bottom);
  show(engine, render);
  let startTime = Date.now();

  return {
    engine,
    render,
    startTime
  }
}


// new for fridge views//MALIN FRIDGE
checkBuildSentence = function (sentenceArray, poss_next, button2Toggle) {
  // sentence is only submittable if it is one of the sentences in UTTERANCES //
  let sentence = sentenceArray.join(" ");
  if(UTTERANCES.includes(sentence.trim())) {
    toggleNextIfDone(button2Toggle, true);
  } else {
    button2Toggle.addClass("grid-button");
  }
}

update_clickables = function (last_selected, all_selected, submitted=false) {
  WORDS.forEach(function (word) {
    $("#" + word.replace(/\s/g, ''))
    .addClass('not-clickable');
  })
  if(!last_selected) {
    last_selected = 'S'
  }
  if (submitted) {
    return [];
  } else {
    let poss_words = shownNext(last_selected, all_selected.join(" "));
    let c1 = BLOCK_COLS.test[0];
    let c2 = BLOCK_COLS.test[1];
    let idx_c1 = -1; let idx_c2 = -1;
    poss_words.forEach(function (word, i) {
      word = word.replace(/\s/g, '');
      $("#" + word)
      .toggleClass('not-clickable');
      if(word.includes(c1)) {
        idx_c1 = i
      }
      if(word.includes(c2)){
        idx_c2 = i
      }
    });
    let col_unclickable = idx_c1 != -1 && idx_c2 != -1 ?
      (_.some(all_selected, function(words){return words.includes(c1)}) ? poss_words[idx_c1] :
      (_.some(all_selected, function(words){return words.includes(c2)}) ? poss_words[idx_c2] : '')) : '';
    if(col_unclickable != '') {
      let id = col_unclickable.replace(/\s/g, '');
      $("#" + id).toggleClass('not-clickable');
    }
    poss_words = _.without(poss_words, col_unclickable);
    return poss_words
  }
}

getTrialById = function(trials_all, id){
  let trial = _.filter(trials_all, function(trial){
    return trial.id == id
  })
  trial.length != 1 ? console.error('several trials with id ' + id) : null;
  return trial[0];
}

let CountTrials = {'test': 0,
                   'color_vision': 0
                 };

/*@arg answers: "sliders", "buttons"*/
functionalityRunBttn = function(anim, answers){
  let animation = anim.animation;
  let CT = anim.CT;
  let id = SHUFFLED_TRAIN_STIMULI[CT].id
  let id_correct = TrainExpectations[id];
  $('#runButton')
    .on('click', function (e) {
      if (!anim.started) {
        anim.started = true;
        runAnimation(animation.engine);
        $('#runButton').addClass("grid-button");
        //selected answers can't be changed anymore
        if(answers == "sliders"){
          _.range(1, 5)
            .forEach(function (i) {
              document.getElementById("response" + i)
                .disabled = true;
            });
          if(id_correct.includes("b") || id_correct.includes("g")) {
            id_correct.replace("b", "r");
            id_correct.replace("g", "y")
          }
          let correct =
            id_correct == "ry" ? {idx: 1, img: "red-yellow.png"} :
            id_correct == "r" ? {idx: 2, img: "red-not-yellow.png"} :
            id_correct == "y" ? {idx: 3, img: "not-red-yellow.png"} :
                                {idx: 4, img: "not-red-not-yellow.png"};

          $('#response' + correct.idx).addClass("correct-slider");
          let val_correct = $("#response" + correct.idx).val();
          setTimeout(function () {
            new $.Zebra_Dialog(`The icon below shows what actually happened &mdash;
              you assigned this outcome a ` + `chance of <b>` + val_correct + "</b>%.<br/>" +
            `Notice that even if you assigned this event a rather low chance of occurring,
            this could still be a good guess, since, due to chance, other outcomes may
            have been possible. <br/>
            Keep using the sliders to express your <b>genuine
            beliefs and uncertainty </b> about what will happen in each scene.<br/><br/>`,
            { type: false,
              keyboard: true,
              source: {
                iframe: {src: "stimuli/img/icons/" + correct.img},
              },
              custom_class: "trainResult",
              title: "Actual outcome"
            });
            toggleNextIfDone($("#buttonNextAnimation"), true);
          }, 7000);
        } else { // buttons
            $('#' + id_correct).addClass("correct");
            _.map(TRAIN_BTTN_IDS, function(id){
              $('#' + id).addClass('train-not-clickable');
            });
            $('#comment').append(SHUFFLED_TRAIN_TRIALS[CT].comment)
            // highlight selected button in red if wrong:
            // console.log(id_selected + ' ' + id_correct);
            // id_selected !== id_correct ?
            //   $('#' + id_selected).addClass('incorrect') : null;
        }
      }
    });
}

functionalityBttnNextAnimation = function(response_fn, magpie, anim){
  $("#buttonNextAnimation")
    .on("click", function () {
      let CT = anim.CT
      let animation = anim.animation
      const RT = Date.now() - animation.startTime; // measure RT before anything else
      if (!anim.cleared) {
        clearWorld(animation.engine, animation.render, stop2Render = false);
      }
      let data = response_fn();
      let trial_data = Object.assign(data, {
        trial_name: anim.trial_name,
        trial_number: CT + 1,
        RT: RT,
        id: SHUFFLED_TRAIN_TRIALS[CT].id
      });
      // copied.expected = TrainExpectations[trial_data.id];
      trial_data = magpieUtils.view.save_config_trial_data(
        _.omit(SHUFFLED_TRAIN_TRIALS[CT], ['icon1', 'icon2', 'icon3', 'icon4']),
        trial_data
      );
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
    });
}
