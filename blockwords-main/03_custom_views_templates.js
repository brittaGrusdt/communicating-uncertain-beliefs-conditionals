// In this file you can create your own custom view templates

// A view template is a function that returns a view,
// this functions gets some config (e.g. trial_data, name, etc.) information as input
// A view is an object, that has a name, CT (the counter of how many times this view occurred in the experiment),
// trials the maximum number of times this view is repeated
// and a render function, the render function gets CT and the magpie-object as input
// and has to call magpie.findNextView() eventually to proceed to the next view (or the next trial in this view),
// if it is an trial view it also makes sense to call magpie.trial_data.push(trial_data) to save the trial information
const animation_view = {
  name: "animation_view",
  title: "title",
  CT: 0, //is this the start value?
  trials: NB_TRAIN_TRIALS - 1,
  data: "",
  // The render function gets the magpie object as well as the current trial
  // in view counter as input
  render: function (CT, magpie) {
    let html_answers = htmlButtonAnswers();
    let animation = showAnimationInTrial(CT, html_answers);
    let cleared = false;
    Events.on(animation.engine, 'afterUpdate', function (event) {
      if (!cleared && animation.engine.timing.timestamp >= DURATION_ANIMATION) {
        clearWorld(animation.engine, animation.render, stop2Render = false);
        cleared = true;
      }
    });
    let id_selected;
    TRAIN_BTTN_IDS.forEach(function (id) {
      $('#' + id)
        .on('click', function (e) {
          var parent = document.getElementById('TrainButtons');
          let selected = parent.getElementsByClassName("selected");
          let nb_selected = selected.length;
          if (nb_selected === 1) {
            TRAIN_BTTN_IDS.forEach(function (bttn) {
              $('#' + bttn).hasClass('selected') ?
              $('#' + bttn).removeClass('selected') : null;
            })
          }
          $('#' + id).addClass('selected');
          id_selected = id;
          nb_selected = 1;
          toggleNextIfDone($('#runButton'), true);
        });
    });
    let anim = {animation, cleared, CT, started:false, trial_name:'animation_buttons'};
    functionalityRunBttn(anim, "buttons");
    functionalityBttnNextAnimation(getButtonResponse, magpie, anim)
  }
};

// generate a new multi_slider
const multi_slider_generator = {
  stimulus_container_gen: function (config, CT) {
    return `<div class='magpie-view'>
        <h2 id='qud' class='stimulus'>${config.data[CT].QUD}</h2>
        <div class='stimulus_grid'>
          <img src=${config.data[CT].picture} class="stim_pic">
          <div class="response">
            <div id="chartdiv_blue"></div>
            <div id="chartdiv_green"></div>
          </div>
        </div>
      </div>
      `;
  },

  answer_container_gen: function (config, CT) {
    return htmlSliderAnswers(config.data[CT]) +
    `<button id='smallMarginNextButton' class='grid-button magpie-view-button'>continue with task 2</button>`;
  },
  handle_response_function: function (
    config,
    CT,
    magpie,
    answer_container_generator,
    startingTime
  ) {
    $(".magpie-view")
      .append(answer_container_generator(config, CT));
    let button = $("#smallMarginNextButton");
    let ratings = _.map(_.range(4), function(idx){
      // if(VAL_START_SLIDERS != 0){
        // $('#response' + (idx+1)).addClass('replied');
        $('#response' + (idx+1)).attr('iReplied', idx+1);
      // }
      return({val: VAL_START_SLIDERS, id: "response" + (idx+1),
              idxSlider: idx+1, category: idx2Event(idx)});
    });
    chart_blue = drawChart(ratings, "blue");
    chart_green = drawChart(ratings, "green");
    // function for debugging - if "y" is pressed, the slider will change
    if (DEBUG) {
      addKeyToMoveSliders(button);
      console.log(config.data[CT].id)
    }
    addCheckSliderResponse(button, true);
    button.on("click", function () {
      const RT = Date.now() - startingTime; // measure RT before anything else
      let responseData = getSliderResponse();
      CountTrials.test = CountTrials.test + 1;
      let trial_data = Object.assign(responseData, {
        trial_name: config.name,
        trial_number: CountTrials.test,
        RT: RT
      });
      trial_data = magpieUtils.view.save_config_trial_data(
        _.omit(config.data[CT], ['icon1', 'icon2', 'icon3', 'icon4']),
        trial_data
      );
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
    });
  },

  example_text_container_gen: function(config, CT){
    return `<div class='magpie-view smallMarginDiv'>
              <h1 class='magpie-view-title'>Slider Example</h1>
              <section class="magpie-text-container">
                  <p class="magpie-view-text">${config.data[CT].QUD}</p>
              </section>
            </div>`;
  },
  example_answer_container_gen: function (config, CT) {
    return htmlSliderAnswers(config.data[CT], ["55", "50", "5", "5"]) +
    `<div class='magpie-view smallMarginDiv'>
              <section class="magpie-text-container">
                  <p class="magpie-view-text">${config.data[CT].question}</p>
              </section>
      </div>
      <button id='smallMarginNextButton' class=magpie-view-button small-margin-button>Continue</button>`;
  },
  example_handle_response_function: function(
    config,
    CT,
    magpie,
    answer_container_generator,
    startingTime
  ) {
    $(".magpie-view")
      .append(answer_container_generator(config, CT));
    [1,2,3,4].forEach(function(i){
      // $("#response" + i).addClass('replied');
      $("#response" + i).addClass('moved');
      document.getElementById("response" + i).disabled = true;
    });

    $("#smallMarginNextButton").on("click", function () {
      magpie.findNextView();
    });
  }
};

// generate a new fridge view
const fridge_generator = {
  stimulus_container_gen: function (config, CT) {
    return `<div class='magpie-view'>
      <h2 id='qud' class='stimulus'>
      ${config.data[CT].QUD}
      </h2>
      <div class='stimulus'>
      <img src=${config.data[CT].picture}>
      </div>
      </div>`;
  },


  answer_container_gen: function (config, CT) {

    let start_words = _.flatten(shownNext('S'));
    function return_word_array(array, class_btn) {
      return array.map((word, index) => {
          let clickable = start_words.includes(word) ? ' ' : ' not-clickable ';
          return (
            `<div class="word ` + class_btn + clickable + `"id=` +
            word.replace(/\s/g, '') + ` >
            <p> ` +
            word +
            ` </p>
          </div>`
          ); + `</div> `
        })
        .join('')
    }
    // let word_array = '';
    // WORD_GROUPS.forEach(function(group_obj, i){
    //   word_array = word_array + return_word_array(group_obj.words, group_obj.col)
    // })
    let word_array = return_word_array(WORDS, 'word-button')
    if(DEBUG) {console.log(config.data[CT].id)}
    return `<div class = "fix-box"> <div class="fridge">` +
      word_array +
      `</div>
      <br>
      <div class ="sentence selected1" style = "font-size: 20px"> Your sentence:<br/><br/>
        <span class = "selected-words" id ="sentence">${config.data[CT].sentence}</span>
      </div>
      <div align="right">
        <button id='buttonDelete' class='magpie-view-button fridge-buttons'> delete last word </button>
        </br>
        <button id='buttonSubmit' class='magpie-view-button grid-button fridge-buttons'> submit sentence</button>
      </div>
      <div id="container">
      <div id="custom-sentence" class = "magpie-nodisplay sentence" >
        <p class="magpie-view-text">
          <label for="custom-text" style = "font-size: 20px"> Your custom sentence: </label>
          <input type="text" id="custom-text" name="textbox-input" cols = 50 class='magpie-response-text selected-words'>
        </p>
      </div>
      <div align="right">
        <button id ='customWords' class="magpie-view-button magpie-nodisplay fridge-buttons"> Use my own words </button>
        <button id='buttonNextFridge' class='magpie-view-button magpie-nodisplay fridge-buttons'>Next scene</button>
      </div>
    </div>`;
  },

  handle_response_function: function (
    config,
    CT,
    magpie,
    answer_container_generator,
    startingTime
  ) {
    $(".magpie-view")
      .append(answer_container_generator(config, CT));
    let button = $("#buttonNextFridge");
    let submitbutton = $("#buttonSubmit");
    let sentence_array = [];
    let sentence = "";
    let custom_sentence = "";

    let textInput = $("#custom-text");

    // each word which is pressed is saved in an array to build the sentence
    $(".word")
      .click(function () {
        var value = $(this)
          .text()
          .replace(/(\r\n|\n|\r)/gm, " ")
          .trim();
        sentence_array.push(value)

        let poss_next = update_clickables(value, sentence_array);
        $(".selected-words")
          .append(" " + value)
          // Important for displaying sentence built so far
        sentence = sentence_array.toString()
          .replace(/,/g, " ");
        config.data[CT].sentence = sentence;
        config.data[CT].cost = sentence_array.length;

        checkBuildSentence(sentence_array, poss_next, submitbutton)
      });

    $("#buttonDelete")
      .click(function () {
        sentence_array.pop();
        var sentence = sentence_array.join(" ")

        $(".selected-words")
          .empty();

        $(".selected-words")
          .append(sentence);
        //update for synchronizing with what is shown in textbox

        config.data[CT].sentence = sentence;
        config.data[CT].cost = sentence_array.length;
        let value = _.last(sentence_array)
        let poss_next = update_clickables(value, sentence_array);
        checkBuildSentence(sentence_array, poss_next, submitbutton);
      });

    $("#customWords")
      .on("click", function () {

        $("#custom-sentence")
          .removeClass("magpie-nodisplay");

        submitbutton.addClass("grid-button");

        // attaches an event listener to the textbox input
        textInput.on("keyup", function () {
        // if any text is typed, 'next' button appears
          if (textInput.val()
            .trim()
            .length > 0) {
            submitbutton.removeClass("grid-button");
          } else {
            submitbutton.addClass("grid-button");
          }
        });
        custom_sentence = document.getElementById('custom-text');
      });

    submitbutton.on("click", function () {
      $("#buttonNextFridge")
        .removeClass("magpie-nodisplay")
      $("#customWords")
        .removeClass("magpie-nodisplay");
      toggleNextIfDone($("#customWords"), true);
      toggleNextIfDone($("#buttonNextFridge"), true);
      update_clickables('', sentence_array, true);
      submitbutton.addClass("magpie-nodisplay");
      $("#buttonDelete")
        .addClass("magpie-nodisplay");
      $(".selected1")
        .addClass("magpie-nodisplay");
    });

    button.on("click", function () {
      const RT = Date.now() - startingTime; // measure RT before anything else
      let response2 = custom_sentence == "" ? "" : custom_sentence.value;
      CountTrials.test = CountTrials.test + 1;
      let trial_data = {
        trial_name: config.name,
        trial_number: CountTrials.test,
        response: [config.data[CT].sentence, response2],
        response1: config.data[CT].sentence,
        response2: response2,
        response3: config.data[CT].cost,
        response4: response2.split(" ").length,
        RT: RT
      };
      trial_data = magpieUtils.view.save_config_trial_data(
        _.omit(config.data[CT], 'sentence'),
        trial_data
      );
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
    });
  }
};

const dropdown_choice_generator = {
  answer_container_gen_attention: function (config, CT) {
    const question_left_part = config.data[CT].question_left_part === undefined ? "" : config.data[CT].question_left_part;
    const question_right_part = config.data[CT].question_right_part === undefined ? "" : config.data[CT].question_right_part;
    return `
    <div class='magpie-view-answer-container magpie-response-dropdown'>
      ${question_left_part}
      <select id='response' name='answer'>
         <option disabled selected></option>
         <option value=${config.data[CT].option1}>${EVENT_MAP[config.data[CT].option1]}</option>
         <option value=${config.data[CT].option2}>${EVENT_MAP[config.data[CT].option2]}</option>
         <option value=${config.data[CT].option3}>${EVENT_MAP[config.data[CT].option3]}</option>
         <option value=${config.data[CT].option4}>${EVENT_MAP[config.data[CT].option4]}</option>
      </select>
      ${question_right_part}
      </p>
      <button id='next' class='magpie-view-button magpie-nodisplay'>Next</button>
   </div>`;
  },
  stimulus_container_gen: function (config, CT) {
    return `<div class='magpie-view'>
    <h1 class='magpie-view-title'>${config.title}</h1>
    <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
    <div class='magpie-view-stimulus-container'>
      <img src="${config.data[CT].picture}" class = "img"/>
    </div>
  </div>`;
  },
  stimulus_container_gen_attention: function (config, CT) {
    return `<div class='magpie-view'>
    <h1 class='magpie-view-title'>${config.title}</h1>
    <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
    <div class='magpie-view-stimulus-container'>
      <img src="${config.data[CT].picture}" class ="img_wide"/>
    </div>
  </div>`;
  },
  answer_container_gen: function (config, CT) {
    const question_left_part = config.data[CT].question_left_part === undefined ? "" : config.data[CT].question_left_part;
    const question_right_part = config.data[CT].question_right_part === undefined ? "" : config.data[CT].question_right_part;
    return `<div class='magpie-view-answer-container magpie-response-dropdown'>
               ${question_left_part}
               <select id='response' name='answer'>
                   <option disabled selected></option>
                   <option value=${config.data[CT].option1}>${config.data[CT].option1}</option>
                   <option value=${config.data[CT].option2}>${config.data[CT].option2}</option>
                   <option value=${config.data[CT].option3}>${config.data[CT].option3}</option>
                   <option value=${config.data[CT].option4}>${config.data[CT].option4}</option>
                   <option value=${config.data[CT].option5}>${config.data[CT].option5}</option>
                   <option value=${config.data[CT].option6}>${config.data[CT].option6}</option>
                   <option value=${config.data[CT].option7}>${config.data[CT].option7}</option>
               </select>
               ${question_right_part}
               </p>
               <button id='next' class='magpie-view-button magpie-nodisplay'>Next</button>
           </div>`;
},
handle_response_function: function(config, CT, magpie, answer_container_generator, startingTime){
  let response;
  const question_left_part =
      config.data[CT].question_left_part === undefined ? "" : config.data[CT].question_left_part;
  const question_right_part =
      config.data[CT].question_right_part === undefined ? "" : config.data[CT].question_right_part;

  $(".magpie-view").append(answer_container_generator(config, CT));
  response = $("#response");
  response.on("change", function() {
      $("#next").removeClass("magpie-nodisplay");
  });
  $("#next").on("click", function() {
      const RT = Date.now() - startingTime; // measure RT before anything else
      CountTrials.color_vision = CountTrials.color_vision + 1;
      let trial_data = {
          trial_name: config.name,
          // trial_number: CT + 1,
          trial_number: CountTrials.color_vision,
          question: question_left_part.concat("...answer here...").concat(question_right_part),
          response: response.val(),
          RT: RT
      };

      trial_data = magpieUtils.view.save_config_trial_data(config.data[CT], trial_data);
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
  });
},
handle_response_function_attention: function(config, CT, magpie, answer_container_generator, startingTime){
  const question_left_part =
      config.data[CT].question_left_part === undefined ? "" : config.data[CT].question_left_part;
  const question_right_part =
      config.data[CT].question_right_part === undefined ? "" : config.data[CT].question_right_part;

  $(".magpie-view").append(answer_container_generator(config, CT));
  let response = $("#response");
  response.on("change", function() {
      $("#next").removeClass("magpie-nodisplay");
  });
  $("#next").on("click", function() {
      const RT = Date.now() - startingTime; // measure RT before anything else
      CountTrials.color_vision = CountTrials.color_vision + 1;
      let trial_data = {
          trial_name: config.name,
          trial_number: CT + 1,
          question: question_left_part.concat("...answer here...").concat(question_right_part),
          response: response.val(),
          RT: RT
      };

      trial_data = magpieUtils.view.save_config_trial_data(config.data[CT], trial_data);
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
  });
}

};

// const custom_posttest_generator = {
//   answer_container_gen: function (config, CT) {
//     const quest = magpieUtils.view.fill_defaults_post_test(config);
//     return `<form>
//                     <p class='magpie-view-text'>
//                         <label for="age">${quest.age.title}:</label>
//                         <input type="number" name="age" min="18" max="110" id="age" />
//                     </p>
//                     <p class='magpie-view-text'>
//                         <label for="gender">${quest.gender.title}:</label>
//                         <select id="gender" name="gender">
//                             <option></option>
//                             <option value="${quest.gender.male}">${quest.gender.male}</option>
//                             <option value="${quest.gender.female}">${quest.gender.female}</option>
//                             <option value="${quest.gender.other}">${quest.gender.other}</option>
//                         </select>
//                     </p>
//                     <p class='magpie-view-text'>
//                         <label for="education">${quest.edu.title}:</label>
//                         <select id="education" name="education">
//                             <option></option>
//                             <option value="${quest.edu.graduated_high_school}">${quest.edu.graduated_high_school}</option>
//                             <option value="${quest.edu.graduated_college}">${quest.edu.graduated_college}</option>
//                             <option value="${quest.edu.higher_degree}">${quest.edu.higher_degree}</option>
//                         </select>
//                     </p>
//                     <p class='magpie-view-text'>
//                         <label for="languages" name="languages">${quest.langs.title}:<br /><span>${quest.langs.text}</</span></label>
//                         <input type="text" id="languages"/>
//                     </p>
//                     <p class="magpie-view-text">
//                         <label for="ramp1">Did you notice that in those trials, where a ball was present, the incline of the ramp was not always identical, but in some trials lower than in other trials?</label>
//                         <textarea name="ramp1" id="ramp1" rows="1" cols="40"></textarea>
//                     </p>
//                     <p class="magpie-view-text">
//                         <label for="comments">${quest.comments.title}</label>
//                         <textarea name="comments" id="comments" rows="6" cols="40"></textarea>
//                     </p>
//
//                     <button id="next" class='magpie-view-button'>${config.button}</button>
//             </form>`
//   },
//
//   handle_response_function: function (config, CT, magpie, answer_container_generator, startingTime) {
//     $(".magpie-view")
//       .append(answer_container_generator(config, CT));
//
//     $("#next")
//       .on("click", function (e) {
//         // prevents the form from submitting
//         e.preventDefault();
//
//         // records the post test info
//         magpie.global_data.age = $("#age")
//           .val();
//         magpie.global_data.gender = $("#gender")
//           .val();
//         magpie.global_data.education = $("#education")
//           .val();
//         magpie.global_data.languages = $("#languages")
//           .val();
//         magpie.global_data.comments = $("#comments")
//           .val()
//           .trim();
//         magpie.global_data.noticed_steepness = $("#ramp1")
//           .val();
//         magpie.global_data.endTime = Date.now();
//         magpie.global_data.timeSpent =
//           (magpie.global_data.endTime -
//             magpie.global_data.startTime) /
//           60000;
//
//         // moves to the next view
//         magpie.findNextView();
//       });
//   }
// };


// view for train trial with sliders instead of buttons
const animation_view_sliders = {
  name: "animation_view_sliders",
  title: "title",
  CT: 0,
  trials: NB_TRAIN_TRIALS - 1,
  data: "",
  // The render function gets the magpie object as well as the current trial
  // in view counter as input
  render: function (CT, magpie) {
    let html_answers = `
    <div class="stimulus_grid">` + htmlSliderAnswers(TRAIN_TRIALS[CT]) +
      `<div class="response">
        <div id="chartdiv_red"></div>
        <div id="chartdiv_yellow"></div>
       </div>
    </div>` + htmlRunNextButtons();
    let animation = showAnimationInTrial(CT, html_answers, progress_bar=true);
    let cleared = false;
    Events.on(animation.engine, 'afterUpdate', function (event) {
      if (!cleared && animation.engine.timing.timestamp >= DURATION_ANIMATION) {
        clearWorld(animation.engine, animation.render, stop2Render = false);
        cleared = true;
      }
    });
    let ratings = _.map(_.range(1,5), function(idx){
      if(VAL_START_SLIDERS != 0){
        // iReplied gives order in which sliders were moved
        $('#response' + idx).attr('iReplied', idx);
      }
      return({val: VAL_START_SLIDERS, id: "response" + idx,
              idxSlider: idx, category: idx2Event(idx-1)});
    });
    addCheckSliderResponse($('#runButton'), false);
    DEBUG ? addKeyToMoveSliders($("#runButton")) : null;

    let anim = {animation, cleared, CT, started:false,  trial_name:'animation_slider'}
    functionalityRunBttn(anim, "sliders");
    functionalityBttnNextAnimation(getSliderResponse, magpie, anim);
    chart_red = drawChart(ratings, "red");
    chart_yellow = drawChart(ratings, "yellow");
  }
};
