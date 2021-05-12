// TRAINING TRIALS
// data for animations
pseudoRandomTrainTrials = function(){
  let dict = TrainStimuli.map_category;
  // start with simple trial where something happens: uncertain3 (now high/low (but name not changed))
  let fall1 = [dict.uncertain.uncertain3, dict.uncertain.uncertain1,
               dict.uncertain.uncertain2]
  let fall2 = _.shuffle([dict.if2.ssw0, dict.if1.ac2, dict.if1.ac0, dict.independent.ind1])
  let fall01 = _.shuffle([dict.if2.ssw1, dict.if1.ac3, dict.if1.ac1, dict.independent.ind0])
  let ramp = _.shuffle(_.values(dict.ramp));

  let fall02 = _.flatten(_.zip(fall01, fall2))
  fall02.splice(4, 0, dict.uncertain.uncertain0)
  let stimuli = fall1.slice(0, 2).concat(ramp).concat(fall02)
  stimuli = stimuli.concat([fall1[2], dict.independent.ind2])

  let trials = _.map(_.range(0, stimuli.length), function(i){
    return(getTrialById(TRAIN_TRIALS, stimuli[i].id))
  })
  return {stimuli_data: stimuli, trial_data: trials}
}

let training = pseudoRandomTrainTrials()
const SHUFFLED_TRAIN_STIMULI = training.stimuli_data;
const SHUFFLED_TRAIN_TRIALS = training.trial_data;
// const SHUFFLED_TRAIN_STIMULI = TrainStimuli.list_all;

// TEST TRIALS //
// generates sequence of test trial ids specified in PRIORS_IDS; if these
// change, order needs to be adapted here!
pseudoRandomTestTrials = function(){
  let if1_conj = _.shuffle(["if1_hh", "if1_lh"]);
  let if2_conj = _.shuffle(["if2_hl", "if2_ll"]);
  let ind_conj = _.shuffle(["independent_hh", "independent_ll"]);
  let ind = _.shuffle(["independent_uh", "independent_ul"]);
  ind.splice(1, 0, "independent_hl");
  let if1 = _.shuffle(["if1_uh", "if2_ul"]);
  let if2 = _.shuffle(["if1_u-Lh", "if2_u-Ll"]);
  // trials added such that not two same kinds directly after another, and
  // expected conditinals/conjunctions approximately evenly distributed
  let conditionals = _.flatten(_.shuffle([if1, if2]));
  let trials = _.flatten(_.zip(conditionals.slice(0, 3), ind)).concat(conditionals[3]);
  let conjunctions = []
  let i1 = 0; let i2=0;
  _.map(_.range(0, 7, by=2), function(i, idx){
    if(trials[i].includes("if1")){
      conjunctions.push(if2_conj[i2]);
      i2 = i2 + 1;
    } else {
      conjunctions.push(if1_conj[i1]);
      i1 = i1 + 1;
    }
  });
  trials = _.flatten(_.zip(conjunctions.slice(0,3), conditionals.slice(0, 3), ind))
  trials.push(conjunctions[3]);
  trials.push(conditionals[3]);
  // add 2 independent trials where conjunction expected s.t not two independent directly after one another
  trials.splice(4, 0, ind_conj[0]);
  trials.push(ind_conj[1]);
  // console.log(trials)
  return trials
}

// save trial data in specified pseudorandom order s.t. accessible in experiment
let shuffleTestTrials = function(trial_data){
  let shuffled_trials = [];
  let trial_ids = _.map(trial_data, 'id'); // data for all to be used test-ids
  const ids_sequence = pseudoRandomTestTrials();

  ids_sequence.forEach(function(id){
    let idx = _.indexOf(trial_ids, id)
    // let idx = _.lastIndexOf(test_ids, id)
    if(idx === -1) {
      let kind = id.slice(0, _.lastIndexOf(id, "_"));
      let ps = id.slice(_.lastIndexOf(id, "_") + 1);
      console.warn('Test trial with id: ' + id + ' not found. All test-ids to be used must be specified in PRIORS_IDS and considered in function *pseudoRandomTestTrials*!')
    }
    shuffled_trials.push(trial_data[idx]);
  });
  return shuffled_trials;
}
// Fridge-trials and test-trials (exp 1) need to be in the same order
const TEST_TRIALS = shuffleTestTrials(slider_rating_trials);
var test_trial_ids = _.map(TEST_TRIALS, 'id')
var fridge_trial_ids = _.map(fridge_trials, 'id')
var FRIDGE_TRIALS = [];

_.map(test_trial_ids, function(id, idx){
  let idx_fridge = fridge_trial_ids.indexOf(id)
  FRIDGE_TRIALS[[idx]] = fridge_trials[idx_fridge]
});

const COLOR_VISION_TRIALS = _.shuffle(color_vision_trials);

if (DEBUG){
  let arr = _.map(TEST_TRIALS, 'id')
  let res = _.countBy(arr, function(id) {
    return id ? (id.includes('independent') ? 'ind' :
      id.includes('iff') ? 'iff' : 'ac') : 'undefined';
  });
  console.log(res)
}
