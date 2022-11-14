// let _ = require('../node_modules/underscore/underscore.js')

// map from type of words to actual words
const GRAMMAR_VAR = {
  "SUBJ": ["the green block", "the blue block", "both blocks", "neither block"],
  "V": ["falls", "fall"],
  'CONJ': ["if", "but", "and"],
  'NEG': ["neither", "nor"],
  'NOT': ["does not"],
  'ADV1': ["as well"],
  'ADV2': ["might", "might not"]
}
// each word must appear in this array to get a color
let WORD_GROUPS = [
  {words: GRAMMAR_VAR.SUBJ,
   col: 'black'
 },
  {
    words: GRAMMAR_VAR.V,
    col: 'orange'
  },
  {
    words: GRAMMAR_VAR.NEG.concat(GRAMMAR_VAR.NOT),
    col: 'red'
  },
  {
    words: GRAMMAR_VAR.CONJ,
    col: 'blue'
  },
  {
    words: GRAMMAR_VAR.ADV1,
    col: 'light-blue'
  },
  {
    words: GRAMMAR_VAR.ADV2,
    col: 'purple'
  }
];
let WORDS = _.flatten(_.map(_.values(WORD_GROUPS), 'words'));
// console.log(WORDS)

let shownNext = function (last, sentence='') {
  // can the currently built sentence be extended further to an utterance?
  let poss_utts = _.filter(UTTERANCES, function(utt){
    return utt.startsWith(sentence.trim()) && sentence !== utt;
  });
  let symbols = poss_utts.length == 0 ? [] : WORDS;
  symbols = _.filter(symbols, function(word){
    // at least one of the possible utterences must be possible to build by current sentence + next word
    return _.some(poss_utts, function(utt){
      let s = (sentence + ' ' + word).trim()
      let holds = utt.startsWith(s);
      // make sure that fall and falls are used correctly
      holds =
       (sentence=="the green block" || sentence=="probably the green block" ||
        sentence=="the green block probably" || sentence=="the blue block" ||
        sentence=="probably the blue block" || sentence=="the blue block probably" ||
        sentence=="maybe the blue block" || sentence=="maybe the green block" ||
        (sentence.includes('neither') && !sentence.includes('nor')) ||
        (sentence.includes('if') && last !== "does not"))
        && word.startsWith('fall') ? (holds && word.endsWith('falls')) : holds;

      return holds;
    });
  });
  return symbols
}



let templates = [
  "the COL1 block falls",
  "the COL1 block does not fall",
  "the COL1 block might fall",
  "the COL1 block might not fall",

  "the COL1 block and the COL2 block fall",
  "the COL1 block falls and the COL2 block falls [as well]",
  "the COL1 block falls but the COL2 block does not [fall]",
  "the COL1 block falls and the COL2 block does not fall",
  "the COL1 block does not fall but the COL2 block falls",
  "the COL1 block does not fall and the COL2 block falls",
  "neither the COL1 block nor the COL2 block fall",
  "neither the COL1 block nor the COL2 block falls",
  "neither the COL1 block falls nor the COL2 block falls",
  "neither block falls",
  "both blocks fall",

  "if the COL1 block falls the COL2 block falls [as well]",
  "if the COL1 block does not fall the COL2 block falls",
  "if the COL1 block does not fall the COL2 block does not fall",
  "if the COL1 block falls the COL2 block does not fall",

  "the COL1 block falls if the COL2 block falls",
  "the COL1 block does not fall if the COL2 block falls",
  "the COL1 block does not fall if the COL2 block does not fall",
  "the COL1 block falls if the COL2 block does not fall"

]

let UTTERANCES = [];
_.map(templates, function(u){
  if(u.includes("COL1")) {
    let gb = u.replace("COL1", "green").replace("COL2", "blue");
    let bg = u.replace("COL1", "blue").replace("COL2", "green");

    let u1 = gb.replace("[", "").replace("]", "").trim();
    let u2 = gb.replace(/\[[^\]]*\] */g, "").trim() // replace what is inside []-brackets
    let u3 = bg.replace("[", "").replace("]", "").trim();
    let u4 = bg.replace(/\[[^\]]*\] */g, "").trim() // replace what is inside []-brackets

    UTTERANCES.push.apply(UTTERANCES, [u1, u2, u3, u4]);
  } else {
    UTTERANCES.push(u);
  }
});

UTTERANCES = Array.from(new Set(UTTERANCES));

// let symbols = shownNext('S')
// let i = _.random(0, symbols.length - 1)
// let selected = typeof(symbols[i]) == 'string' ? symbols[i] : _.sample(symbols[i]);
