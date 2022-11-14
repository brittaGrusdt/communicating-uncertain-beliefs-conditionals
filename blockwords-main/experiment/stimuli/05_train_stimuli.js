// IMPORTANT: DYNAMIC BLOCKS HAVE TO BE ADDED BEFORE STATIC OBJECTS, OTHERWISE
// THEY WILL FALL VERY ODD (JITTERING)
let TrainStimuli = {
  'map_category': {"independent": {}, "uncertain": {}, "if1": {},
                   "if2": {}},
  'list_all': []
};
TrainExpectations = {};

trials_independent = function(){
  let walls = Walls.train.independent;
  let prior = {
    "ind0": ["uncertainH", "lowH"],
    "ind1": ["high", "uncertainH"],
    "ind2": ["uncertainL", "high"] // slider train trial (no feedback)
  };
  let dir = {'ind0': ['vertical', 'vertical'],
             'ind1': ['vertical', 'vertical'],
             'ind2': ['horizontal', 'vertical']};
  let expected = {'ind0': BLOCK_COLS_SHORT.train[0],
                  'ind1': BLOCK_COLS_SHORT.train.join(""),
                  'ind2': ''}
  let data = {}
  _.keys(dir).forEach(function(id, i) {
    let walls = Walls.train.independent;
    let ramp = makeRamp(dir[id][0], prior[id][0], false, walls[0], "top", false);

    let color_blocks = id == "ind2" ? cols.test_blocks : cols.train_blocks;
    let b1 = blockOnBase(ramp.wall_bottom, PRIOR['impossible'],
      color_blocks[0], 'block1', dir[id][0] == 'horizontal');
    let b2 = blockOnBase(walls[1], -PRIOR[dir[id][1]][prior[id][1]],
      color_blocks[1], 'block2', dir[id][1] == 'horizontal');

    let ssw= seesaw(x=walls[1].bounds.min.x - 250);
      // y_base_min=SCENE.h - PROPS.bottom.h,
      // props={'stick': {'h': PROPS.seesaw.stick.h * 2/3, 'w': PROPS.seesaw.stick.w}
    // });
    walls = walls.concat([ramp.tilted, ramp.wall_bottom, ssw.skeleton]);
    let objs = {'objs':[ssw.plank, ssw.constraint, b1, b2, ramp.ball].concat(walls),
                'meta': prior[id],
                'id': id}
    data[id] = objs
    TrainExpectations[id] = expected[id];
  });

  return data
}

trials_ramp = function(){
  let cs = BLOCK_COLS_SHORT.train;
  let colors = {distance0: [cols.train_blocks[1],
                            cols.sienna,
                            cols.train_blocks[0]],
                distance1: [cols.sienna].concat(cols.train_blocks),
                balls: COLORS_BALL.train
               };
  let priors = {distance0: ['xlow', 'low', 'high'],
                distance1: ['xlow', 'low', 'uncertainH']}
  let dir = {distance0: ['horizontal', 'horizontal', 'horizontal'],
             distance1: ['vertical', 'vertical', 'vertical']};
  let expected = {distance0: cs[0], distance1: cs[1]}

  let data = {};
  _.keys(dir).forEach(function(id, i) {
    // add walls
    let walls = Walls.train[id]
    let objs_dyn = [];
    _.range(0, priors[id].length).forEach(function(idx){
      let ramp = makeRamp(dir[id][idx], "uncertain", false, walls[idx],
                          "top", false, colors.balls[idx])
      ramp.ball.label = 'ball' + idx;
      objs_dyn.push(ramp.ball);
      walls.push(ramp.tilted);

      // data for blocks
      let width_base = BASE_RAMP[dir[id][idx]][priors[id][idx]];
      let base = wall('ramp_bottom' + idx,
                      ramp.wall_bottom.bounds.min.x + width_base / 2,
                      ramp.wall_bottom.position.y, width_base);
      walls.push(base);
      let width = dir[id][idx] == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w
      let horiz = dir[id][idx] == 'horizontal';
      let dist = (id=="distance0" && idx==1) ? "low" : "default";
      let prop_on_base = (width + DIST_EDGE[dist]) / width;
      let block = blockOnBase(base, prop_on_base, colors[id][idx],
                              "block"+idx, horiz);
      objs_dyn.push(block);
    });
    data[id] = {'objs': objs_dyn.concat(walls), 'meta': priors[id], id}
    TrainExpectations[id] = expected[id]
  });
  return data
}

trials_uncertain = function(){
  let priors = {'uncertain0': ["uncertainL", "uncertainH"],
                'uncertain1': ["uncertainH", "low"],
                'uncertain2': ['high', 'uncertainL'],
                'uncertain3': ['low', 'high']};
  let dir = {'uncertain0': ['horizontal', 'vertical'],
             'uncertain1': ['horizontal', 'horizontal'],
             'uncertain2': ['horizontal', 'vertical'],
             'uncertain3': ['vertical', 'vertical'],
            };
 let colors = BLOCK_COLS_SHORT.train;

  let expected = {'uncertain0': colors[1],
                  'uncertain1': colors[0],
                  'uncertain2': colors[0],
                  'uncertain3': colors[1]}
  let walls = Walls.train.uncertain;
  let data = {}
  _.keys(priors).forEach(function(id, i){
    let b1 = [blockOnBase(walls[0], PRIOR[dir[id][0]][priors[id][0]],
      cols.train_blocks[0], "block1", dir[id][0] == 'horizontal')];
    let b2 = [blockOnBase(walls[1], -PRIOR[dir[id][1]][priors[id][1]],
      cols.train_blocks[1], "block2", dir[id][1] == 'horizontal')];

    let xBlocks = [];
    if(id == "uncertain1"){
      xBlocks.push(blockOnBase(walls[1], -PRIOR["horizontal"]["uncertainL"],
        cols.sienna, "xBlock", true));
      xBlocks.push(blockOnBase(xBlocks[0], PRIOR[dir[id][1]][priors[id][1]],
        cols.train_blocks[1], "block2", dir[id][1] == 'horizontal'));
      b2 = [];
    } else if (id == "uncertain2") {
      xBlocks.push(blockOnBase(walls[0], PRIOR["horizontal"]["lowL"],
        cols.sienna, "xBlock", true));
      xBlocks.push(blockOnBase(xBlocks[0], -PRIOR[dir[id][0]][priors[id][0]],
        cols.train_blocks[0], "block2", dir[id][0] == 'horizontal'));
      b1 = [];
    }
    data[id] = {objs: b1.concat(b2).concat(walls).concat(xBlocks),
                meta: priors[id],
                id}
    TrainExpectations[id] = expected[id]
  });
  return data
  }

// A implies C TRIALS
trials_ac = function(){
  let priors = {
    'ac0': ["high", "high"],
    'ac1': ["high", "low"],
    'ac2': ['uncertainH', 'uncertainH'],
    'ac3': ['uncertainL', 'high']
  };
  let dir = {'ac0': ['horizontal', 'horizontal'], 'ac1': ['vertical', 'vertical'],
             'ac2': ['horizontal', 'vertical'], 'ac3': ['vertical', 'horizontal']}
  let colors = {'ac0': cols.train_blocks,
                'ac1': [cols.train_blocks[1], cols.train_blocks[0]],
                'ac2': cols.train_blocks,
                'ac3': [cols.train_blocks[1], cols.train_blocks[0]]}

  let expected = {'ac0': BLOCK_COLS_SHORT.train.join(""),
                  'ac1': BLOCK_COLS_SHORT.train[1],
                  'ac2': BLOCK_COLS_SHORT.train.join(""),
                  'ac3': 'none'};
  let data = {};
  _.keys(priors).forEach(function(id, i){
    let objs = Walls.train.if1("left", dir[id][1], priors[id][1]);
    let p1 = priors[id][0]
    let b1 = blockOnBase(objs.walls[0], PRIOR[dir[id][0]][priors[id][0]],
      colors[id][0], 'blockUp', dir[id][0] == 'horizontal');
    let b2 = blockOnBase(objs.walls[1], PRIOR.impossible, colors[id][1],
      'blockLow', dir[id][1] == 'horizontal');

    let blocks = [b1, b2].concat(objs.dynamic);
    data[id] = {objs: blocks.concat(objs.walls), meta: priors[id], id}
    TrainExpectations[id] = expected[id];
    });
    return data
}

// TRIALS with seesaw (kickoff/dont kickoff block)
trials_ssw = function(){
  let color = {'ssw0': [cols.train_blocks[1], cols.train_blocks[0]],
               'ssw1': cols.train_blocks}
  let dir = {
    'ssw0': ['vertical', 'horizontal'],
    'ssw1': ['vertical', 'horizontal']
  };
  let prior = {
    'ssw0': ["uncertainH", "low"],
    'ssw1': ["uncertainL", "low"]
  };
  let expected = {'ssw0': BLOCK_COLS_SHORT.train.join(""),
                  'ssw1': 'none'}
  data = {};

  _.keys(prior).forEach(function(id, i){
    let objs = Walls.train.ssw();
    let xblock = blockOnBase(objs.walls[1], -PRIOR.horizontal.uncertainL,
      cols.sienna, "Xblock", true);
    let xblock2 = blockOnBase(objs.walls[0], -PRIOR.vertical.uncertainL,
      cols.sienna, "Xblock2", false);
    let b1 = blockOnBase(objs.walls[0], PRIOR[dir[id][0]][prior[id][0]],
      color[id][0], 'blockC', dir[id][0] == 'horizontal');

    let data_b2 = {base: xblock, side: 1, xblocks: [xblock, xblock2]};
    let b2 = blockOnBase(data_b2.base, data_b2.side * PRIOR[dir[id][1]][prior[id][1]],
      color[id][1], 'blockC', dir[id][1] == 'horizontal');

    data[id] = {
      objs: [b1, b2].concat(data_b2.xblocks).concat(objs.dynamic).concat(objs.walls),
      meta: prior[id],
      id}
    TrainExpectations[id] = expected[id];
  });
  return data
}

// pretestTrials = function(){
//   let horiz = [true, false]
//   let data = {};
//   [0, 1].forEach(function(ci){
//     let col_str = BLOCK_COLS.test[ci];
//     let col = cols.test_blocks[ci];
//     horiz.forEach(function(dir){
//       PRETEST_ANGLES.forEach(function(angle) {
//         let trial_id = [(dir ? "horiz" : "vert"), angle.toString(), col_str].join('-');
//         let increase = _.sample([true, false])
//         increase = true;
//         let x = increase ? SCENE.w/3 : SCENE.w * (2/3)
//         let walls = [wall('w_middle', x, SCENE.h/2)]
//         let ramp = makeRamp(angle, increase, walls[0], "bottom");
//         // block
//         let w = dir ? PROPS.blocks.h : PROPS.blocks.w;
//         let fct = increase ? -1 : 1;
//         let b = blockOnBase(walls[0], fct * (w+DIST_EDGE)/w, col, "block", dir);
//
//         let objs_dyn = [b, ramp.ball];
//         walls.push(ramp.tilted);
//         walls.push(ramp.wall_top);
//         data[trial_id] = {objs: objs_dyn.concat(walls), id: trial_id}
//       });
//     });
//   })
//   return data
// }

// generate all train stimuli
TrainStimuli.map_category["ramp"] = trials_ramp();
TrainStimuli.map_category["uncertain"] = trials_uncertain();
TrainStimuli.map_category["if2"] = trials_ssw();
TrainStimuli.map_category["independent"] = trials_independent();
TrainStimuli.map_category["if1"] = trials_ac();
// put all train stimuli into array independent of kind
let train_keys = _.keys(TrainStimuli.map_category);
train_keys.forEach(function(kind){
  let arr = _.values(TrainStimuli.map_category[kind]);
  TrainStimuli.list_all = TrainStimuli.list_all.concat(arr);
});
