let TestStimuli = {"independent": {}, "if1": {}, "if2": {}};

// IMPORTANT: DYNAMIC BLOCKS HAVE TO BE ADDED BEFORE STATIC OBJECTS, OTHERWISE
// THEY WILL FALL VERY ODD (JITTERING)

testTrials_if2 = function(priors){
  let pp = priors[0][0] + priors[1][0]
  let horiz = HORIZ_AC2[pp];
  let data = priors[0] !== priors[1] ?
    {side_ramp: "left", i_ramp: 0, b_sides: [1,1], prior: priors[0], 'increase': 0}:
    {side_ramp: "right", i_ramp: 1, b_sides: [-1,-1], prior: priors[1], 'increase': 1};

  let seesaw = Walls.test.if2(data.prior, horiz[data.i_ramp], data.side_ramp)
  let ramp = makeRamp(horiz[data.i_ramp], priors[data.i_ramp], data.increase,
    seesaw.walls[data.i_ramp]);
  let objs = seesaw.dynamic.concat([ramp.ball]);
  objs = objs.concat([ramp.tilted, ramp.wall_top]).concat(seesaw.walls);
  let w_no_ramp = seesaw.walls[data.i_ramp === 0 ? 1 : 0];
  // let w_x = w_no_ramp.bounds[data.i_ramp === 0 ? "min" : "max"].x
  // let w_width = w_no_ramp.bounds.max.x - w_no_ramp.bounds.min.x;
  let xBlock = blockOnBase(w_no_ramp, -1 * data.b_sides[0] * 0.60,
    cols.sienna, "Xblock", true);
  let bases = [xBlock];
  let w_ramp = seesaw.walls[data.i_ramp];
  data.i_ramp === 0 ? bases.unshift(w_ramp) : bases.push(w_ramp);
  let blocks = [];
  let colors = assignColors();
  let c1 = cols.test_blocks[colors[0]];
  let c2 = cols.test_blocks[colors[1]];

  let dir = horiz[data.i_ramp]
  let w = dir == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w;
  let p_fall = priors[data.i_ramp]
  let pr = (p_fall == "high" ||
            p_fall == "uncertain" ||
            p_fall == "uncertainL") ? "default" : p_fall
  let ps = [data.i_ramp === 0 ? (w + DIST_EDGE[pr]) / w : PRIOR[dir][priors[0]],
            data.i_ramp === 1 ? (w + DIST_EDGE[pr]) / w : PRIOR[dir][priors[1]]];
  let b1 = blockOnBase(bases[0], data.b_sides[0]*ps[0], c1, 'blockA', horiz[0]=='horizontal');
  let b2 = blockOnBase(bases[1], data.b_sides[1]*ps[1], c2, 'blockC', horiz[1]=='horizontal');
  let twoBlocks = [b1, b2];
  blocks = twoBlocks.concat(blocks);
  return blocks.concat(xBlock).concat(objs);
}

testTrials_if1 = function(priors){
  let colors = assignColors();
  let p1 = priors[0];
  let p2 = priors[1];
  let horiz =  HORIZ_AC1[p1[0]+p2[0]];
  let b2_w = horiz[1] == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w;

  let data = ['ll', 'hl', 'uh', 'uu'].includes(p1[0]+p2[0]) ?
  {edge_blocks: -1, increase: true, idx_w: 0, moveBall: 1, side:"right"} :
  {edge_blocks: 1, increase: false, idx_w: 1, moveBall: -1, side:"left"};

  let objs = Walls.test.if1(data.side, horiz[1], p2)

  let b1 = blockOnBase(objs.walls[0], PRIOR[horiz[0]][p1] * data.edge_blocks,
    cols.test_blocks[colors[0]], 'blockA', horiz[0] == 'horizontal');
  // uncertainty via length of base platform
  let b2 = blockOnBase(objs.walls[1], data.edge_blocks * (b2_w + DIST_EDGE["default"]) / b2_w,
    cols.test_blocks[colors[1]], 'blockC', horiz[1] == 'horizontal');

  let blocks = [b1, b2].concat(objs.dynamic);
  return blocks.concat(objs.walls)
}

testTrials_independent = function(priors){
  let pp = priors[0][0] + priors[1][0];
  let data = ['ll', 'uh', 'hl'].includes(pp) ?
    {walls: Walls.test.independent[0], increase: false, sides: [-1, 1]} :
    {walls: Walls.test.independent[1], increase: true, sides: [1, -1]};

  let horiz = HORIZ_IND[pp];
  let ramp = makeRamp(horiz[1], priors[1], data.increase, data.walls[1], "top");

  let colors = assignColors();
  let b1 = blockOnBase(data.walls[0], data.sides[0] * PRIOR[horiz[0]][priors[0]],
    cols.test_blocks[colors[0]], "blockA", horiz[0] == 'horizontal')
  let w2 = horiz[1] == 'horizontal' ? PROPS.blocks.h : PROPS.blocks.w;

  let ps = (w2 + DIST_EDGE[priors[1]]) / w2;
  let b2 = blockOnBase(ramp.wall_bottom, data.sides[1] * ps,
    cols.test_blocks[colors[1]], "blockC", horiz[1] == 'horizontal')

  // add seesaw as distractor
  let dist = seesaw(data.increase ? 680 : 120)
  let objs = data.walls.concat([dist.skeleton]).concat(
    [ramp.wall_top, ramp.wall_bottom, ramp.tilted]
  );
  return [b1, b2, ramp.ball].concat([dist.plank, dist.constraint]).concat(objs);
}

makeTestStimuli = function(conditions, relations){
  relations.forEach(function(rel){
    let priors_all = conditions[rel]
    for(var i=0; i<priors_all.length; i++){
      let priors = priors_all[i];
      let pb1 = priors[0]
      pb1 = (pb1.endsWith("L") || pb1.endsWith("H")) ?
        (pb1[0] + "-" + pb1[pb1.length-1]) : pb1[0];
      let pb2 = priors[1][0]
      let id = rel + '_' + pb1 + pb2;
      let blocks = rel === "if2" ? testTrials_if2(priors) :
                   rel === "if1" ? testTrials_if1(priors) :
                   rel === "independent" ? testTrials_independent(priors) : null;
      TestStimuli[rel][id] = {"objs": blocks, "meta": priors, "id": id};
    }
  });
  return TestStimuli
}

if (MODE === "test") {
  let prior_conditions = getConditions();
  let if1 = prior_conditions["if1"];
  if1.push.apply(if1, [["uncertainL", "high", "if1"],
                       ["uncertainH", "high", "if1"]]);
  if2 = prior_conditions["if2"];
  if2.push.apply(if2, [["uncertainL", "low", "if2"],
                       ["uncertainH", "low", "if2"]]);
  makeTestStimuli(prior_conditions, TRIAL_TYPES);
}
