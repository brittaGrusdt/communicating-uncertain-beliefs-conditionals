makeColorVisionStimuli = function(){
  let stimuli = [];
  let base = wall('w_center', SCENE.w/2, SCENE.h/2, PROPS.walls.w,
    PROPS.walls.h, {'render': {'fillStyle': cols.grey}})
  let cols_blocks = cols.test_blocks.concat(cols.train_blocks);
  for(var trial=0; trial<cols_blocks.length; trial++) {
    let id = 'color' + trial;
    let col_block = cols_blocks[trial];
    let b = block(base.position.x, base.bounds.min.y, col_block,
      'b' + trial, true)

    // second block
    let cb = col_block
    let col_b2 = cb == cols.train_blocks[0] ? cols.train_blocks[1] :
      cb == cols.train_blocks[1] ? cols.train_blocks[0] :
      cb == cols.test_blocks[0] ? cols.test_blocks[1] :
      cb == cols.test_blocks[1] ? cols.test_blocks[0] : cols.black;
    let b2 = blockOnBase(b, 0.75, col_b2, 'b2' + trial, true)

    // stimuli.push({'objs': [base, b], 'meta': ['', '', ''], id});
    stimuli.push({'objs': [base, b, b2], 'meta': ['', '', ''], id});

  }
  return stimuli
}
