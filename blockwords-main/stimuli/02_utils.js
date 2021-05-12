blockOnBase = function(base, propOnBase, color, label, horiz=false, wh={}) {
  let w,
      h;
  w = Object.keys(wh).length === 0 ?
    (horiz ? PROPS.blocks.h : PROPS.blocks.w) : wh.w;
  h = Object.keys(wh).length === 0 ?
    (horiz ? PROPS.blocks.w : PROPS.blocks.h) : wh.h;

  // when propOnBase is negative, block is put on left side of the base,
  // else right side
  let edge = propOnBase < 0 ? "min" : "max"
  let factor = propOnBase < 0 ? - propOnBase : (1-propOnBase)
  let x = base.bounds[edge]["x"] + factor * w - w / 2;

  let opts = Object.assign({'render': {'fillStyle': color}, label}, OPTS.blocks)
  return Bodies.rectangle(x, base.bounds.min.y - h / 2, w, h, opts);
}

// create a rectangle block at particular position
block = function(x, y_min_base, col, label, horiz=false, opts={}){
  let w = horiz ? PROPS.blocks.h : PROPS.blocks.w;
  let h = horiz ? PROPS.blocks.w : PROPS.blocks.h;
  opts = Object.assign(opts, {'render': {'fillStyle': col}, label}, OPTS.blocks)
  return Bodies.rectangle(x, y_min_base - h/2, w, h, opts);
}

wall = function(label, x, y, w=PROPS.walls.w, h=PROPS.walls.h, opts={}){
  let opt_vals = Object.assign({label}, OPTS.walls);
  opt_vals = opts.length != 0 ? Object.assign(opt_vals, opts) : opt_vals;
  return Bodies.rectangle(x, y, w, h, opt_vals);
}

ball = function(x, y, r, label, color, opts={}){
  opts = Object.assign({label, 'render': {'fillStyle': color}}, opts,OPTS.balls)
  return Bodies.circle(x, y, r, opts);
}

radians = function(angle){
  return (2 * Math.PI / 360) * angle;
}

// move = function(obj, pos_hit, angle, force){
//   let pos = pos_hit == "center" ? obj.position : {};
//   let x = Math.cos(radians(angle)) * force * obj.mass;
//   let y = Math.sin(radians(angle)) * force * obj.mass;
//   Body.applyForce(obj, pos, {x, y});
// }

sortConditions = function(conditions){
  let filtered = {};
  let iff = [];
  TRIAL_TYPES.forEach(function(rel){
    filtered[rel] = [];
  })
  conditions.forEach(function(arr){
    filtered[arr[2]].push(arr);
  });
  return filtered
}

/**
*@return Object with key-val pairs:
 independent: [[pa,pc,"independent"], ...]
 if2: [[pa,pc, "if2"], ...]
 if1: [[pa, pc, "if1"], ...]
**/
getConditions = function(){
  let keys = PRIOR.conditions;
  let probs = [];
  keys.forEach(function(p){
    let vals = new Array(keys.length).fill(p);
    probs = probs.concat(_.zip(vals, keys));
  });
  let combis = [];
  probs.forEach(function(ps){
    TRIAL_TYPES.forEach(function(r){
      combis.push(ps.concat(r))
    })
  })
  return sortConditions(combis);
}

assignColors = function(){
  let col1 = _.random(0, 1);
  let col2 = col1 === 1 ? 0 : 1;
  return [col1, col2];
}
