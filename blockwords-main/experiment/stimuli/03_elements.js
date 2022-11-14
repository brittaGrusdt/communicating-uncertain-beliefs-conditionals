 let Walls = {'test': {}, 'train': {}, 'tilted': {}};

// ground of scene
const Bottom = wall(label='bottom', x=SCENE.w/2, y=SCENE.h - PROPS.bottom.h/2,
  w=SCENE.w, h=PROPS.bottom.h);

// INDEPENDENT TRIALS
makeRamp = function(dir, prior, increase, w1, label1="bottom", test=true,
  col_ball=COLORS_BALL.test) {

  let angle = ANGLES.default;
  let overlap = OVERLAP_SHIFT["angle" + angle];
  let label2 = label1 === "bottom" ? "top" : "bottom";

  let dat = increase && label1 === "top" ? {rx: "min", ry: "min", x: -1, y: 1} :
            increase && label1 === "bottom" ? {rx: "max", ry: "max", x: 1, y: -1} :
            label1 === "top" ? {rx: "max", ry: "min", x: 1, y: 1}
                             : {rx: "min", ry: "max", x: -1, y: -1};
  // 1. sin(angle) = h/w_tillted and 2. h² + w_low² = ramp²
  let r = increase ? radians(360 - angle) : radians(angle);
  // always use the same angle + width of the ramp!
  let factor = 1.2
  // let factor = dir == "horizontal" ? 1.5 : 0.9;
  let ramp_width = factor * (Math.sqrt(Math.pow(100, 2) / (1 - Math.pow(Math.sin(r), 2))));
  let ramp = wall('ramp' + angle, w1.bounds[dat.rx].x + dat.x * ramp_width/2,
    w1.bounds[dat.ry].y + dat.y * PROPS.walls.h/2, ramp_width);
  Body.rotate(ramp, r, {x: w1.bounds[dat.rx].x, y: w1.bounds[dat.ry].y});

  let width_w2 = label1 == "bottom" ? BASE_RAMP.default : BASE_RAMP[dir][prior]
  let x2 = dat.x == 1 ? "max" : "min";
  let y2 = dat.y == 1 ? "max" : "min";
  let w2 = wall(label = 'ramp_' + label2 + angle,
    ramp.bounds[x2].x + dat.x * width_w2/2 - dat.x * overlap,
    ramp.bounds[y2].y - dat.y * PROPS.walls.h/2, width_w2);
  dat.walls = label1==="bottom" ? {'top': w2, 'bottom': w1} : {'top': w1, 'bottom': w2};
  dat.x_ball = increase ? dat.walls.top.bounds.min.x - PROPS.balls.move_to_roll : dat.walls.top.bounds.max.x + PROPS.balls.move_to_roll;
  let ball1 = ball(dat.x_ball, dat.walls.top.bounds.min.y - PROPS.balls.radius,
    PROPS.balls.radius, 'ball1', col_ball);

  return {'tilted': ramp, 'wall_top': dat.walls.top,
   'wall_bottom': dat.walls.bottom, 'ball': ball1, 'angle': angle}
}

seesaw = function(x, y_base_min=SCENE.h - PROPS.bottom.h, props={}){
  props = Object.keys(props).length == 0 ? PROPS.seesaw :
    Object.assign({}, PROPS.seesaw, props);
  let y = y_base_min - props.stick.h / 2;
  let stick = wall('stick', x, y,
    props.stick.w, props.stick.h, {render: {fillStyle: cols.darkgrey}});

  let link = wall('link', x, stick.bounds.min.y - props.link.h/2,
    props.link.w, props.link.h, {render: {fillStyle: cols.darkbrown}}
  );
  let skeleton = Body.create({'parts': [stick, link],
    'isStatic': true,
    'label': 'skeleton'
  });
  let def_ss = props;
  let y_plank = link.bounds.min.y - def_ss.plank.h/2;
  let opts = Object.assign({label:'plank', render:{fillStyle: cols.plank}}, OPTS.plank);
  let plank = Bodies.rectangle(x, y_plank, def_ss.plank.w, def_ss.plank.h, opts)

  var constraint = Constraint.create({
    pointA: {x: plank.position.x, y: plank.position.y},
    bodyB: plank,
    stiffness: 0.7,
    length: 0
  });
  return {stick, link, skeleton, plank, constraint}
}

// The first two list entries are respectively the bases for block1 and block2
wallsIf1 = function(side, horiz, prior){
  let x_up_r = 700
  let x_up_l = 100
  let dat = side == "right" ?
    {w_up: wall('wall_ac_up', x_up_r, 90, 150), x_up: x_up_r, move_x: 1, increase: true} :
    {w_up:  wall('wall_ac_up', x_up_l, 90, 150), x_up: x_up_l, move_x: -1, increase: false};
  let base_ssw = wall('base_seesaw', dat.x_up+225*(-dat.move_x), 185, PROPS.if1_base_ssw.w,
                      PROPS.if1_base_ssw.h);
  let x = base_ssw.position.x + dat.move_x * 40 + dat.move_x * PROPS.walls.w/2
  let y = base_ssw.position.y + 67;
  let ramp_top = wall('ramp_top', x, y);
  let ramp = makeRamp(horiz, prior, dat.increase, ramp_top, "top")
  Body.setPosition(ramp.ball, {x: ramp.ball.position.x + 40 * dat.move_x,
    y: ramp.ball.position.y});
  let ssw = seesaw(base_ssw.position.x, base_ssw.bounds.min.y, PROPS.if1_ssw);
  return {walls: [dat.w_up, ramp.wall_bottom, ramp_top, ramp.tilted,
                  base_ssw, ssw.skeleton],
          dynamic: [ramp.ball, ssw.plank, ssw.constraint]}
}

seesawIf2 = function(prior, dir, side_ramp, offset=PROPS.seesaw.d_to_walls){
  let y_low = 220;
  let y_high = 170;
  let w_w2blocks = PROPS.seesaw.base2blocks.w;
  let data = side_ramp === "right" ?
    {x0: 75, y0: y_low, w0: w_w2blocks, y1: y_high, w1: BASE_RAMP[dir][prior]} :
    {x0: 300, y0: y_high, w0: BASE_RAMP[dir][prior], y1: y_low, w1: w_w2blocks};
  let base0 = wall('seesaw_base_left', data.x0, data.y0, data.w0);
  let pos = base0.bounds.max.x + PROPS.seesaw.plank.w/2 + offset;
  let objs = seesaw(pos);
  let base1 = wall('seesaw_base_right',
    objs.plank.bounds.max.x + offset + data.w1/2, data.y1, data.w1);
  let walls = [base0, base1].concat([objs.skeleton]);
  return {'walls': walls, 'dynamic': [objs.plank, objs.constraint]}
}

Walls.test = {
  'independent': [[wall('w_up1', 280, 100), wall('w_low1', 290, 250, w=100)],
                  [wall('w_up2', 520, 100), wall('w_low2', 510, 250, w=100)]],
  'if1': wallsIf1,
  'if2': seesawIf2
};

//// Elements for TRAINING TRIALS //////
Walls.train.uncertain = [
  wall('w_mid_left', 0.3 * SCENE.w, SCENE.h/2, PROPS.seesaw.base2blocks.w),
  wall('w_mid_right', 0.75 * SCENE.w, SCENE.h/2, PROPS.seesaw.base2blocks.w)
];

Walls.train.steepness = [wall('w_bottom1', SCENE.w/2, 100),
  wall('w_bottom2', SCENE.w/2, 220),
  wall('w_bottom3', SCENE.w/2, 340)];

Walls.train.distance0 = [
  wall('w_top1', 150, 50),
  wall('w_top2', 150, 160),
  wall('w_top2', 150, 280)
];

Walls.train.distance1 = [
  wall('w_top1', 150, 50),
  wall('w_top2', 150, 160),
  wall('w_top3', 150, 280)
];

Walls.train.independent = [
  wall('ramp_top', 100, 75),
  wall('w_right', 750, 140, 90)
];
Walls.train.if1 = wallsIf1

Walls.train.ssw = function(){
  let x = SCENE.w/2 - 30
  let pw = 280
  let objs = seesaw(x, SCENE.h - PROPS.bottom.h,
    props={'plank': {'w': pw, 'h': 10}});

  let bw = PROPS.seesaw.base2blocks.w
  let walls = [
    wall('wallTopLeft', 150, 155, 140),
    wall('wall_seesaw_right', x+pw/2+PROPS.seesaw.d_to_walls+bw/2, 240, bw)
  ].concat([objs.skeleton]);
  return {'walls': walls, 'dynamic': [objs.plank, objs.constraint]}
}
