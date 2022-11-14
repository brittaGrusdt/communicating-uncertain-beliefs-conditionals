var Engine = Matter.Engine,
    Render = Matter.Render,
    World = Matter.World,
    Bodies = Matter.Bodies;
    Body = Matter.Body;
    Constraint = Matter.Constraint;
    Events = Matter.Events;

cols = {
      'pink_red': '#E74C3C',
      'red': '#ff0000',
      'turquois': '#00c9cc',
      'blue': '#2471A3',
      'royal': '#0496FF',
      'green': '#28B463',
      'very_green': '#8DDE19',
      'pinkish': "#D81159",
      'light_green': '#51DE19',
      'purple': '#AF7AC5',
      'brown': '#B05D3A',
      'grey': '#E3DFDC',
      'darkgrey': '#938F8E',
      'darkbrown': '#642F17',
      'black': '#191817',
      'olive': '#53553B',
      'orange': '#FF9B52',
      'lightyellow': '#FFC966',
      'bordeaux': '#D81159',
      'yellow': '#FFBC42',
      'darkgreen': '#119533',
      'darkred': '#8F2D56',
      'sienna': '#A0522D',
      'darkgreen': '#2C6131'
    };
cols.plank = cols.blue
cols.test_blocks = [cols.royal, cols.green];
cols.train_blocks = [cols.red, cols.yellow];

const BLOCK_COLS = {
  test: ['blue', 'green'],
  train: ['red', 'yellow']
}
const BLOCK_COLS_SHORT = {
  test: [BLOCK_COLS.test[0][0], BLOCK_COLS.test[1][0]],
  train: [BLOCK_COLS.train[0][0], BLOCK_COLS.train[1][0]]
}

const COLORS_BALL = {
  'test': cols.darkred,
  'train': [cols.darkred, cols.darkred, cols.darkred]
  };
