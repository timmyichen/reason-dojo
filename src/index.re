open Reprocessing;

type obj = {
  rotation: float,
  x: float,
  y: float,
  dx: float,
  dy: float,
};

type star = {
  x: float,
  y: float,
  layer: int,
};

type state = {
  frame: int,
  player: obj,
  asteroids: list(obj),
  lost: bool,
  stars: list(star),
  showStars: bool,
};

let drag = 0.1;

let player = {rotation: 0., x: 200., y: 200., dx: 0., dy: 0.};

let getInitialState = () => {
  frame: 0,
  player,
  asteroids:
    Array.init(3, idx =>
      {
        x: float_of_int(idx + 1) *. 150.,
        y: float_of_int(idx + 1) *. 150.,
        rotation: 0.,
        dx: Random.float(5.),
        dy: Random.float(5.),
      }
    )
    |> Array.to_list,
  lost: false,
  stars:
    Array.init(100, _idx =>
      {
        x: Random.float(1000.0),
        y: Random.float(1000.0),
        layer: Random.int(3),
      }
    )
    |> Array.to_list,
  showStars: false,
};

let rotation = 0.0;

let setup = env: state => {
  Env.size(~width=600, ~height=600, env);

  getInitialState();
};

let log = print_endline;
let logState = list =>
  List.fold_left(
    (str, num) => str ++ " " ++ string_of_float(num),
    "",
    list,
  );

let updatePos = (obj: obj) => {
  ...obj,
  x:
    switch (obj.x) {
    | x when x > 600. => 0.
    | x when x < 0. => 600.
    | x => x +. obj.dx
    },
  y:
    switch (obj.y) {
    | y when y > 600. => 0.
    | y when y < 0. => 600.
    | y => y +. obj.dy
    },
};

let draw = (state, env) => {
  let {player: {x, y}} = state;
  Draw.background(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  state.showStars ?
    state.stars
    |> List.iter(({x, y, layer}) => {
         let intensity =
           switch (layer) {
           | 0 => 50.0
           | 1 => 100.0
           | 2 => 150.0
           | _ => 5.0
           };

         let lIntensity =
           switch (layer) {
           | 0 => 0.1
           | n => float_of_int(n)
           };

         let sx =
           mod_float(
             x
             +. (state.player.dx +. lIntensity)
             *. float_of_int(state.frame)
             +. 100.,
             600.0,
           );
         let sy =
           mod_float(
             y +. (state.player.dy +. lIntensity) *. float_of_int(state.frame),
             600.0,
           );

         print_endline(string_of_float(sx) ++ " " ++ string_of_float(sy));

         Draw.pixelf(
           ~pos=(sx, sy),
           ~color={r: intensity, g: intensity, b: intensity, a: 1.0},
           env,
         );
       }) :
    ();

  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.rotate(state.player.rotation, env);
  Draw.trianglef(
    ~p1=(20., 0.),
    ~p2=((-20.), (-10.)),
    ~p3=((-20.), 10.),
    env,
  );
  Draw.popMatrix(env);
  List.iter(
    (asteroid: obj) => {
      Draw.pushMatrix(env);
      Draw.translate(~x=asteroid.x, ~y=asteroid.y, env);
      Draw.rotate(asteroid.rotation, env);
      Draw.quad(
        ~p1=((-50), (-50)),
        ~p2=((-50), 50),
        ~p3=(50, 50),
        ~p4=(50, (-50)),
        env,
      );
      Draw.popMatrix(env);
    },
    state.asteroids,
  );

  /*
   log(logState([state.dx, state.dy, state.rotation]));
   Unix.sleep(1);
   */

  List.fold_left(
    (state, f) => f(state),
    state,
    [
      ({player: {rotation} as player} as state) =>
        Env.key(Right, env) ?
          {
            ...state,
            player: {
              ...state.player,
              rotation: state.player.rotation +. 0.1,
            },
          } :
          state,
      state =>
        Env.key(Left, env) ?
          {
            ...state,
            player: {
              ...state.player,
              rotation: state.player.rotation -. 0.1,
            },
          } :
          state,
      state =>
        Env.key(Up, env) ?
          {
            ...state,
            player: {
              ...state.player,
              dx: state.player.dx +. cos(state.player.rotation),
              dy: state.player.dy +. sin(state.player.rotation),
            },
          } :
          state,
      state =>
        Env.key(Down, env) ?
          {
            ...state,
            player: {
              ...state.player,
              dx: state.player.dx -. cos(state.player.rotation),
              dy: state.player.dy -. sin(state.player.rotation),
            },
          } :
          state,
      state => {
        player: updatePos(state.player),
        asteroids: List.map(updatePos, state.asteroids),
        lost: false,
        frame: state.frame + 1,
        stars: state.stars,
        showStars: state.showStars,
      },
      /*
       state => {
         ...state,
         dx: max(state.dx -. cos(drag), 0.),
         dy: max(state.dy -. sin(drag), 0.),
       },
       */
      state =>
        Env.key(Enter, env) ?
          {...state, showStars: !state.showStars} : state,
      state => Env.key(Space, env) ? getInitialState() : state,
    ],
  );
};

run(~setup, ~draw, ());