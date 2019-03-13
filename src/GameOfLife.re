[@bs.val] external std_out_write : string => unit = "process.stdout.write";

type cell =
  | Dead
  | Alive

type world = {
  width: int,
  height: int,
  data: array(cell)
};

type index = int;
type word_coord = {x : int, y :int};
type word_index = int;

let world_cord_to_index = ({height} : world, {x, y} : word_coord) : index => {
  y * height + x
};

let index_to_world_cord = ({width, height} : world, i : index) : word_coord => {
  {x: index % width,
   y: floor(index / width)}
};

let neighbors = ({height, width} : world, {x, y} : word_coord) : list(word_coord) => {
  let di = [-1, 0, 1];

  List.map(i => List.map( ii => (i, ii), di), di)
  |> List.flatten
  |> List.filter(c => { c != (0,0)})
  |> List.map( ((dx, dy)) => (dx + x, dy + y) )
  |> List.filter( ((x, y)) => {x >= 0 && y>= 0 && x < width && y < height})
  |> List.map( ((x, y)) => {x: x, y: y} )
};

let mk_world = (w : int, h :int) :world => {
  {
    width: w,
    height: h,
    data: Array.make(w*h, Dead)
  };
};

let mk_bar_world = () : world => {
  let o = Dead;
  let w = Alive;

  {
    width: 5,
    height: 5,
    data: [|
    o,o,o,o,o,
    o,o,w,o,o,
    o,o,w,o,o,
    o,o,w,o,o,
    o,o,o,o,o
    |]
  }
};

// let simulate = ({data}) => {
//   Array.to_list(data)
//     |> List.mapi( (i, c) => {
//       neighbors(data, c)
//       switch(c)
//       })
// }

let a_world = mk_bar_world(); // mk_world(10, 5)

let render = (world : world) : unit => {
  for (y in 0 to (world.height - 1)){
    std_out_write("|")
    for (x in 0 to (world.width - 1)){
      //Js.log("[" ++ string_of_int(y) ++ "," ++ string_of_int(x) ++ "]")
      switch(world.data[y * world.width + x]) {
        | Dead => std_out_write(" ")
        | Alive => std_out_write("X")
      }
    }
    std_out_write("|")
    std_out_write("\n")
  }
};

render(a_world)
