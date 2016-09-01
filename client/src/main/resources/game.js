
// ---

var game= (function() {
    
    // ---
    
    var Matrix= new Array;
    
    Matrix.instance = function (nrows, ncols, initial) {
          var a, row, col, mat = [];
          for (row = 0; row < nrows; row += 1) {
            a = [];
            for (col = 0; col < ncols; col += 1) {
              a[col] = 0;
            }
            mat[row] = a;
          }
          return mat;
        };


    // --- GAME STATE SETUP ---

    var Game = {};
    
    Game.dims= {};
    Game.dims.CELL_SIZE= 5;
    Game.dims.X=         350;
    Game.dims.Y=         350;
    Game.dims.NCOLS=     Game.dims.X / Game.dims.CELL_SIZE;
    Game.dims.NROWS=     Game.dims.Y / Game.dims.CELL_SIZE;
    Game.dims.delay=     700;                
    
    Game.state= {};
    Game.state.DEAD=    0;
    Game.state.ALIVE=   1;
                 
    Game.limes= {};
    Game.limes.MIN=   2; 
    Game.limes.MAX=   3;
    Game.limes.SPAWN= 3;
                 
    Game.grid = Matrix.instance(Game.dims.NROWS, Game.dims.NCOLS, 0);
    
    
    // ---
    
    var Canvas = document.getElementById("grid_canvas").getContext('2d');
    var Gentext = document.getElementById("gentext");

    
    // --- VIEW UPDATE ---

    function updateView(Game, Canvas) {
      for (var row = 0; row < Game.dims.NROWS; row++) {
        for (var col = 0; col < Game.dims.NCOLS; col++) {
          if (Game.grid[row][col] === Game.state.ALIVE) {
               Canvas.fillStyle = "#000";
          } else if (Game.grid[row][col] === Game.state.DEAD) {
               Canvas.fillStyle = "#fff";
          } else {
               Canvas.fillStyle = "#ff0000";
          }
          Canvas.fillRect(col * Game.dims.CELL_SIZE +1,
                          row * Game.dims.CELL_SIZE +1,
                          Game.dims.CELL_SIZE -1,
                          Game.dims.CELL_SIZE -1);
        }
      }

      console.log("updating...");
    };

    
    // --- GRID SETUP ---

    function drawGrid(Game, Canvas){
        
      var offset = Game.CELL_SIZE;

      for (var x = 0; x <= Game.dims.X; x += Game.dims.CELL_SIZE) {
        Canvas.moveTo(0.5 + x, 0);
        Canvas.lineTo(0.5 + x, Game.dims.Y);
      }
      for (var y = 0; y <= Game.dims.Y; y += Game.dims.CELL_SIZE) {
        Canvas.moveTo(0, 0.5 + y);
        Canvas.lineTo(Game.dims.X, 0.5 + y);
      }

      Canvas.strokeStyle = "#eed";
      Canvas.stroke();
    }
    
    

    // --- DRAW GAME ---

    function drawGame(gameHistory, i) {
      //console.log("drawing state " + i);
      Game.grid = gameHistory[i];
      //console.log("state is\n" + gameHistory[i]);
      //console.log("grid is\n" + Game.grid);
      Game.dims.NCOLS = Game.grid.length;
      Game.dims.NROWS = Game.grid[0].length;
      Game.dims.X = Game.dims.NCOLS * Game.dims.CELL_SIZE;
      Game.dims.Y = Game.dims.NROWS * Game.dims.CELL_SIZE;
      if (i == 0) {
        drawGrid(Game, Canvas)
      }
      Gentext.innerHTML = "Generation: " + i;
      updateView(Game, Canvas);
      setTimeout(function(){
                    drawGame(gameHistory, (i+1) % gameHistory.length); 
                  }, 
                  Game.dims.delay);
    }
    
    // ---
    
    return {Game:     Game,
            drawGame
           };
    
}());


// ---

console.log("*** " 
            + document.getElementsByTagName('script')[0].baseURI 
            + " loaded: rules.js" 
            +  " ***"
           );

// ---
