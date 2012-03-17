var s99 = {};

(function() {
  s99.knight = s99.knight || {};

  s99.knight.utils = {
    drawBoard: function(canvas, size) {
      var context = canvas.getContext("2d"),
      caseWidth = canvas.width/size,
      caseHeight = canvas.height/size;

      for (var x = 0; x < size; x++) {
        for (var y = 0; y < size; y++) {
          context.fillStyle = ((x+y) %2 == 0) ? "khaki" : "gray";
          context.fillRect(x * caseWidth, y * caseHeight, caseWidth, caseHeight);
        }
      }
    },

    drawPath: function(canvas, path, size) {
      var context = canvas.getContext("2d"),
      caseWidth = canvas.width/size,
      caseHeight = canvas.height/size,
      middleCaseWidth = caseWidth/2,
      middleCaseHeight = caseHeight/2,
      start = _.head(path);

      context.strokeStyle = "black";
      context.moveTo(caseWidth * start[0] + middleCaseWidth, caseHeight * start[1] + middleCaseHeight);
      _.each(_.tail(path), function(pos) {
        context.lineTo(caseWidth * pos[0] + middleCaseWidth, caseHeight * pos[1] + middleCaseHeight);
      });
      context.stroke();
    }
  };

  var self = s99.knight.utils;
})();

(function() {
  s99.travelling = s99.travelling || {};

  s99.travelling.Path = function(hash, res) {
    this.hash = hash;
    this.res = res;
  };

  s99.travelling.Path.prototype = {
    draw: function(canvas) {
      var context = canvas.getContext("2d"),
      start = _.last(this.res.path),
      w = canvas.width,
      h = canvas.height;

      s99.utils.clearCanvas(canvas);
      this.drawCities(canvas);

      context.beginPath();
      context.strokeStyle = "black";
      context.moveTo(w * start.x, h * start.y);
      _.each(this.res.path, function(pos) {
        context.lineTo(w * pos.x, h * pos.y);
      });
      context.stroke();
    },
    drawCities: function(canvas) {
      var context = canvas.getContext("2d"),
      w = canvas.width,
      h = canvas.height;

      context.fillStyle = "green";
      _.each(this.res.path, function(pos) {
        context.beginPath();
        context.arc(w * pos.x, h * pos.y, 2, 0, 2 * Math.PI, true);
        context.fill();
      });
    },
    toString: function() {
      return "Hash : " + this.hash + ", Seed : " + this.res.seed +
        ", Distance : " + this.res.distance;
    }
  };

  var self = s99.travelling.Path;
})();

(function() {
  s99.utils = s99.utils || {};

  s99.utils = {
    clearCanvas: function(canvas) {
      var context = canvas.getContext("2d");
      context.save();
      context.setTransform(1, 0, 0, 1, 0, 0);
      context.clearRect(0, 0, canvas.width, canvas.height);
      context.restore();
    }
  }
})();

