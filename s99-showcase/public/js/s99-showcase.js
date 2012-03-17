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
  }

  var self = s99.knight.utils;
})();

