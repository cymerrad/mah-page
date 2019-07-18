// module Main

exports.renderObject = function renderObject(o) {
  return function() {
    var el = document.getElementById(o.id);
    el.setAttribute("class", o.css);
    el.setAttribute(
      "style",
      "left: " +
        o.position.x +
        "px; top: " +
        Math.max(o.position.y - el.offsetHeight, 0) +
        "px"
    );
  };
};

exports.width = (function() {
  return Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
})();
exports.height = (function() {
  return Math.max(
    document.documentElement.clientHeight,
    window.innerHeight || 0
  );
})();
