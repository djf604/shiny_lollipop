var twoHitSlider = new Shiny.InputBinding();
$.extend(twoHitSlider, {
  find: function(scope) {
    return $(scope).find('.two-hit-slider');
  },
  getId: function(el) {
    return $(el).attr('id');
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
    $(el).on('change.twoHitSlider', function(e) {
      callback(true);
    });
  },
  unsubscribe: function(el) {
    $(el).off('.twoHitSlider');
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 500
    }
  }
});
Shiny.inputBindings.register(twoHitSlider, 'twoHitProject.twoHitSlider');