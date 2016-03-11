var hiddenInputOutput = new Shiny.OutputBinding();
$.extend(hiddenInputOutput, {
  find: function(scope) {
    return $(scope).find('.hidden-input-output');
  },
  getId: function(el) {
    return $(el).attr('id');
  },
  renderValue: function(el, data) {
    $(el).attr('data-value', data);
    idSplit = $(el).attr('id').split('_');
    $($(el).data('target')).trigger('shiny-server-return', {
      prop: $(el).data('prop'),
      value: data
    });
  },
  renderError: function(el, err) {
    $(el).val(err.message);
  },
  clearError: function(el) {
    $(el).val('');
  }
});
Shiny.outputBindings.register(hiddenInputOutput, 'twoHitProject.hiddenInputOutput');
console.log('hidden input bound');
