$(document).ready(function(){
    /*$('.lollipop-slider').addClass('shiny-slider-element-ex').slider({
        id: 'test-slider',
        min: 0.0001,
        max: 1.0,
        scale: 'logarithmic',
        step: 0.0001,
        value: 0.1,
        tooltip: 'always'
    });
    
    $('.lollipop-slider2').slider({
        id: 'test-slider2'
        //min: 0,
        //max: 10,
        //step: 0.01,
       // range: true,
       // value: [5, 7],
       // tooltip: 'always'
    });*/
    
    $('.two-hit-slider').each(function(){
      var sliderId = $(this).attr('id') + '-display';
      var min = parseInt($(this).data('min'));
      var max = parseInt($(this).data('max'));
      $(this).slider({
        range: true,
        min: min,
        max: max,
        step: 100,
        value: [min, max],
        id: sliderId,
        tooltip: 'always'
      });
    }).on('shiny-server-return', function(e, data){
      var sliderProps = {};
      sliderProps[data.prop] = parseInt(data.value);
      $(this).slider(sliderProps);
      $(this).slider({
        value: [
          parseInt($(this).slider('getAttribute', 'min')),
          parseInt($(this).slider('getAttribute', 'max'))
        ]
      });
      $(this).slider('refresh').change();
    });
});



