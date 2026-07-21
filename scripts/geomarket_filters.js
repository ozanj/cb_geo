function(el, x) {
  
  let myMap = this;
      
  $(String.raw`<style>.leaflet-control-layers label { font-weight: 500; margin-bottom: 0; } .collapsible-label span::before { content: "➕"; margin-right: 2.5px; margin-left: 5.5px; width: 13px; display: inline-block; transition: transform 0.25s ease-out; } .collapsible-label.active span::before { content: "➖" } .collapsible-label:not(.active) span::before { transform: rotate(90deg); } .leaflet-popup-content ul { margin: 0; padding: 0 15px; } .leaflet-popup-content p { border-left: 2px lightgray solid; color: gray; padding: 0 7px; margin: 2px 5px; }</style>`).appendTo('head');

  // handle collapsible selection
  
  $('.collapsible-label').on('click', function(e) {
    $(this).toggleClass('active');
    $('#' + $(this).attr('data-label') + '-container').slideToggle();
  })
  
  let update_base_layer = function() {
    $('.metro-shape').css('display', 'none');
    
    $('.metro-shape.metro-' + active_attr.active_metro).css('display', 'inherit');
    
    if (active_attr.active_base !== 'MSA') {
      $('.metro-line-' + active_attr.active_metro).css('display', 'inherit');
    }
  };
  
  // handle legend update
  
  let update_legend = function(sel_name) {
    $('.legend').css('display', 'none');
    switch (active_attr.active_base) {
      case 'MSA by Total Population':
        $('.legend-pop-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case 'MSA by Median Income':
        $('.legend-income-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% White, non-Hispanic':
        $('.legend-nhisp_white-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% Black, non-Hispanic':
        $('.legend-nhisp_black-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% Hispanic':
        $('.legend-hisp_all-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% Asian, non-Hispanic':
        $('.legend-nhisp_asian-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% NHPI, non-Hispanic':
        $('.legend-nhisp_nhpi-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% AIAN, non-Hispanic':
        $('.legend-nhisp_native-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% 2+ Races, non-Hispanic':
        $('.legend-nhisp_multi-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case 'MSA by % in Poverty':
        $('.legend-pov-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case 'MSA by % with BA+':
        $('.legend-edu-' + active_attr.active_metro).css('display', 'inherit');
        break;
    }
  };
  
  // handle controls selection

  myMap.on('baselayerchange', function(e) {
    active_attr.active_base = e.name;
    update_legend();
    
    e.layer.bringToBack();
    update_base_layer();
  });
  
  // default settings on load
      
  let active_attr = {
    active_base: 'MSA',
    active_metro: 'metro'
  };
  
  $('.legend').css('display', 'none');
  $('.label').css('opacity', 0);
  
  $('.legend-pop-' + active_attr.active_metro).css('display', 'inherit');
}
