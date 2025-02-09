function(el, x, choices) {
  
  console.log(choices);
  
  let myMap = this;
  
  let region = choices.region_choices.region,
      region_name = choices.region_choices.region_name,
      latitude = choices.region_choices.latitude,
      longitude = choices.region_choices.longitude,
      order_region = choices.order_choices.region,
      order_num = choices.order_choices.order_num,
      order_title = choices.order_choices.order_title;
      
  $(String.raw`<style>.no-fill { fill-opacity: 0; } #race-label span::before { content: "➕"; margin-right: 2.5px; margin-left: 5.5px; width: 13px; display: inline-block; transition: transform 0.25s ease-out; } #race-label.active span::before { content: "➖" } #race-label:not(.active) span::before { transform: rotate(90deg); } .leaflet-control-layers-base { width: 190px; }</style>`).appendTo('head');
  
  $('.leaflet-control-layers-base').prepend('<p style="margin: 5px; font-weight: 600;">Variables</p>');
  
  // buttons
  
  $('.easy-button-button').css('width', 'auto');
  $('.easy-button-button .button-state .fa').css({'float': 'left', 'margin-top': '8px'});
  
  $('button[title="Select Metro Area"] .button-state').append('<span style="display: inline-block; float: left; padding-left: 5px;">Select Metro Area</span>');
  $('button[title="Select Order Number"] .button-state').append('<span style="display: inline-block; float: left; padding-left: 5px;">Select Order Number</span>');
  
  // metro selection options
  
  let metroControlHTML = '<div id="metro-control" class="leaflet-control-layers leaflet-control leaflet-control-layers-expanded custom-control" style="width: auto; height: auto;">';
  
  region_name.forEach(function(curr, idx) {
    metroControlHTML += '<div><input type="radio" class="leaflet-control-layers-selector" name="metro-choice" data-region="' + region[idx] + '" data-lat="' + latitude[idx] + '" data-lng="' + longitude[idx] + '"><span> ' + curr + '</span></div>';
  });
    
  metroControlHTML += '</div>';
  
  $('button[title="Select Metro Area"]').parent().after(metroControlHTML);
  
  // order selection options
  
  let orderControlHTML = '<div id="order-control" class="leaflet-control-layers leaflet-control leaflet-control-layers-expanded custom-control" style="width: auto; height: auto;"></div>';
  
  $('button[title="Select Order Number"]').parent().after(orderControlHTML);
  
  // race/ethnicity base layer
  
  let raceOptions = $('.leaflet-control-layers-base label').filter(function() {
    return $(this).text().trim().startsWith('%');
  });
  
  raceControlHTML = '<label id="race-label" style="cursor: pointer;"><div><span style="font-weight: 500;"> MSA by Race/Ethnicity</span></div></label><div id="race-container" style="padding-left: 20px;"></div>';
  
  $('.leaflet-control-layers-base label:nth-child(5)').after(raceControlHTML);
  
  $('#race-container').append(raceOptions).slideUp();
  
  $('#race-label').on('click', function(e) {
    $(this).toggleClass('active');
    $('#race-container').slideToggle();
  })
  
  // race/ethnicity overlay layer
  
  $('.leaflet-control-layers-overlays label').each(function(idx, curr) {
    if (idx >= 2) {
      $(this).css('margin-left', '20px');
      $(this).find('input').attr('name', 'purchased-race');
      $(this).find('div').append('<span class="dot" style="background-color: ' + choices.pin_colors[$(this).text().trim()] + '; height: 4px; width: 4px; border-radius: 50%; display: inline-block; padding: 1px; margin: 0 0 1px 5px;"></span>');
      $(this).find('.dot').fadeOut(0);
    }
  });
  
  $('input[name="purchased-race"]').on('change', function(e) {
    handleFirstGen();
    $(this).siblings('.dot').fadeToggle(50);
  });
  
  $('.leaflet-control-layers-overlays label:nth-child(2)').on('change', function(e) {
    handleFirstGen();
  });
  
  // first-gen fill option
  
  $('.leaflet-control-layers-overlays').after('<div class="leaflet-control-layers-separator"></div><div><label><div><input type="checkbox" class="leaflet-control-layers-selector" name="first-gen"><span> First-generation</span></div></label></div>');
  
  $('input[name="first-gen"]').on('change', function(e) {
    handleFirstGen();
  });
  
  let handleFirstGen = function() {
    if ($('input[name="first-gen"]').is(':checked')) {
      $('.order-pin').removeClass('no-fill');
    } else {
      $('.order-pin').addClass('no-fill');
    }
  }
    
  // selection text
  
  selTextHTML = '<div id="selection-text" style="padding: 10px; display: inline-block; font-weight: 900; color: #444;"></div>';
  
  $('.leaflet > .leaflet-control-container > .leaflet-top.leaflet-left').append(selTextHTML);
  
  // handle selections
  
  let updateOrderOptions = function() {
    orderOptionsHTML = '';
    order_region.forEach(function(curr, idx) {
      if (curr === active_attr.active_metro) {
        orderOptionsHTML += '<div><input type="radio" class="leaflet-control-layers-selector" name="order-choice" data-order="' + order_num[idx] + '"><span> ' + order_num[idx] + ': ' + order_title[idx] + '</span></div>';
      }
    });
    
    $('#order-control').html(orderOptionsHTML);
    
    active_attr.active_order = $('#order-control div:first-child input').attr('data-order');
    $('input[data-order="' + active_attr.active_order + '"]').trigger('click');
  }
    
  $('input[name="metro-choice"]').on('change', function(e) {
    let $this = $(this);
    
    let metro = $this.attr('data-region'),
        lat = $this.attr('data-lat'),
        lng = $this.attr('data-lng');
        
    active_attr.active_metro = metro;
    update_base_layer();

    myMap.setView([lng, lat], 8.2);
    
    updateOrderOptions();
    update_pins();
    
    update_sel_text();
    update_legend();
  });
    
  $(document).on('change', 'input[name="order-choice"]', function(e) {
    let $this = $(this);
    
    let order = $this.attr('data-order');
    active_attr.active_order = order;
    
    update_pins();
    update_sel_text();
  });
  
  let update_base_layer = function() {
    $('.metro-shape').css('display', 'none');
    
    $('.metro-shape.metro-' + active_attr.active_metro).css('display', 'inherit');
    
    if (active_attr.active_base !== 'MSA') {
      $('.metro-line-' + active_attr.active_metro).css('display', 'inherit');
    }
  };
  
  let update_pins = function() {
    $('.order-pin').css('display', 'none');
    
    $('.metro-' + active_attr.active_metro + '.order-' + active_attr.active_order).css('display', 'inherit');
  }
  
  // handle selection text update
  
  let update_sel_text = function() {
    let sel_metro = $('input[data-region="' + active_attr.active_metro + '"]').next().text();
    
    if (active_attr.active_order !== '') {
      sel_metro += ' - ' + $('input[data-order="' + active_attr.active_order + '"]').next().text();
    }
    
    $('#selection-text').text(sel_metro);
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
  
  myMap.on('overlayadd', function(e) {
    update_pins();
  });
  
  // default settings on load
      
  let active_attr = {
    active_base: 'MSA',
    active_metro: region[0],
    active_order: ''
  };
  
  $('.legend, #metro-control, #order-control').css('display', 'none');
  
  $('input[data-region="' + active_attr.active_metro + '"]').trigger('click');
  handleFirstGen();
  
}
