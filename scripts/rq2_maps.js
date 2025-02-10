function(el, x, choices) {
  
  console.log(choices);
  
  let myMap = this;
  
  let region = choices.region_choices.region,
      region_name = choices.region_choices.region_name,
      latitude = choices.region_choices.latitude,
      longitude = choices.region_choices.longitude,
      order_region = choices.order_choices.region,
      order_num = choices.order_choices.order_num,
      order_title = choices.order_choices.order_title,
      race_abbrev = choices.race_vars.abbrev,
      race_name = choices.race_vars.name,
      edu_abbrev = choices.edu_vars.abbrev,
      edu_name = choices.edu_vars.name;
      
  $(String.raw`<style>.collapsible-label span::before { content: "➕"; margin-right: 2.5px; margin-left: 5.5px; width: 13px; display: inline-block; transition: transform 0.25s ease-out; } .collapsible-label.active span::before { content: "➖" } .collapsible-label:not(.active) span::before { transform: rotate(90deg); } .leaflet-control-layers-base { width: 190px; } .leaflet-popup-content ul { margin: 0; padding: 0 15px; } .leaflet-popup-content p { border-left: 2px lightgray solid; color: gray; padding: 0 7px; margin: 2px 5px; }</style>`).appendTo('head');
  
  $('.leaflet-control-layers-base').prepend('<p style="margin: 5px; font-weight: 600;">Census data (by tract)</p>');
  $('.leaflet-control-layers-overlays').prepend('<p style="margin: 8px 5px 5px; font-weight: 600;">Student lists (by school)</p>');
  
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
  
  raceControlHTML = '<label class="collapsible-label" data-label="race" style="cursor: pointer;"><div><span style="font-weight: 500;"> MSA by Race/Ethnicity</span></div></label><div id="race-container" style="padding-left: 20px;"></div>';
  
  $('.leaflet-control-layers-base label:nth-child(3)').after(raceControlHTML);
  
  $('#race-container').append(raceOptions).slideUp(0);
  
  // purchased hs overlay layer
  
  let HSControlHTML = '<p style="margin: 0 18px; color: gray;"><span style="height: 4px; width: 4px; border-radius: 50%; display: inline-block; padding: 1px; margin: 0 8px 1px; background-color: orange;"></span>Public HS</p>' +
    '<p style="margin: 0 18px; color: gray;"><span style="height: 4px; width: 4px; border-radius: 50%; display: inline-block; padding: 1px; margin: 0 8px 1px; background-color: blue;"></span>Private HS</p>';
  
  let raceHSControlHTML = '<label class="collapsible-label" data-label="hs-race" style="cursor: pointer; margin-left: -1px;"><div><span style="font-weight: 500;"> by Race/Ethnicity</span></div></label><div id="hs-race-container" style="padding-left: 19px;">';
  
  race_name.forEach(function(curr, idx) {
    raceHSControlHTML += '<label><div><input type="radio" class="leaflet-control-layers-selector" name="race-choice" data-race="' + race_abbrev[idx] + '"><span> ' + curr + '</span></div></label>'
  });
  
  raceHSControlHTML += '</div>';
  
  let eduHSControlHTML = '<label class="collapsible-label" data-label="hs-edu" style="cursor: pointer; margin-left: -1px;"><div><span style="font-weight: 500;"> by Parental Education</span></div></label><div id="hs-edu-container" style="padding-left: 19px;">';
  
  edu_name.forEach(function(curr, idx) {
    eduHSControlHTML += '<label><div><input type="radio" class="leaflet-control-layers-selector" name="edu-choice" data-edu="' + edu_abbrev[idx] + '"><span> ' + curr + '</span></div></label>'
  });
  
  eduHSControlHTML += '</div>';
  
  $('.leaflet-control-layers-overlays').append(HSControlHTML + raceHSControlHTML + eduHSControlHTML);
  
  $('#hs-race-container').slideUp(0);
  $('#hs-edu-container').slideUp(0);
  
  // handle collapsible selection
  
  $('.collapsible-label').on('click', function(e) {
    $(this).toggleClass('active');
    $('#' + $(this).attr('data-label') + '-container').slideToggle();
  })
    
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

    myMap.setView([lng, lat], 8.5);
    
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
  
  $('input[name="race-choice"]').on('change', function(e) {
    active_attr.active_race = $(this).attr('data-race');
    update_pins();
  });
  
  $('input[name="edu-choice"]').on('change', function(e) {
    active_attr.active_edu = $(this).attr('data-edu');
    update_pins();
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
    
    // non-purchased pins
    $('.metro-' + active_attr.active_metro + '.order-' + active_attr.active_order).css('display', 'inherit');
    
    // purchased pins
    $('.metro-' + active_attr.active_metro + '.order-' + active_attr.active_order + '-' + active_attr.active_race + '-' + active_attr.active_edu).css('display', 'inherit');
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
    active_order: '',
    active_race: 'all',
    active_edu: 'all'
  };
  
  $('.legend, #metro-control, #order-control').css('display', 'none');
  
  $('input[data-region="' + active_attr.active_metro + '"]').trigger('click');
  $('input[data-race="' + active_attr.active_race + '"]').trigger('click');
  $('input[data-edu="' + active_attr.active_edu + '"]').trigger('click');

}
