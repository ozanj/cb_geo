function(el, x, choices) {
  
  console.log(choices);
  
  let myMap = this;
  
  let region = choices.region_choices.region,
      region_name = choices.region_choices.region_name,
      latitude = choices.region_choices.latitude,
      longitude = choices.region_choices.longitude;
      
  $(String.raw`<style>#race-label span::before { content: "➕"; margin-right: 2.5px; margin-left: 5.5px; width: 13px; display: inline-block; transition: transform 0.25s ease-out; } #race-label.active span::before { content: "➖" } #race-label:not(.active) span::before { transform: rotate(90deg); } .leaflet-control-layers-base { width: 190px; }</style>`).appendTo('head');
  
  // buttons
  
  $('.easy-button-button').css('width', 'auto');
  $('.easy-button-button .button-state .fa').css({'float': 'left', 'margin-top': '8px'});
  
  $('button[title="Select Metro Area"] .button-state').append('<span style="display: inline-block; float: left; padding-left: 5px;">Select Metro Area</span>');
  
  // metro selection options
  
  let metroControlHTML = '<div id="metro-control" class="leaflet-control-layers leaflet-control leaflet-control-layers-expanded custom-control" style="width: auto; height: auto;">';
  
  region_name.forEach(function(curr, idx) {
    metroControlHTML += '<div><input type="radio" class="leaflet-control-layers-selector" name="metro-choice" data-region="' + region[idx] + '" data-lat="' + latitude[idx] + '" data-lng="' + longitude[idx] + '"><span> ' + curr + '</span></div>';
  });
    
  metroControlHTML += '</div>';
  
  $('button[title="Select Metro Area"]').parent().after(metroControlHTML);
  
  // year selection options
  
  let yearControlHTML = '<p style="margin: 5px; font-weight: 600;">Year</p><div style="display: flex;">';
  
  ['1980', '2000', '2020'].forEach(function(curr, idx) {
    yearControlHTML += '<div' + (idx === 0 ? '' : ' style="margin-left: 5px;"') + '><input type="radio" class="leaflet-control-layers-selector" name="year-choice" data-year="' + curr + '"><span>' + curr + '</span></div>';
  });
  
  yearControlHTML += '</div><p style="margin: 5px; font-weight: 600;">Variables</p>';
  
  $('.leaflet-control-layers-base').prepend(yearControlHTML);
  
  // level selection options
  
  let levelControlHTML = '<p style="margin: 5px; font-weight: 600;">Level</p><div style="display: flex; margin-bottom: 5px;">';
  
  ['EPS', 'Tract'].forEach(function(curr, idx) {
    levelControlHTML += '<div' + (idx === 0 ? '' : ' style="margin-left: 5px;"') + '><input type="radio" class="leaflet-control-layers-selector" name="level-choice" data-level="' + curr.toLowerCase() + '"><span>' + curr + '</span></div>';
  });
  
  levelControlHTML += '</div>';
  
  $('.leaflet-control-layers-base').append(levelControlHTML);
  
  // race/ethnicity selection options
  
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
    
  // selection text
  
  selTextHTML = '<div id="selection-text" style="padding: 10px; display: inline-block; font-weight: 900; color: #444;"></div>';
  
  $('.leaflet > .leaflet-control-container > .leaflet-top.leaflet-left').append(selTextHTML);
  
  // handle selections
    
  $('input[name="metro-choice"]').on('change', function(e) {
    let $this = $(this);
    
    let metro = $this.attr('data-region'),
        lat = $this.attr('data-lat'),
        lng = $this.attr('data-lng');
        
    active_attr.active_metro = metro;
    update_base_layer();
    
    myMap.setView([lng, lat], 8.2);
    
    update_sel_text();
    update_legend();
  });
  
  $('input[name="year-choice"]').on('change', function(e) {
    let $this = $(this);
    
    let year = $this.attr('data-year');
    
    active_attr.active_year = year;
    update_base_layer();
    
    update_sel_text();
  });
  
  $('input[name="level-choice"]').on('change', function(e) {
    let $this = $(this);
    
    let level = $this.attr('data-level');
    
    active_attr.active_level = level;
    update_base_layer();
    
    update_legend();
  });
  
  let update_base_layer = function() {
    $('.metro-shape').css('display', 'none');
    
    if (active_attr.active_base === 'MSA') {
      $('.metro-' + active_attr.active_metro + '.metro-line').css('display', 'inherit');
    } else {
      $('.metro-' + active_attr.active_metro + '.level-' + active_attr.active_level + '.year-' + active_attr.active_year).css('display', 'inherit');
    }
  };
  
  // disable options
  
  $('input').on('change', function(e) {
    $('input').prop('disabled', false);
    
    if (active_attr.active_base === 'MSA') {
      if (!$('input[data-year]:checked').length) {
        $('input[data-year="1980"]').trigger('click');
      }
      if (!$('input[data-level]:checked').length) {
        $('input[data-level="eps"]').trigger('click');
      }
      
      $('input[data-year], input[data-level]').prop('disabled', true);
    }
    
    if (['% Asian, non-Hispanic', '% NHPI, non-Hispanic', '% AIAN, non-Hispanic', '% 2+ Races, non-Hispanic'].includes(active_attr.active_base)) {
      $('input[data-year="1980"]').prop('disabled', true);
      if (active_attr.active_year === '1980') {
        $('input[data-year="2000"]').trigger('click');
      }
    }
  })
  
  // handle selection text update
  
  let update_sel_text = function() {
    let sel_metro = $('input[data-region="' + active_attr.active_metro + '"]').next().text(),
      sel_year = active_attr.active_year;
    $('#selection-text').text(sel_metro + ' in ' + sel_year);
  };
  
  // handle legend update
  
  let update_legend = function(sel_name) {
    $('.legend').css('display', 'none');
    switch (active_attr.active_base) {
      case 'MSA by Total Population':
        $('.legend-pop-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case 'MSA by Median Income':
        $('.legend-income-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case '% White, non-Hispanic':
        $('.legend-nhisp_white-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case '% Black, non-Hispanic':
        $('.legend-nhisp_black-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case '% Hispanic':
        $('.legend-hisp_all-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case '% Asian, non-Hispanic':
        $('.legend-nhisp_asian-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case '% NHPI, non-Hispanic':
        $('.legend-nhisp_nhpi-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case '% AIAN, non-Hispanic':
        $('.legend-nhisp_native-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case '% 2+ Races, non-Hispanic':
        $('.legend-nhisp_multi-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case 'MSA by % in Poverty':
        $('.legend-pov-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
      case 'MSA by % with BA+':
        $('.legend-edu-' + active_attr.active_metro + '-' + active_attr.active_level).css('display', 'inherit');
        break;
    }
  };
  
  // handle base layer selection

  myMap.on('baselayerchange', function(e) {
    active_attr.active_base = e.name;
    update_legend();
    
    e.layer.bringToBack();
    update_base_layer();
  });
  
  // default settings on load
      
  let active_attr = {
    active_base: 'MSA',
    active_metro: region[0],
    active_level: 'eps',
    active_year: '1980'
  };
  
  $('.legend, #metro-control').css('display', 'none');
  
  $('input[data-region="' + active_attr.active_metro + '"]').trigger('click');
  $('input[data-year="' + active_attr.active_year + '"]').trigger('click');
  $('input[data-level="' + active_attr.active_level + '"]').trigger('click');
  
}
