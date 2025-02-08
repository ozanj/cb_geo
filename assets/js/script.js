$(function() {

  const modal = '<div class="modal"><span class="close">&times;</span><img /></div>';
  $('.slides section').prepend(modal);
  
  const link = '<a class="link" href="https://ozanj.github.io/cb_geo/">ozanj.github.io/cb_geo</a>';
  $('.slides section.level3').prepend(link);
  
  $('.slides section img').on('click', function() {
    let src = $(this).attr('src');
    
    var $slide = $(this).closest('.slide');
    $slide.find('.modal img').attr('src', src);
    
    $(this).closest('.slide').find('.modal').fadeIn(600);
    $(this).closest('.slide').find('p img').addClass('disabled');
  });
  
  $('.close').on('click', function() {
    $(this).closest('.slide').find('.modal').fadeOut();
    $(this).closest('.slide').find('p img').removeClass('disabled');
  });
  
  $('<hr>').insertAfter('.reveal .slide h4');
  $('<hr>').insertAfter('.reveal .slide:not(.caption) h3:not(:has(+ h4))');
  
  Reveal.addEventListener('slidechanged', function() {
    $('.modal').fadeOut();
    $('img').removeClass('disabled');
  });
  
  const h3 = $('.slide:not(.hide) h3').map(function() {
    return $(this).text();
  }).get();

  $('.slide-menu-item-vertical').filter(function() {
    if (h3.includes($(this).text())) {
      $(this).css('padding-left', '42px');
    }
  });

});
