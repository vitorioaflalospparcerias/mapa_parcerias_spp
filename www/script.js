// Ping para manter o app ativo
var keepAlive = setInterval(function() {
  if (typeof Shiny !== 'undefined') {
    Shiny.onInputChange('keep_alive_ping', Math.random());
  }
}, 10000);

// SCRIPT DE POSICIONAMENTO DINÂMICO
$(document).ready(function() {
  const panelRight = document.getElementById('painel_wrapper_right');
  const wsBtn = document.getElementById('btn_fullscreen');
  const expBtn = document.getElementById('btn_expand');
  
  function updateWsBtn() {
    let rightPos = '65px';
    if ($(panelRight).is(':visible')) {
      rightPos = ($(panelRight).outerWidth() + 25) + 'px';
    }
    if (wsBtn) wsBtn.style.right = rightPos;
    if (expBtn) expBtn.style.right = rightPos;
  }
  
  const resizeObserver = new ResizeObserver(updateWsBtn);
  resizeObserver.observe(panelRight);
  
  const mutObserver = new MutationObserver(updateWsBtn);
  mutObserver.observe(panelRight, { attributes: true, attributeFilter: ['style'] });
  
  // REPOSICIONAMENTO DA LEGENDA DO MAPA (SEM LOOP)
  const panelLeft = document.getElementById('painel_wrapper');

  function updateLegendPos() {
    const legend = document.querySelector('.info.legend.leaflet-control');
    if (!legend) return;

    const isPanelVisible = $(panelLeft).is(':visible');
    const parentRight = document.querySelector('.leaflet-bottom.leaflet-right');
    const parentLeft = document.querySelector('.leaflet-bottom.leaflet-left');

    if (isPanelVisible && legend.parentElement !== parentRight) {
      $(parentRight).prepend(legend);
    } else if (!isPanelVisible && legend.parentElement !== parentLeft) {
      $(parentLeft).append(legend);
    }
  }

  if (panelLeft) {
    const leftObserver = new MutationObserver(updateLegendPos);
    leftObserver.observe(panelLeft, { attributes: true, attributeFilter: ['style'] });
  }

  const mapContainer = document.querySelector('.main-content-wrapper');
  if (mapContainer) {
    const domObserver = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        if (mutation.addedNodes.length > 0 && document.querySelector('.info.legend.leaflet-control')) {
          updateLegendPos();
        }
      });
    });
    domObserver.observe(mapContainer, { childList: true, subtree: true });
  }
});