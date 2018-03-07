window.customizeOnyx = {

  background: 'rgb(54,59,123)',
  timestampFont: '17px monospace',
  timestampColor: 'white',

  loadingBackground: '#4B1C4E',
  loadingBigSquare: 'rgb(37,14,39)',
  loadingSmallSquares: '#CC8ED1',
  loadingSmallSquareSize: 50,
  loadingLoopTime_ms: 2000,

  progressBorder: 'black',
  progressEmpty: 'white',
  progressFilled: 'rgb(100,130,255)',

  highway: 'rgb(126,126,150)',
  highwayBlackKey: 'rgb(105,105,129)',
  highwayRailing: 'rgb(184,185,204)',
  highwayDivider: 'black',
  highwaySolo: 'rgb(91,137,185)',
  highwaySoloBlackKey: 'rgb(73,111,149)',

  proKeysRangeOverlay: 'rgba(0,0,0,0.3)', // darkens pro keys outside of the current range
  glissandoBorder: 'white',

  freeformLane: 'rgb(60,60,60)', // trill/tremolo/BRE

  harm1Pitch: 'rgb(46,229,223)',
  harm2Pitch: 'rgb(189,67,0)',
  harm3Pitch: 'rgb(225,148,22)',
  harm1Talky: 'rgba(46,229,223,0.6)',
  harm2Talky: 'rgba(189,67,0,0.6)',
  harm3Talky: 'rgba(225,148,22,0.6)',
  percussionOuter: '#d9d9d9',
  percussionInner: '#00b9c9',
  percussionHit: function(o){return 'rgba(255,255,255,'+o+')';},
  vocalPhraseEnd: '#bbb',
  vocalTargetLine: '#ddd',
  lyricColor: 'white',
  lyricColorEnergy: 'yellow',
  lyricFont: 'bold 17px sans-serif',
  lyricFontTalky: 'bold italic 17px sans-serif',
  lyricLaneTop: 'rgba(87,55,0,0.85)',
  vocalNoteArea: 'rgba(0,0,0,0.6)',
  lyricLaneBottom: 'rgba(0,27,89,0.85)',

  sustainBorder: 'black',
  sustainGreen: {
    light: 'rgb(135,247,126)',
    normal: 'rgb(21,218,2)',
    dark: 'rgb(13,140,2)',
    hit: function(o){return 'rgba(190,255,192,'+o+')';},
  },
  sustainRed: {
    light: 'rgb(247,127,158)',
    normal: 'rgb(218,2,62)',
    dark: 'rgb(140,2,40)',
    hit: function(o){return 'rgba(255,188,188,'+o+')';},
  },
  sustainYellow: {
    light: 'rgb(247,228,127)',
    normal: 'rgb(218,180,2)',
    dark: 'rgb(140,115,3)',
    hit: function(o){return 'rgba(255,244,151,'+o+')';},
  },
  sustainBlue: {
    light: 'rgb(119,189,255)',
    normal: 'rgb(2,117,218)',
    dark: 'rgb(3,76,140)',
    hit: function(o){return 'rgba(190,198,255,'+o+')';},
  },
  sustainOrange: {
    light: 'rgb(255,183,119)',
    normal: 'rgb(218,97,4)',
    dark: 'rgb(140,63,3)',
    hit: function(o){return 'rgba(231,196,112,'+o+')';},
  },
  sustainPurple: {
    light: 'rgb(214,154,242)',
    normal: 'rgb(167,25,241)',
    dark: 'rgb(128,12,188)',
    hit: function(o){return 'rgba(210,162,255,'+o+')';},
  },
  sustainEnergy: {
    light: 'rgb(137,235,204)',
    normal: 'rgb(138,192,175)',
    dark: 'rgb(124,158,149)',
    hit: function(o){return 'magenta';}, // not used
  },
  sustainWhiteKey: {
    light: 'rgb(199,134,218)',
    normal: 'rgb(184,102,208)',
    dark: 'rgb(178, 86,204)',
    hit: function(o){return 'rgba(227,193,238,'+o+')';},
  },
  sustainBlackKey: {
    light: 'rgb(175,83,201)',
    normal: 'rgb(147,49,175)',
    dark: 'rgb(123,42,150)',
    hit: function(o){return 'rgba(227,193,238,'+o+')';},
  },
  sustainBlackKeyEnergy: {
    light: 'rgb(52,148,117)',
    normal: 'rgb(71,107,95)',
    dark: 'rgb(69,83,79)',
    hit: function(o){return 'magenta';}, // not used
  },
  // note: GHL white/both/open currently the same
  sustainWhiteGHL: {
    light: 'rgb(220,220,220)',
    normal: 'rgb(181,181,181)',
    dark: 'rgb(162,162,162)',
    hit: function(o){return 'rgba(200,200,200,'+o+')';},
  },
  sustainBothGHL: {
    light: 'rgb(220,220,220)',
    normal: 'rgb(181,181,181)',
    dark: 'rgb(162,162,162)',
    hit: function(o){return 'rgba(200,200,200,'+o+')';},
  },
  sustainBlackGHL: {
    light: 'rgb(121,121,121)',
    normal: 'rgb(70,70,70)',
    dark: 'rgb(45,45,45)',
    hit: function(o){return 'rgba(200,200,200,'+o+')';},
  },
  sustainOpenGHL: {
    light: 'rgb(220,220,220)',
    normal: 'rgb(181,181,181)',
    dark: 'rgb(162,162,162)',
    hit: function(o){return 'rgba(200,200,200,'+o+')';},
  },

  widthStandardFret: 36,
  widthProtarFret: 30,

  autoplay: true,

};
