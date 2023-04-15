DTXMapping "mstr.dtx"

  [ MatchNote Kick (Branch
    [ MatchVelocity VelocityGhost (Chip "1V")
    , Chip "0V"
    ])
  , MatchNote Snare (Branch
    [ MatchType GemRim (Chip "0J")
    , MatchVelocity VelocityGhost (Chip "0H")
    , Chip "0Y"
    ])
  , MatchNote Tom1 (Branch
    [ MatchVelocity VelocityGhost (Chip "1O")
    , Chip "0O"
    ])
  , MatchNote Tom2 (Branch
    [ MatchVelocity VelocityGhost (Chip "1P")
    , Chip "0P"
    ])
  , MatchNote Tom3 (Branch
    [ MatchVelocity VelocityGhost (Chip "1Q")
    , Chip "0Q"
    ])
  , MatchNote Hihat (Branch
    [ MatchType GemHihatOpen (Branch
      [ MatchVelocity VelocityGhost (Chip "1B")
      , Chip "0B"
      ])
    , MatchVelocity VelocityGhost (Chip "19")
    , Chip "09"
    ])
  , MatchNote CrashL (Branch
    [ MatchVelocity VelocityGhost (Chip "14")
    , Chip "04"
    ])
  , MatchNote CrashR (Branch
    [ MatchVelocity VelocityGhost (Chip "15")
    , Chip "05"
    ])
  , MatchNote Ride (Branch
    [ MatchVelocity VelocityAccent (Chip "0E")
    , MatchVelocity VelocityGhost (Chip "1F")
    , Chip "0F"
    ])
  , MatchNote HihatFoot (Branch
    [ MatchType GemHihatOpen (Chip "0C")
    , Chip "0A"
    ])
  ]

  [ DTXOverride "blue-china"
    [ MatchNote CrashL (Branch
      [ MatchVelocity VelocityGhost (Chip "11")
      , Chip "01"
      ])
    ]
  , DTXOverride "green-china"
    [ MatchNote CrashR (Branch
      [ MatchVelocity VelocityGhost (Chip "12")
      , Chip "02"
      ])
    ]
  , DTXOverride "purple-china"
    [ MatchNote Ride (Branch
      [ MatchVelocity VelocityGhost (Chip "13")
      , Chip "03"
      ])
    ]
  , DTXOverride "purple-crash"
    [ MatchNote Ride (Branch
      [ MatchVelocity VelocityGhost (Chip "16")
      , Chip "06"
      ])
    ]
  , DTXOverride "purple-hihat-open"
    [ MatchNote Ride (Branch
      [ MatchVelocity VelocityGhost (Chip "18")
      , Chip "08"
      ])
    ]
  , DTXOverride "purple-hihat-closed"
    [ MatchNote Ride (Branch
      [ MatchVelocity VelocityGhost (Chip "17")
      , Chip "07"
      ])
    ]
  , DTXOverride "green-ride"
    [ MatchNote CrashR (Branch
      [ MatchVelocity VelocityGhost (Chip "1D")
      , Chip "0D"
      ])
    ]
  , DTXOverride "yellow-ride"
    [ MatchNote Hihat (Branch
      [ MatchVelocity VelocityGhost (Chip "1G")
      , Chip "0G"
      ])
    ]
  , DTXOverride "yellow-splash"
    [ MatchNote Hihat (Branch
      [ MatchVelocity VelocityGhost (Chip "1N")
      , Chip "0N"
      ])
    ]
  , DTXOverride "blue-splash"
    [ MatchNote CrashL (Branch
      [ MatchVelocity VelocityGhost (Chip "1K")
      , Chip "0K"
      ])
    ]
  , DTXOverride "green-splash"
    [ MatchNote CrashR (Branch
      [ MatchVelocity VelocityGhost (Chip "1L")
      , Chip "0L"
      ])
    ]
  , DTXOverride "purple-splash"
    [ MatchNote Ride (Branch
      [ MatchVelocity VelocityGhost (Chip "1M")
      , Chip "0M"
      ])
    ]
  , DTXOverride "yellow-stack"
    [ MatchNote Hihat (Branch
      [ MatchVelocity VelocityGhost (Chip "1U")
      , Chip "0U"
      ])
    ]
  , DTXOverride "blue-stack"
    [ MatchNote CrashL (Branch
      [ MatchVelocity VelocityGhost (Chip "1R")
      , Chip "0R"
      ])
    ]
  , DTXOverride "green-stack"
    [ MatchNote CrashR (Branch
      [ MatchVelocity VelocityGhost (Chip "1S")
      , Chip "0S"
      ])
    ]
  , DTXOverride "purple-stack"
    [ MatchNote Ride (Branch
      [ MatchVelocity VelocityGhost (Chip "1T")
      , Chip "0T"
      ])
    ]
  , DTXOverride "tom3-cowbell"
    [ MatchNote Tom3 (Branch
      [ MatchVelocity VelocityGhost (Chip "1W")
      , Chip "0W"
      ])
    ]
  , DTXOverride "tom1-alt-snare"
    [ MatchNote Tom1 (Branch
      [ MatchVelocity VelocityGhost (Chip "1Z")
      , Chip "0Z"
      ])
    ]
  ]
