DTXMapping "mstr.dtx"

  [ MatchNote Kick (Chip "01")
  , MatchNote Snare (Branch
    [ MatchType GemRim (Chip "0W")
    , MatchVelocity VelocityGhost (Chip "04")
    , Chip "02"
    ])
  , MatchNote Tom1 (Branch
    [ MatchVelocity VelocityGhost (Chip "0C")
    , Chip "0B"
    ])
  , MatchNote Tom2 (Branch
    [ MatchVelocity VelocityGhost (Chip "0E")
    , Chip "0D"
    ])
  , MatchNote Tom3 (Branch
    [ MatchVelocity VelocityGhost (Chip "0G")
    , Chip "0F"
    ])
  , MatchNote Hihat (Branch
    [ MatchType GemHihatOpen (Branch
      [ MatchVelocity VelocityGhost (Chip "0A")
      , Chip "08"
      ])
    , MatchVelocity VelocityGhost (Chip "0Z")
    , Chip "06"
    ])
  , MatchNote CrashL (Branch
    [ MatchType GemCymbalChoke (Chip "0Q")
    , MatchVelocity VelocityGhost (Chip "0O")
    , Chip "0M"
    ])
  , MatchNote CrashR (Branch
    [ MatchType GemCymbalChoke (Chip "0L")
    , MatchVelocity VelocityGhost (Chip "0J")
    , Chip "0H"
    ])
  , MatchNote Ride (Branch
    [ MatchVelocity VelocityGhost (Chip "0S")
    , Chip "0R"
    ])
  , MatchNote HihatFoot (Chip "0Y")
  ]

  [ DTXOverride "blue-splash"
    [ MatchNote CrashL (Chip "0V")
    ]
  , DTXOverride "green-china"
    [ MatchNote CrashR (Chip "0U")
    ]
  , DTXOverride "purple-china"
    [ MatchNote Ride (Chip "0U")
    ]
  , DTXOverride "purple-bell"
    [ MatchNote Ride (Chip "0T")
    ]
  ]
