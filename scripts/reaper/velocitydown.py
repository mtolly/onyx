take = RPR_MIDIEditor_GetTake(RPR_MIDIEditor_GetActive())
noteCount = RPR_MIDI_CountEvts(take, 0, 0, 0)[2]
for i in range(noteCount):
  note = RPR_MIDI_GetNote(take, i, 0, 0, 0, 0, 0, 0, 0)
  selected = note[3]
  velocity = note[9]
  if selected:
    if velocity == 127:
      newVelocity = 96
    else:
      newVelocity = 1
    RPR_MIDI_SetNote(take, i, -1, 0, -1, -1, -1, -1, newVelocity, -1)
