take = RPR_MIDIEditor_GetTake(RPR_MIDIEditor_GetActive())
noteCount = RPR_MIDI_CountEvts(take, 0, 0, 0)[2]
rightFoot = True
for i in range(noteCount):
  note = RPR_MIDI_GetNote(take, i, 0, 0, 0, 0, 0, 0, 0)
  selected = note[3]
  pitch = note[8]
  if selected:
    if rightFoot:
      rightFoot = False
    else:
      RPR_MIDI_SetNote(take, i, -1, 0, -1, -1, -1, pitch - 1, -1, -1)
      rightFoot = True
RPR_MIDI_Sort(take)
