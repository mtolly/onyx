-- modified from talkies.lua which was written by ChatGPT

-- Instantiate Reaper API
local reaper = reaper

function main()
    -- Get the active MIDI editor
    local editor = reaper.MIDIEditor_GetActive()
    if editor == nil then return end

    -- Get the current take being edited
    local take = reaper.MIDIEditor_GetTake(editor)
    if take == nil then return end

    -- Get the number of MIDI events in the take
    local _, count = reaper.MIDI_CountEvts(take)

    -- Iterate over all MIDI events
    for i = 0, count - 1 do
        local retval, selected, muted, startppqpos, chanmsg, text = reaper.MIDI_GetTextSysexEvt(take, i)

        -- Check if the event is a text event (0x01) and selected
        if selected and chanmsg == 0x01 then
            -- Set the modified event back
            reaper.MIDI_SetTextSysexEvt(take, i, selected, muted, startppqpos, 0x05, text)
        end
    end
end

main()
