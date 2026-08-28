### Minor update: `merge_webcam_files()` shows a progress bar while reading files

- Reading files (or, for `kind = "labvanced"`, processing each participant folder) now shows a `purrr`-based progress bar, so it's clear the function is still working rather than hung on a large merge.

### Bug fix: `merge_webcam_files()` converts PsychoPy's time to milliseconds

- `kind = "psychopy"` now always converts `time` from seconds to milliseconds, since PsychoPy's clocks report elapsed time in seconds while every other `kind` (and the downstream sampling-rate/binning functions) assume milliseconds. Previously no conversion happened, so e.g. `analyze_sampling_rate()` returned values ~1000x too high (confirmed on real data: 34,906 Hz before the fix, 34.9 Hz after -- a physically plausible webcam eye-tracking rate).

### Bug fixes: `merge_webcam_files()` handles jsPsych's nested trial fields correctly

- `webgazer_targets` is now always flattened into flat `<selector>_<field>` columns (per `webgazer_target_names`), regardless of which of two shapes jsonlite parses it into: a uniform, packed data frame when every trial has the same selector(s)/fields (the webgazer extension's own `targets` parameter), or a per-row list when it varies from trial to trial (e.g. the `jspsych-free-sort` plugin's per-target rows, keyed by a `selector` column instead of by name). Previously only the first shape actually flattened -- the second silently fell through unflattened.
- Any *other* remaining nested column after merging (e.g. jsPsych's `mouse_events`, `final_locations`) used to be flattened with a plain `unlist()` + pipe-join, which interleaved every record's fields with no way to tell which value belonged to which field or record -- effectively unusable. Structured data (a data frame or named/nested list) is now serialized as JSON instead, preserving field names and row grouping; a flat, unnamed vector still collapses to the simpler pipe-delimited text as before.

### Minor update: `merge_webcam_files()` gains Labvanced support

- Added `kind = "labvanced"`, which combines a Labvanced **timeseries export** with its **trials export** into the same standardized `subject`/`trial`/`time`/`x`/`y` long format, auto-detecting which input file is which regardless of order.
- Labvanced's timeseries CSV only names columns through `value`, but `gaze_data` rows pack three further unnamed values onto the end of the line; these are now always parsed out as `x`, `y`, `t` (per-sample time), and `c` (confidence). Rows where a `gaze_data` event was logged with no sample attached (e.g. right as tracking starts up) are dropped rather than kept as `NA` rows.
- `time` is reported as milliseconds elapsed from 0 at each trial's first *actual* gaze sample (matching Gorilla/jsPsych), not the raw absolute epoch timestamp. This deliberately doesn't anchor to the trials export's `trial_start`: `Trial_Start` isn't an official Labvanced system variable, so it's whatever a given study's own condition table happened to define it as, not something guaranteed to exist or mean the same thing across studies -- anchoring to the first gaze sample instead only ever needs the timeseries export itself, so it works the same way for every Labvanced study.
- Each participant's timeseries and trials files are combined together first (grouped by the folder they live in, e.g. `sub1/{timeseries.csv,trials.csv}`), and only then are the participants' results stacked, so trial metadata can never leak across participants.
- `trial` defaults to `"<Block_Name>_<Task_Name>_<Trial_Nr>"`, not `Trial_Id`: Labvanced's `Trial_Nr` is at least a *per-task* counter (a `sentence` task and a later `sentence_DC` task each restart their own `Trial_Nr` at 1), `Trial_Id` turns out to be a per-task randomized item-order id rather than a stable trial identity, and a task can be reused across multiple blocks -- so the default folds in every level of Labvanced's own session > block > task > trial hierarchy rather than betting on exactly which level resets the counter. Trial metadata is joined on `Rec_Session_Id` + `Block_Name` + `Task_Name` + `Trial_Nr` to match.
- File-type detection is stricter: a file is only treated as trial metadata if it actually has a `Trial_Id` column. Anything that's none of a timeseries, trials, or sessions export is ignored with a message instead of being merged in.
- Added a `task` argument (Labvanced only) to restrict to a single `Task_Name`. Within one task, `Trial_Nr` alone is stable, so `trial` becomes a plain numeral (e.g. `"1"`, `"2"`, ...) instead of the composite id -- errors clearly if that task turns out to span more than one block, or if `task` doesn't match any `Task_Name` in the data.
- A Labvanced **sessions export**, if supplied, now has its screen/window size columns (`Screen_Width_In_Pixels`, `Screen_Height_In_Pixels`, `Window_Width_In_Pixels`, `Window_Height_In_Pixels`) -- and only those, not the rest of the file's browser/timing metadata -- joined onto matching rows by `Rec_Session_Id`. Useful since Labvanced's `x`/`y` are raw pixel coordinates, so screen size is what you'd need to normalize them or define AOIs consistently across participants with different screens.

### Minor update: `merge_webcam_files()`

- Refactored `merge_webcam_files()` to provide a unified interface for importing and merging webcam eye-tracking data from **Gorilla**, **jsPsych**, and **PsychoPy**.
- Added flexible `col_map` argument, allowing users to explicitly map subject IDs, trial indices, timestamps, and gaze coordinates across heterogeneous data sources.
- Improved handling of nested gaze arrays via `array_col`, with support for both **key–value** (e.g., `{t, x, y}`) and **positional** (e.g., `[t, x, y]`) array formats.
- Introduced `array_key` option to automatically detect or manually specify whether gaze arrays contain named keys.
- Enhanced robustness to missing or partially specified gaze variables, enabling successful imports when only a subset of expected columns is present.
- Added optional trial-level filtering and screen index selection for multi-monitor or multi-screen experiments.
- Improved error messages and input validation to make failures more informative and easier to debug.
- Optional saving of merged long-format eye-tracking data to disk via `out_dir`, with overwrite protection.

This update substantially improves cross-platform compatibility and reliability when working with webcam-based eye-tracking data collected across different experimental frameworks.
