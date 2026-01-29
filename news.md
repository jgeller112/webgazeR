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
