# What was removed for this public copy

**127 `uri` locators.** Every file in the export carried a locator pointing at the improve instance
it came from — an address on an internal host that nobody outside can reach. They were removed here.

**Why this costs no verification.** A locator is carried, never covered: it does not enter the
content address, so the `sha256` of every file is exactly what it was, and so is anything computed
from it. The identity is intact; only "where it also happens to live" is gone.

**What replaced them.** Commit-pinned locators into this repository, one per file, in
`lineage/lineage.json`. They point at `raw.githubusercontent.com/…/<commit>/…` rather than at a
branch, because a branch moves and the bytes under it do not have to — the same rule
`plankton author --located` enforces: *no commit, no locator*.

Nothing else was changed. The bytes in `lineage/steps/` are the bytes the computations read and
wrote, and `lineage/specs/` still names every one of them with its hash.
