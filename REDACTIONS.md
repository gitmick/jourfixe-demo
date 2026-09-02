# What was removed for this public copy

**127 `uri` locators.** Every file in the export carried a locator pointing at the improve instance
it came from — an address on an internal host that nobody outside can reach. They were removed here
and replaced by nothing.

**Why this costs no verification.** A locator is carried, never covered: it does not enter the
content address, so the `sha256` of every file is exactly what it was, and so is anything computed
from it. The identity is intact; only the "where it also happens to live" is gone.

Nothing else was changed. The bytes in `lineage/steps/` are the bytes the computations read and
wrote, and `lineage/specs/` still names every one of them with its hash.
