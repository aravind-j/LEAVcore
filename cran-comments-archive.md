# Version 0.1.0 - First submission

- First release.

### Test environments

- local Windows 10 Pro 25H2, R-release (R 4.6.0) & R-devel (R 4.7.0
  Pre-release).
- local Ubuntu 20.04, R-release (R 4.6.0) & R-devel (R 4.7.0
  Pre-release).
- win-builder, R-release (R 4.6.0) & R-devel (R 4.7.0 Pre-release).
- github macOS Sequoia 15.7.4, R-release (R 4.6.0).
- github Ubuntu 24.04.4, R-release (R 4.6.0), R-oldrel-1 (R 4.5.3) &
  R-devel (R 4.7.0 Pre-release).

### R CMD check results

- There was a NOTE `Timeout was reached [nbpgr.org.in]` in win-builder,
  R-release (R 4.6.0) & R-devel (R 4.7.0 Pre-release) as well as the
  github environments which is a false positive. This is because of
  temporary geo-blocking by our IT admin due to some cyber-security
  concerns.
- There were no other NOTES, ERRORs or WARNINGs.
