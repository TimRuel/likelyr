# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/).

---

## [v0.1.0] - 2026-01-21

### Added
- Core S3 class system and result state tracking
- Model specification DSL (`model_spec()`, component specs)
- Modular calibration pipeline
- Integrated likelihood Monte Carlo branch engine
- Profile likelihood sweep engine
- ψ evaluation and ω̂ nuisance machinery
- Public API:
  - `integrate()`
  - `profile()`
  - `diagnose()`
  - `infer()`
  - `compare()`
- Integrated likelihood diagnostics:
  - ESS
  - dispersion
  - outlier detection
  - warning system
- Inference framework:
  - point estimates
  - likelihood-based intervals
- Comparison tools for profile vs integrated likelihood
- Unified plotting framework
- Styled table rendering system
- YAML-driven style configuration:
  - `inst/styles/plots.yml`
  - `inst/styles/tables.yml`
- Full package documentation and pkgdown configuration

### Changed
- Project structure refactored into modular subsystems
- Public API stabilized for simulation workflows

### Removed
- Deprecated development test scripts

---

