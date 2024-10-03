# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.8.4] - 2024-10-03
### Fixed
- Handle data such as tuples.

## [0.8.3] - 2024-06-29
### Fixed
- Handle complex data in reports.

## [0.8.2] - 2024-04-07
### Fixed
- Correct logging of non-printable characters.
  Data for which io_lib:printable_unicode_list/1 returns true is logged as a string.
  Embedded newlines are escaped using JSON rules, i.e., encoded as "\n".
  Other data, e.g., binaries, is encoded using io_lib:format "~tp".

## [0.8.1] - 2024-01-04
### Added
- Upgrade thoas version
- Update CONTRIBUTING.md

## [0.8.0] - 2024-01-04
### Added
- Support structured logging
- Add map_msg config param

## [0.7.3] - 2023-10-30
### Added
- Updated docs for Elixir 1.15

## [0.7.2] - 2023-10-23
### Added
- Improved docs to cover latest Elixir
- Updated test matrix for latest Elixir and OTP

## [0.7.1] - 2023-10-15
### Fixed
- Handle log messages with Unicode

## [0.7.0] - 2023-03-20
### Added
- Initial version

