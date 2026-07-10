# Security Policy

## Supported Versions

`cl-prolog` is currently maintained as a single active line.

| Version line | Status |
| --- | --- |
| `main` / latest release | supported |
| older releases | best effort only |

Security fixes are developed against the latest code. If you need a backport
for an older release line, open an issue first to confirm whether that line is
still practical to support.

## Reporting a Vulnerability

Do not open a public issue for a suspected vulnerability before maintainers
have confirmed the impact.

Instead, report the issue through one of these channels:

- GitHub Security Advisories for this repository
- a private maintainer contact method already established for this project

Include as much concrete detail as possible:

- affected API or file paths
- proof-of-concept input or reproduction steps
- impact assessment
- any proposed mitigation or patch direction

## Response Expectations

Maintainers aim to:

- acknowledge the report
- reproduce or reject the issue based on evidence
- prepare a fix or mitigation when the report is valid
- publish the fix through the normal repository workflow

Because `cl-prolog` is a library, coordinated disclosure may require checking
downstream compatibility before a fix is released. Reports without a reliable
reproduction may take longer to evaluate.
