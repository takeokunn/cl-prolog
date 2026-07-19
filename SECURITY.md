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

- the repository's [private vulnerability report form](https://github.com/takeokunn/cl-prolog/security/advisories/new),
  when GitHub makes that form available
- if the form is unavailable, a private contact method explicitly published on
  the [maintainer's GitHub profile](https://github.com/takeokunn), initially
  sharing only enough information to request a secure reporting channel

Private vulnerability reporting is not guaranteed to be enabled at all times.
If neither private route is available, do not disclose vulnerability details in
a public issue. You may open a public issue containing no sensitive details to
ask the maintainer to enable a private reporting channel.

Include as much concrete detail as possible:

- affected API or file paths
- proof-of-concept input or reproduction steps
- impact assessment
- any proposed mitigation or patch direction

## Response Expectations

This is a best-effort, volunteer-maintained project. Maintainers aim to:

- acknowledge receipt within 7 days
- provide an initial evidence-based assessment within 14 days
- provide a progress update at least every 30 days while a confirmed report
  remains unresolved
- prepare a fix or mitigation when the report is valid
- publish the fix through the normal repository workflow

These targets are service goals, not guarantees. Complex reports, incomplete
reproductions, or maintainer availability may extend them; the maintainer will
communicate revised expectations through the private reporting channel.

Because `cl-prolog` is a library, coordinated disclosure may require checking
downstream compatibility before a fix is released. Reports without a reliable
reproduction may take longer to evaluate.
