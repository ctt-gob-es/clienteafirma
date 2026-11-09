# Handoff: CodeQL alert protection (#89 / #90)

El umbral de **code scanning protection** «high or higher» solo para alertas **nuevas** de CodeQL no se cambia desde el agente (prohibido tocar rulesets / ajustes del repo).

Cuando `ci-gate` esté en `master`, **Arquitecto** debe:

1. Settings → Code security → Code scanning → Protection rules (o equivalente).
2. Exigir que no haya alertas nuevas de gravedad high/critical en PRs a `master`.
3. Coordinar con el ruleset de #90 que exige el check `ci-gate`.

El gate de Semgrep para hallazgos nuevos graves ya corre en el job `semgrep-diff-gate` de `ci.yml` (sin segunda subida SARIF).
