# requirements/ — dependencias Python de CI con hashes

Ficheros `*.in`: dependencia directa y versión.
Ficheros `*.txt`: árbol completo (transitivas incluidas) con `--hash=sha256:` de cada
artefacto, generados con `pip-compile --generate-hashes`.

Los workflows instalan con `pip3 install --require-hashes -r requirements/ci-*.txt`,
lo que exige que **todas** las dependencias estén fijadas por hash. La versión de
Python de referencia es la declarada en los workflows (`python-version: "3.14"`).

## Regenerar

```bash
pip install pip-tools
pip-compile --generate-hashes --output-file requirements/ci-semgrep.txt requirements/ci-semgrep.in
pip-compile --generate-hashes --output-file requirements/ci-pytest.txt requirements/ci-pytest.in
pip install --require-hashes -r requirements/ci-semgrep.txt   # verificación
```

Renovate (gestor `pip-compile`) propone actualizaciones sin automerge.

Motivo y contexto: `docs/scorecard.md`.
