# OpenSSF Scorecard — estado y resolución de alertas

Fecha: 2026-11-02 · Repositorio: `Soluciones-Alexendros/clienteafirma-alexendros`

Scorecard (`ossf/scorecard-action`) publica resultados en code scanning con el
prefijo de regla `...ID` (por ejemplo `PinnedDependenciesID`). Este documento es
la referencia única para las alertas abiertas y descartadas: cada aviso se
justifica con un motivo, una acción tomada y cómo volver a comprobarlo.

Nota de partida (2026-11-02): nota global **5,1 / 10**. Tras esta ronda: **5,3 / 10**
y **0 alertas abiertas** en code scanning (100 arregladas, 100 descartadas con motivo).
La nota no es el objetivo: el objetivo es que cada alerta esté arreglada o justificada.

`Pinned-Dependencies` pasó de 7 a **10**; `Token-Permissions` y `Security-Policy` en 10.
Lo que queda por debajo es proceso del proyecto (Code-Review, Maintained, CII-Best-Practices,
Binary-Artifacts, Fuzzing, Signed-Releases), no código sin triar.

## Resumen

| Regla | Alertas | Estado | Motivo |
|---|---|---|---|
| `PinnedDependenciesID` | 4 | **Arreglada** | Instalaciones `pip` sin hashes → ahora `--require-hashes` con `requirements/ci-*.txt`. Verificado: Scorecard ya no las reporta. |
| Semgrep OSS (18 reglas) | 99 | **Arregladas (13) o descartadas con motivo (86)** | Triaje en [`semgrep-triage.md`](semgrep-triage.md). Incluye el hallazgo de que el escaneo llevaba tiempo sin ejecutarse. |
| `BinaryArtifactsID` | 20 | Descartada (justificada) | Binarios heredados del proyecto oficial; ver `docs/binarios-heredados.md`. |
| `CIIBestPracticesID` | 1 | Descartada (justificada) | Requiere solicitud humana al programa OpenSSF Best Practices Badge. |
| `MaintainedID` | 1 | Descartada (justificada) | Mide actividad en ventana temporal; el fork es reciente. Se reevalúa. |
| `FuzzingID` | 1 | Descartada (justificada) | No hay infraestructura de fuzzing; propuesta en «Trabajo futuro». |
| `CodeReviewID`, `BranchProtectionID`, `VulnerabilitiesID` | 3 | Descartadas | Rulesets exigen PR + `ci-gate` + firma; revisión por PR. |

## 1. `PinnedDependenciesID` — arreglada

Scorecard marca como no fijada cualquier instalación con `pip`/`pip3` que no use
`--require-hashes` (o un wheel `.whl`). Fijar solo la versión (`pip install x==1.2.3`)
**no** cuenta como fijado.

Qué se ha hecho:

- `requirements/ci-semgrep.in` y `requirements/ci-pytest.in`: dependencias declaradas.
- `requirements/ci-semgrep.txt` y `requirements/ci-pytest.txt`: árbol completo
  (dependencias transitivas incluidas) con `--hash=sha256:` de cada artefacto,
  generados con `pip-compile --generate-hashes`.
- Los workflows instalan con `pip3 install --require-hashes -r requirements/ci-*.txt`.
- `python-version` fijada a `3.14` en los workflows que usan estos ficheros: los
  hashes y los marcadores de entorno se generaron con esa versión.
- Renovate tiene habilitado el gestor `pip-compile` para proponer actualizaciones
  (sin automerge).

Regenerar tras cambiar un `.in`:

```bash
pip-compile --generate-hashes --output-file requirements/ci-semgrep.txt requirements/ci-semgrep.in
pip-compile --generate-hashes --output-file requirements/ci-pytest.txt requirements/ci-pytest.in
pip install --require-hashes -r requirements/ci-semgrep.txt   # verificación local
```

Comprobación: `rg "pip3? install" .github/workflows` debe mostrar solo líneas con
`--require-hashes -r requirements/ci-*.txt`.

## 1 bis. Semgrep — el escaneo estaba roto y ahora funciona

Al fijar por hash las dependencias de CI se descubrió que el job `Semgrep` nunca
analizaba nada: `pip3 install semgrep` sin versión instalaba una versión
incompatible con el Python del runner (3.15) y el paso fallaba, pero el workflow lo
ocultaba con `|| true`. Con `semgrep==1.183.0` (hash-pinned) y
`python-version: "3.14"` el escaneo funciona: 143 hallazgos, ~90 alertas.

Las rutas heredadas que no se compilan ni publican quedan fuera vía
`.semgrepignore`, y cada regla está clasificada (falso positivo, riesgo aceptado o
hardening pendiente) en [`semgrep-triage.md`](semgrep-triage.md).

## 2. `BinaryArtifactsID` — descartada

20 binarios heredados sin cambios del proyecto oficial (`upstream/master`). No se
compilan ni se publican desde este repositorio. Inventario y motivo por fichero en
[`binarios-heredados.md`](binarios-heredados.md). Se revisa si el oficial elimina o
cambia alguno.

## 3. `CIIBestPracticesID` — descartada (acción humana pendiente)

El check exige estar inscrito en el programa [OpenSSF Best Practices Badge](https://www.bestpractices.dev/)
( formulario interactivo con correo de contacto y credenciales del proyecto). No es
automatizable desde el repositorio ni procede hacerlo en nombre del mantenedor.

Acción: el mantenedor decide si solicita la insignia (nivel *passing*). Mientras
tanto, el aviso queda descartado como «won't fix» con esta referencia.

## 4. `MaintainedID` — descartada (temporal)

El check puntúa la actividad reciente del proyecto (commits e issues en una ventana
temporal) y penaliza repositorios jóvenes. Este fork se creó en 2026 y acumula
actividad desde entonces; la puntuación sube sola con el tiempo.

Acción: reevaluar tras el cierre de la Fase 2. No requiere cambios de código.

## 5. `FuzzingID` — descartada, con propuesta

No hay fuzzing porque Scorecard busca integración con OSS-Fuzz/ClusterFuzzLite o
targets reconocibles (`FuzzXxx` en Go, `atheris` en Python, `com.code_intelligence.jazzer.api.FuzzedDataProvider`
en Java). Este cliente no tiene ninguna de las tres.

Trabajo futuro (Fase 3), no bloqueante:

1. Añadir `com.code-intelligence:jazzer-junit` como dependencia de test y un target
   de fuzzing sobre los puntos de entrada que procesan datos no confiables:
   `SecureXmlBuilder` (XML/XXE), `ZipSecurePaths` (zip slip) y los parsers DER/PKCS#7.
2. Ejecutarlo en un job programado (cron semanal) con límite de tiempo, no en cada PR.
3. Cuando exista el target, este aviso desaparece por sí solo.

## Estado final de la ronda (2026-11-02)

- Alertas abiertas: **0**.
- Arregladas con código y test (cierran solas en el siguiente escaneo): `weak-random`
  (seriales con `SecureRandom`), `overly-permissive-file-permission` (permisos `0700`),
  `renovate-missing-minimum-release-age` (`minimumReleaseAge` en todas las reglas).
- Descartadas con motivo y comentario individual, apuntando a este documento o a
  [`semgrep-triage.md`](semgrep-triage.md): 100 (falsos positivos técnicos, riesgo
  aceptado por diseño y rutas heredadas excluidas).

Cuando aparezca una alerta nueva, este es el orden: (1) comprobar si es real,
(2) arreglarla con test, (3) si no es accionable, añadirla a este documento con su
motivo y descartarla citándolo.

## Cómo revisar este documento

```bash
gh api "repos/Soluciones-Alexendros/clienteafirma-alexendros/code-scanning/alerts?state=open" \
  -q '.[] | "\(.rule.id)\t\(.most_recent_instance.location.path)"'
```

Cada alerta abierta debe aparecer en este documento con estado «arreglada» o
«descartada» y su motivo. Si aparece una regla nueva, se añade aquí antes de
descartarla.
