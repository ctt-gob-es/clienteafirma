#!/usr/bin/env bash
# quality: meta-seccion Proposito y POM bien formado. No toca el arbol de producto.
set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$root"

fail=0

require_purpose() {
  local f="$1"
  if [[ ! -f "$f" ]]; then
    echo "FALTA: $f"
    fail=1
    return
  fi
  if ! grep -q "Propósito de este documento" "$f"; then
    echo "FALTA meta-seccion Proposito: $f"
    fail=1
  fi
}

require_purpose README.md
require_purpose SECURITY.md
require_purpose CONTRIBUTING.md
require_purpose CHANGELOG.md
require_purpose .github/PULL_REQUEST_TEMPLATE.md
require_purpose .github/ISSUE_TEMPLATE/bug.md
require_purpose .github/ISSUE_TEMPLATE/feature.md

if [[ ! -f LICENSE ]]; then
  echo "FALTA: LICENSE (puntero SPDX de raiz)"
  fail=1
fi

if ! grep -q 'GPL-2.0-or-later OR EUPL-1.1' LICENSE; then
  echo "LICENSE de raiz debe declarar el SPDX dual del producto"
  fail=1
fi

if [[ ! -f .github/CODEOWNERS ]]; then
  echo "FALTA: .github/CODEOWNERS"
  fail=1
elif ! grep -q '@Alexendros' .github/CODEOWNERS; then
  echo "CODEOWNERS debe incluir @Alexendros"
  fail=1
fi

if [[ -f .github/dependabot.yml ]]; then
  if grep -q 'package-ecosystem' .github/dependabot.yml && grep -q 'version-updates|interval' .github/dependabot.yml; then
    echo "Quita Dependabot version-updates; usa Renovate"
    fail=1
  fi
fi

if [[ ! -f .github/renovate.json ]]; then
  echo "FALTA: .github/renovate.json"
  fail=1
fi

if [[ ! -f pom.xml ]]; then
  echo "FALTA: pom.xml"
  fail=1
else
  python3 - "$root/pom.xml" <<'PY'
import sys
import xml.etree.ElementTree as ET

path = sys.argv[1]
try:
    ET.parse(path)
except ET.ParseError as exc:
    print(f"pom.xml mal formado: {exc}")
    sys.exit(1)
print("pom.xml: XML bien formado")
PY
fi

# Check @Ignore annotations have required category/reason
check_ignore_categories() {
  local root="$(cd "$(dirname "$0")/../.." && pwd)"
  local temp_file=$(mktemp)
  
  # Find all .java files with @Ignore
  while IFS= read -r -d '' file; do
    # Single pattern: @Ignore that doesn't have a reason
    # Valid: @Ignore("reason"), @Ignore // reason, @Ignore /* reason */
    # Invalid: @Ignore (alone), @Ignore something (no paren, no comment)
    grep -n '^[[:space:]]*@Ignore' "$file" 2>/dev/null | while IFS=: read -r line_num line; do
      # Check if it has a valid reason
      has_reason=0
      
      # Pattern 1: @Ignore("reason") or @Ignore ( "reason" )
      if echo "$line" | grep -q '^[[:space:]]*@Ignore[[:space:]]*('; then
        has_reason=1
      fi
      
      # Pattern 2: @Ignore // reason
      if echo "$line" | grep -q '^[[:space:]]*@Ignore[[:space:]]*//'; then
        has_reason=1
      fi
      
      # Pattern 3: @Ignore /* reason */
      if echo "$line" | grep -q '^[[:space:]]*@Ignore[[:space:]]*/\*'; then
        has_reason=1
      fi
      
      if [[ $has_reason -eq 0 ]]; then
        echo "FALTA categoria en @Ignore: $file:$line_num"
        echo "1" >> "$temp_file"
      fi
    done
  done < <(find "$root" -name "*.java" -not -path "*/target/*" -type f -print0)
  
  # Check if any failures were recorded
  local result=0
  if [[ -s "$temp_file" ]]; then
    result=1
  fi
  rm -f "$temp_file"
  return $result
}

check_ignore_categories || fail=1

if [[ "$fail" -ne 0 ]]; then
  echo "check-contract: FALLO"
  exit 1
fi

echo "check-contract: OK"
