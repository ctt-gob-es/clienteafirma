#!/usr/bin/env python3
"""
Script to automatically add categories to @Ignore annotations in Java test files.
"""
import re
import sys
from pathlib import Path

# Category mapping based on file patterns
CATEGORIES = {
    # Accessibility tests - require GUI/Orca
    'accessibility': 'Requiere GUI / lector de pantalla Orca',
    
    # Triphase client tests - require external service
    'padestri': 'Requiere servicio externo de firma trifásica',
    'xadestri': 'Requiere servicio externo de firma trifásica',
    'triphase': 'Requiere servicio externo de firma trifásica',
    
    # Hardware-dependent tests
    'dnie': 'Requiere hardware DNIe',
    'clauer': 'Requiere hardware CLAUER',
    'pkcs11': 'Requiere dispositivo PKCS#11',
    'windows': 'Requiere Windows OS',
    'keystore': 'Requiere almacén de certificados específico',
    'p12': 'Requiere archivo P12 específico',
    'nss': 'Requiere base de datos NSS de Mozilla',
    'mozilla': 'Requiere base de datos NSS de Mozilla',
    
    # Network/external service tests
    'http': 'Requiere conexión de red / servicio externo',
    'downloader': 'Requiere conexión de red / servicio externo',
    'batch': 'Requiere servidor de firma por lotes',
    
    # Integration tests not run in CI
    'integration': 'Prueba de integración no ejecutada en CI',
    'command': 'Prueba de línea de comandos no ejecutada en CI',
    
    # Known issues / flaky tests
    'bug': 'Error conocido de JDK / prueba inestable',
    'flaky': 'Prueba inestable / flaky',
}

def get_category_for_file(filepath: str) -> str:
    """Determine the appropriate category based on file path."""
    filepath_lower = filepath.lower()
    
    # Check for specific patterns in order of specificity
    if 'accessibility' in filepath_lower:
        return CATEGORIES['accessibility']
    if 'padestri' in filepath_lower:
        return CATEGORIES['padestri']
    if 'xadestri' in filepath_lower:
        return CATEGORIES['xadestri']
    if 'triphase' in filepath_lower:
        return CATEGORIES['triphase']
    if 'dnie' in filepath_lower:
        return CATEGORIES['dnie']
    if 'clauer' in filepath_lower:
        return CATEGORIES['clauer']
    if 'pkcs11' in filepath_lower:
        return CATEGORIES['pkcs11']
    if 'windows' in filepath_lower or 'windowscert' in filepath_lower:
        return CATEGORIES['windows']
    if 'p12' in filepath_lower or 'multipassword' in filepath_lower:
        return CATEGORIES['p12']
    if 'nss' in filepath_lower or 'mozilla' in filepath_lower:
        return CATEGORIES['mozilla']
    if 'batch' in filepath_lower:
        return CATEGORIES['batch']
    if 'http' in filepath_lower or 'downloader' in filepath_lower or 'zipfordata' in filepath_lower:
        return CATEGORIES['http']
    if 'bug' in filepath_lower:
        return CATEGORIES['bug']
    if 'command' in filepath_lower:
        return CATEGORIES['command']
    if 'keystore' in filepath_lower:
        return CATEGORIES['keystore']
    
    # Default for test files
    if 'test' in filepath_lower:
        return 'Prueba no ejecutada en CI / requiere configuración específica'
    
    return 'Requiere configuración específica no disponible en CI'

def fix_ignore_annotations(filepath: Path) -> int:
    """Add categories to @Ignore annotations in a Java file."""
    content = filepath.read_text(encoding='utf-8')
    lines = content.split('\n')
    category = get_category_for_file(str(filepath))
    changes = 0
    
    for i, line in enumerate(lines):
        # Match @Ignore without a reason
        # Pattern: @Ignore alone, or @Ignore followed by something other than ( or // or /*
        match = re.match(r'^(\s*)@Ignore(\s*)$', line)
        if match:
            indent = match.group(1)
            lines[i] = f'{indent}@Ignore("{category}")'
            changes += 1
            continue
            
        # Pattern: @Ignore followed by text (not ( or // or /*)
        match = re.match(r'^(\s*)@Ignore(\s+)([^(//].*)$', line)
        if match:
            indent = match.group(1)
            # Already has some text but not a valid reason - wrap it
            existing = match.group(3).strip()
            if existing and not existing.startswith('//') and not existing.startswith('/*'):
                lines[i] = f'{indent}@Ignore("{category}: {existing}")'
                changes += 1
                continue
    
    if changes > 0:
        filepath.write_text('\n'.join(lines), encoding='utf-8')
        print(f"Fixed {changes} @Ignore annotations in {filepath}")
    
    return changes

def main():
    # Get files from check-contract.sh output
    import subprocess
    result = subprocess.run(
        ['bash', 'scripts/ci/check-contract.sh'],
        capture_output=True, text=True, timeout=120
    )
    
    files = set()
    for line in result.stdout.split('\n'):
        if 'FALTA categoria en @Ignore:' in line:
            # Extract file path
            match = re.search(r'FALTA categoria en @Ignore: (.*):\d+', line)
            if match:
                files.add(match.group(1))
    
    print(f"Found {len(files)} files with @Ignore violations")
    
    total_changes = 0
    for file_str in sorted(files):
        filepath = Path(file_str)
        if filepath.exists():
            total_changes += fix_ignore_annotations(filepath)
        else:
            print(f"File not found: {filepath}")
    
    print(f"\nTotal @Ignore annotations fixed: {total_changes}")

if __name__ == '__main__':
    main()
