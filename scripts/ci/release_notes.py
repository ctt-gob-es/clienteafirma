#!/usr/bin/env python3
"""Genera y valida notas de release desde un inventario real de assets."""
from __future__ import annotations

import argparse
import sys
from pathlib import Path


def _description(name: str) -> str:
    lowered = name.lower()
    if lowered.endswith(".jar"):
        return "JAR autoejecutable"
    if lowered.endswith(".deb"):
        return "Paquete DEB (Debian/Ubuntu)"
    if lowered.endswith(".rpm"):
        return "Paquete RPM"
    if lowered.endswith(".appimage"):
        return "AppImage"
    if lowered.endswith(".flatpak"):
        return "Flatpak"
    if name == "SHA256SUMS":
        return "Checksums SHA-256"
    if lowered.endswith(".asc"):
        return "Firma OpenPGP"
    if name == "bom.json":
        return "SBOM (CycloneDX JSON)"
    return "Artefacto"


def generate_notes(release_name: str, assets: list[str]) -> str:
    """Return notes in which every line comes from the supplied file names."""
    if not assets:
        raise ValueError("el inventario de assets está vacío")
    lines = [f"## Autofirma {release_name}", "", "### Qué incluye"]
    for name in sorted(assets):
        lines.append(f"- {_description(name)}: `{name}`")

    verification = []
    if "SHA256SUMS" in assets:
        verification.append("sha256sum -c SHA256SUMS")
    for name in sorted(assets):
        if name.endswith(".asc") and name[:-4] in assets:
            verification.append(f"gpg --verify {name} {name[:-4]}")
    if verification:
        lines.extend(["", "### Verificación", "", "```bash", *verification, "```"])
    lines.extend([
        "",
        "### Cambios",
        "Ver `CHANGELOG.md` en el contenido del tag.",
        "",
        "### Licencia",
        "GPL-2.0+ / EUPL-1.1",
        "",
    ])
    return "\n".join(lines)


def validate_notes(notes: str, assets: list[str]) -> list[str]:
    """Return factual errors: omitted assets or announced absent formats."""
    errors = []
    for name in assets:
        if f"`{name}`" not in notes:
            errors.append(f"el asset {name} no figura en las notas")
        if name.endswith(".asc") and name[:-4] not in assets:
            errors.append(f"la firma {name} no tiene un artefacto firmado asociado")

    announcements = {
        "RPM": lambda name: name.lower().endswith(".rpm"),
        "AppImage": lambda name: name.lower().endswith(".appimage"),
        "Flatpak": lambda name: name.lower().endswith(".flatpak"),
    }
    for line in notes.splitlines():
        if not line.lstrip().startswith("-"):
            continue
        for label, exists in announcements.items():
            if label.lower() in line.lower() and not any(exists(name) for name in assets):
                errors.append(f"las notas anuncian {label} sin un asset correspondiente")
    return errors


def _assets_from_directory(directory: Path) -> list[str]:
    if not directory.is_dir():
        raise ValueError(f"no existe el directorio de assets: {directory}")
    return sorted(path.name for path in directory.iterdir() if path.is_file())


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--assets-dir", required=True, type=Path)
    parser.add_argument("--release-name")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--check", type=Path, help="Validar unas notas existentes")
    args = parser.parse_args()

    try:
        assets = _assets_from_directory(args.assets_dir)
        if not assets:
            raise ValueError("el inventario de assets está vacío")
        if args.check:
            errors = validate_notes(args.check.read_text(encoding="utf-8"), assets)
            if errors:
                for error in errors:
                    print(f"ERROR: {error}", file=sys.stderr)
                return 1
            print(f"release-notes: OK ({len(assets)} assets)")
            return 0
        if not args.release_name or not args.output:
            parser.error("--release-name y --output son obligatorios sin --check")
        args.output.write_text(generate_notes(args.release_name, assets), encoding="utf-8")
        print(f"release-notes: generadas desde {len(assets)} assets en {args.output}")
        return 0
    except (OSError, ValueError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
