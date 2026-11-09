#!/usr/bin/env python3
"""Tests del inventario y la veracidad de notas de release."""
from __future__ import annotations

import unittest

import release_notes


class ReleaseNotesTest(unittest.TestCase):
    def test_genera_solo_assets_reales(self) -> None:
        assets = ["SHA256SUMS", "autofirma-test.jar", "autofirma-test.deb"]
        notes = release_notes.generate_notes("test", assets)
        self.assertEqual([], release_notes.validate_notes(notes, assets))
        self.assertIn("autofirma-test.jar", notes)
        self.assertIn("autofirma-test.deb", notes)
        self.assertNotIn("RPM", notes)
        self.assertNotIn("AppImage", notes)
        self.assertNotIn("Flatpak", notes)
        self.assertNotIn(".asc", notes)

    def test_rechaza_rpm_y_appimage_ausentes(self) -> None:
        notes = "## Release\n\n- Paquete RPM\n- AppImage universal\n"
        errors = release_notes.validate_notes(notes, ["autofirma-test.jar"])
        self.assertTrue(any("RPM" in error for error in errors))
        self.assertTrue(any("AppImage" in error for error in errors))

    def test_rechaza_asset_omitido(self) -> None:
        errors = release_notes.validate_notes("## Release\n", ["autofirma-test.jar"])
        self.assertTrue(any("autofirma-test.jar" in error for error in errors))

    def test_incluye_paquetes_y_firma_cuando_existen(self) -> None:
        assets = [
            "SHA256SUMS",
            "SHA256SUMS.asc",
            "autofirma-test.jar",
            "autofirma-test.rpm",
            "Autofirma-x86_64.AppImage",
        ]
        notes = release_notes.generate_notes("test", assets)
        self.assertEqual([], release_notes.validate_notes(notes, assets))
        self.assertIn("gpg --verify SHA256SUMS.asc SHA256SUMS", notes)
        self.assertIn("sha256sum -c SHA256SUMS", notes)


if __name__ == "__main__":
    unittest.main()
