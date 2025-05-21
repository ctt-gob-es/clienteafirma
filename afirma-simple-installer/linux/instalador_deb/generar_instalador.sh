#!/bin/bash
# Genera el instalador .deb de AutoFirma. usamos la compresion Gzip con versiones antiguas de Dpkg.para evitar problemas en Debian
fakeroot dpkg-deb -Zgzip --build src AutoFirma_1_9_0.deb
