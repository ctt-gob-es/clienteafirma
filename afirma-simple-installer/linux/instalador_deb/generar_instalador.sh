#!/bin/bash
# Genera el instalador .deb de AutoFirma
fakeroot dpkg-deb --build src AutoFirma_1_7_1.deb
