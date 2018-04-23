candle AutoFirma_64_installer.wxs
light -ext WixUtilExtension AutoFirma_64_installer.wixobj
move AutoFirma_64_installer.msi AutoFirma64
cd AutoFirma64
rename AutoFirma_64_installer.msi AutoFirma_64_v1_6_3_installer.msi
@pause