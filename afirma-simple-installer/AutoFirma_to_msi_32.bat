candle AutoFirma_32_installer.wxs
light -ext WixUtilExtension AutoFirma_32_installer.wixobj
move AutoFirma_32_installer.msi AutoFirma32
cd AutoFirma32
rename AutoFirma_32_installer.msi AutoFirma_32_v1_6_5_installer.msi
@pause