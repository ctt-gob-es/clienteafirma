#####################################################################
# New-SelfSignedCertificateEx.ps1
# Version 1.0
#
# Creates self-signed certificate. This tool is a base replacement
# for deprecated makecert.exe
#
# Vadims Podans (c) 2013
# http://en-us.sysadmins.lv/
#####################################################################
#requires -Version 2.0

function New-SelfSignedCertificateEx {
<#
.Synopsis
	This cmdlet generates a self-signed certificate.
.Description
	This cmdlet generates a self-signed certificate with the required data.
.Parameter Subject
	Specifies the certificate subject in a X500 distinguished name format.
	Example: CN=Test Cert, OU=Sandbox
.Parameter NotBefore
	Specifies the date and time when the certificate become valid. By default previous day
	date is used.
.Parameter NotAfter
	Specifies the date and time when the certificate expires. By default, the certificate is
	valid for 1 year.
.Parameter SerialNumber
	Specifies the desired serial number in a hex format.
	Example: 01a4ff2
.Parameter ProviderName
	Specifies the Cryptography Service Provider (CSP) name. You can use either legacy CSP
	and Key Storage Providers (KSP). By default "Microsoft Enhanced Cryptographic Provider v1.0"
	CSP is used.
.Parameter AlgorithmName
	Specifies the public key algorithm. By default RSA algorithm is used. RSA is the only
	algorithm supported by legacy CSPs. With key storage providers (KSP) you can use CNG
	algorithms, like ECDH. For CNG algorithms you must use full name:
	ECDH_P256
	ECDH_P384
	ECDH_P521
	
	In addition, KeyLength parameter must be specified explicitly when non-RSA algorithm is used.
.Parameter KeyLength
	Specifies the key length to generate. By default 2048-bit key is generated.
.Parameter KeySpec
	Specifies the public key operations type. The possible values are: Exchange and Signature.
	Default value is Exchange.
.Parameter EnhancedKeyUsage
	Specifies the intended uses of the public key contained in a certificate. You can
	specify either, EKU friendly name (for example 'Server Authentication') or
	object identifier (OID) value (for example '1.3.6.1.5.5.7.3.1').
.Parameter KeyUsages
	Specifies restrictions on the operations that can be performed by the public key contained in the certificate.
	Possible values (and their respective integer values to make bitwise operations) are:
	EncipherOnly
	CrlSign
	KeyCertSign
	KeyAgreement
	DataEncipherment
	KeyEncipherment
	NonRepudiation
	DigitalSignature
	DecipherOnly
	
	you can combine key usages values by using bitwise OR operation. when combining multiple
	flags, they must be enclosed in quotes and separated by a comma character. For example,
	to combine KeyEncipherment and DigitalSignature flags you should type:
	"KeyEncipherment, DigitalSignature".
	
	If the certificate is CA certificate (see IsCA parameter), key usages extension is generated
	automatically with the following key usages: Certificate Signing, Off-line CRL Signing, CRL Signing.
.Parameter SubjectAlternativeName
	Specifies alternative names for the subject. Unlike Subject field, this extension
	allows to specify more than one name. Also, multiple types of alternative names
	are supported. The cmdlet supports the following SAN types:
	RFC822 Name
	IP address (both, IPv4 and IPv6)
	Guid
	Directory name
	DNS name
.Parameter IsCA
	Specifies whether the certificate is CA (IsCA = $true) or end entity (IsCA = $false)
	certificate. If this parameter is set to $false, PathLength parameter is ignored.
	Basic Constraints extension is marked as critical.
.PathLength
	Specifies the number of additional CA certificates in the chain under this certificate. If
	PathLength parameter is set to zero, then no additional (subordinate) CA certificates are
	permitted under this CA.
.CustomExtension
	Specifies the custom extension to include to a self-signed certificate. This parameter
	must not be used to specify the extension that is supported via other parameters. In order
	to use this parameter, the extension must be formed in a collection of initialized
	System.Security.Cryptography.X509Certificates.X509Extension objects.
.Parameter SignatureAlgorithm
	Specifies signature algorithm used to sign the certificate. By default 'SHA1'
	algorithm is used.
.Parameter FriendlyName
	Specifies friendly name for the certificate.
.Parameter StoreLocation
	Specifies the store location to store self-signed certificate. Possible values are:
	'CurrentUser' and 'LocalMachine'. 'CurrentUser' store is intended for user certificates
	and computer (as well as CA) certificates must be stored in 'LocalMachine' store.
.Parameter StoreName
	Specifies the container name in the certificate store. Possible container names are:
	AddressBook
	AuthRoot
	CertificateAuthority
	Disallowed
	My
	Root
	TrustedPeople
	TrustedPublisher
.Parameter Path
	Specifies the path to a PFX file to export a self-signed certificate.
.Parameter Password
	Specifies the password for PFX file.
.Parameter AllowSMIME
	Enables Secure/Multipurpose Internet Mail Extensions for the certificate.
.Parameter Exportable
	Marks private key as exportable. Smart card providers usually do not allow
	exportable keys.
.Example
	New-SelfsignedCertificateEx -Subject "CN=Test Code Signing" -EKU "Code Signing" -KeySpec "Signature" `
	-KeyUsage "DigitalSignature" -FriendlyName "Test code signing" -NotAfter [datetime]::now.AddYears(5)
	
	Creates a self-signed certificate intended for code signing and which is valid for 5 years. Certificate
	is saved in the Personal store of the current user account.
.Example
	New-SelfsignedCertificateEx -Subject "CN=www.domain.com" -EKU "Server Authentication", "Client authentication" `
	-KeyUsage "KeyEcipherment, DigitalSignature" -SAN "sub.domain.com","www.domain.com","192.168.1.1" `
	-AllowSMIME -Path C:\test\ssl.pfx -Password (ConvertTo-SecureString "P@ssw0rd" -AsPlainText -Force) -Exportable `
	-StoreLocation "LocalMachine"
	
	Creates a self-signed SSL certificate with multiple subject names and saves it to a file. Additionally, the
	certificate is saved in the Personal store of the Local Machine store. Private key is marked as exportable,
	so you can export the certificate with a associated private key to a file at any time. The certificate
	includes SMIME capabilities.
.Example
	New-SelfsignedCertificateEx -Subject "CN=www.domain.com" -EKU "Server Authentication", "Client authentication" `
	-KeyUsage "KeyEcipherment, DigitalSignature" -SAN "sub.domain.com","www.domain.com","192.168.1.1" `
	-StoreLocation "LocalMachine" -ProviderName "Microsoft Software Key Storae Provider" -AlgorithmName ecdh_256 `
	-KeyLength 256 -SignatureAlgorithm sha256
	
	Creates a self-signed SSL certificate with multiple subject names and saves it to a file. Additionally, the
	certificate is saved in the Personal store of the Local Machine store. Private key is marked as exportable,
	so you can export the certificate with a associated private key to a file at any time. Certificate uses
	Ellyptic Curve Cryptography (ECC) key algorithm ECDH with 256-bit key. The certificate is signed by using
	SHA256 algorithm.
.Example
	New-SelfsignedCertificateEx -Subject "CN=Test Root CA, OU=Sandbox" -IsCA $true -ProviderName `
	"Microsoft Software Key Storage Provider" -Exportable
	
	Creates self-signed root CA certificate.
#>
[CmdletBinding(DefaultParameterSetName = '__store')]
	param (
		[Parameter(Mandatory = $true, Position = 0)]
		[string]$Subject,
		[Parameter(Position = 1)]
		[datetime]$NotBefore = [DateTime]::Now.AddDays(-1),
		[Parameter(Position = 2)]
		[datetime]$NotAfter = $NotBefore.AddDays(365),
		[string]$SerialNumber,
		[Alias('CSP')]
		[string]$ProviderName = "Microsoft Enhanced Cryptographic Provider v1.0",
		[string]$AlgorithmName = "RSA",
		[int]$KeyLength = 2048,
		[validateSet("Exchange","Signature")]
		[string]$KeySpec = "Exchange",
		[Alias('EKU')]
		[Security.Cryptography.Oid[]]$EnhancedKeyUsage,
		[Alias('KU')]
		[Security.Cryptography.X509Certificates.X509KeyUsageFlags]$KeyUsage,
		[Alias('SAN')]
		[String[]]$SubjectAlternativeName,
		[bool]$IsCA,
		[int]$PathLength = -1,
		[Security.Cryptography.X509Certificates.X509ExtensionCollection]$CustomExtension,
		[ValidateSet('MD5','SHA1','SHA256','SHA384','SHA512')]
		[string]$SignatureAlgorithm = "SHA1",
		[string]$FriendlyName,
		[Parameter(ParameterSetName = '__store')]
		[Security.Cryptography.X509Certificates.StoreLocation]$StoreLocation = "CurrentUser",
		[Parameter(ParameterSetName = '__store')]
		[Security.Cryptography.X509Certificates.StoreName]$StoreName = "My",
		[Parameter(Mandatory = $true, ParameterSetName = '__file')]
		[Alias('OutFile','OutPath','Out')]
		[IO.FileInfo]$Path,
		[Parameter(Mandatory = $true, ParameterSetName = '__file')]
		[Security.SecureString]$Password,
		[switch]$AllowSMIME,
		[switch]$Exportable
	)
	$ErrorActionPreference = "Stop"
	if ([Environment]::OSVersion.Version.Major -lt 6) {
		$NotSupported = New-Object NotSupportedException -ArgumentList "Windows XP and Windows Server 2003 are not supported!"
		throw $NotSupported
	}
	$ExtensionsToAdd = @()

#region constants
	# contexts
	New-Variable -Name UserContext -Value 0x1 -Option Constant
	New-Variable -Name MachineContext -Value 0x2 -Option Constant
	# encoding
	New-Variable -Name Base64Header -Value 0x0 -Option Constant
	New-Variable -Name Base64 -Value 0x1 -Option Constant
	New-Variable -Name Binary -Value 0x3 -Option Constant
	New-Variable -Name Base64RequestHeader -Value 0x4 -Option Constant
	# SANs
	New-Variable -Name OtherName -Value 0x1 -Option Constant
	New-Variable -Name RFC822Name -Value 0x2 -Option Constant
	New-Variable -Name DNSName -Value 0x3 -Option Constant
	New-Variable -Name DirectoryName -Value 0x5 -Option Constant
	New-Variable -Name URL -Value 0x7 -Option Constant
	New-Variable -Name IPAddress -Value 0x8 -Option Constant
	New-Variable -Name RegisteredID -Value 0x9 -Option Constant
	New-Variable -Name Guid -Value 0xa -Option Constant
	New-Variable -Name UPN -Value 0xb -Option Constant
	# installation options
	New-Variable -Name AllowNone -Value 0x0 -Option Constant
	New-Variable -Name AllowNoOutstandingRequest -Value 0x1 -Option Constant
	New-Variable -Name AllowUntrustedCertificate -Value 0x2 -Option Constant
	New-Variable -Name AllowUntrustedRoot -Value 0x4 -Option Constant
	# PFX export options
	New-Variable -Name PFXExportEEOnly -Value 0x0 -Option Constant
	New-Variable -Name PFXExportChainNoRoot -Value 0x1 -Option Constant
	New-Variable -Name PFXExportChainWithRoot -Value 0x2 -Option Constant
#endregion
	
#region Subject processing
	# http://msdn.microsoft.com/en-us/library/aa377051(VS.85).aspx
	$SubjectDN = New-Object -ComObject X509Enrollment.CX500DistinguishedName
	$SubjectDN.Encode($Subject, 0x0)
#endregion

#region Extensions

#region Enhanced Key Usages processing
	if ($EnhancedKeyUsage) {
		$OIDs = New-Object -ComObject X509Enrollment.CObjectIDs
		$EnhancedKeyUsage | %{
			$OID = New-Object -ComObject X509Enrollment.CObjectID
			$OID.InitializeFromValue($_.Value)
			# http://msdn.microsoft.com/en-us/library/aa376785(VS.85).aspx
			$OIDs.Add($OID)
		}
		# http://msdn.microsoft.com/en-us/library/aa378132(VS.85).aspx
		$EKU = New-Object -ComObject X509Enrollment.CX509ExtensionEnhancedKeyUsage
		$EKU.InitializeEncode($OIDs)
		$ExtensionsToAdd += "EKU"
	}
#endregion

#region Key Usages processing
	if ($KeyUsage -ne $null) {
		$KU = New-Object -ComObject X509Enrollment.CX509ExtensionKeyUsage
		$KU.InitializeEncode([int]$KeyUsage)
		$KU.Critical = $true
		$ExtensionsToAdd += "KU"
	}
#endregion

#region Basic Constraints processing
	if ($PSBoundParameters.Keys.Contains("IsCA")) {
		# http://msdn.microsoft.com/en-us/library/aa378108(v=vs.85).aspx
		$BasicConstraints = New-Object -ComObject X509Enrollment.CX509ExtensionBasicConstraints
		if (!$IsCA) {$PathLength = -1}
		$BasicConstraints.InitializeEncode($IsCA,$PathLength)
		$BasicConstraints.Critical = $IsCA
		$ExtensionsToAdd += "BasicConstraints"
	}
#endregion

#region SAN processing
	if ($SubjectAlternativeName) {
		$SAN = New-Object -ComObject X509Enrollment.CX509ExtensionAlternativeNames
		$Names = New-Object -ComObject X509Enrollment.CAlternativeNames
		foreach ($altname in $SubjectAlternativeName) {
			$Name = New-Object -ComObject X509Enrollment.CAlternativeName
			if ($altname.Contains("@")) {
				$Name.InitializeFromString($RFC822Name,$altname)
			} else {
				try {
					$Bytes = [Net.IPAddress]::Parse($altname).GetAddressBytes()
					$Name.InitializeFromRawData($IPAddress,$Base64,[Convert]::ToBase64String($Bytes))
				} catch {
					try {
						$Bytes = [Guid]::Parse($altname).ToByteArray()
						$Name.InitializeFromRawData($Guid,$Base64,[Convert]::ToBase64String($Bytes))
					} catch {
						try {
							$Bytes = ([Security.Cryptography.X509Certificates.X500DistinguishedName]$altname).RawData
							$Name.InitializeFromRawData($DirectoryName,$Base64,[Convert]::ToBase64String($Bytes))
						} catch {$Name.InitializeFromString($DNSName,$altname)}
					}
				}
			}
			$Names.Add($Name)
		}
		$SAN.InitializeEncode($Names)
		$ExtensionsToAdd += "SAN"
	}
#endregion

#region Custom Extensions
	if ($CustomExtension) {
		$count = 0
		foreach ($ext in $CustomExtension) {
			# http://msdn.microsoft.com/en-us/library/aa378077(v=vs.85).aspx
			$Extension = New-Object -ComObject X509Enrollment.CX509Extension
			$EOID = New-Object -ComObject X509Enrollment.CObjectId
			$EOID.InitializeFromValue($ext.Oid.Value)
			$EValue = [Convert]::ToBase64String($ext.RawData)
			$Extension.Initialize($EOID,$Base64,$EValue)
			$Extension.Critical = $ext.Critical
			New-Variable -Name ("ext" + $count) -Value $Extension
			$ExtensionsToAdd += ("ext" + $count)
			$count++
		}
	}
#endregion

#endregion

#region Private Key
	# http://msdn.microsoft.com/en-us/library/aa378921(VS.85).aspx
	$PrivateKey = New-Object -ComObject X509Enrollment.CX509PrivateKey
	$PrivateKey.ProviderName = $ProviderName
	$AlgID = New-Object -ComObject X509Enrollment.CObjectId
	$AlgID.InitializeFromValue(([Security.Cryptography.Oid]$AlgorithmName).Value)
	$PrivateKey.Algorithm = $AlgID
	# http://msdn.microsoft.com/en-us/library/aa379409(VS.85).aspx
	$PrivateKey.KeySpec = switch ($KeySpec) {"Exchange" {1}; "Signature" {2}}
	$PrivateKey.Length = $KeyLength
	# key will be stored in current user certificate store
	switch ($PSCmdlet.ParameterSetName) {
		'__store' {
			$PrivateKey.MachineContext = if ($StoreLocation -eq "LocalMachine") {$true} else {$false}
		}
		'__file' {
			$PrivateKey.MachineContext = $false
		}
	}
	$PrivateKey.ExportPolicy = if ($Exportable) {1} else {0}
	$PrivateKey.Create()
#endregion

	# http://msdn.microsoft.com/en-us/library/aa377124(VS.85).aspx
	$Cert = New-Object -ComObject X509Enrollment.CX509CertificateRequestCertificate
	if ($PrivateKey.MachineContext) {
		$Cert.InitializeFromPrivateKey($MachineContext,$PrivateKey,"")
	} else {
		$Cert.InitializeFromPrivateKey($UserContext,$PrivateKey,"")
	}
	$Cert.Subject = $SubjectDN
	$Cert.Issuer = $Cert.Subject
	$Cert.NotBefore = $NotBefore
	$Cert.NotAfter = $NotAfter
	foreach ($item in $ExtensionsToAdd) {$Cert.X509Extensions.Add((Get-Variable -Name $item -ValueOnly))}
	if (![string]::IsNullOrEmpty($SerialNumber)) {
		if ($SerialNumber -match "[^0-9a-fA-F]") {throw "Invalid serial number specified."}
		if ($SerialNumber.Length % 2) {$SerialNumber = "0" + $SerialNumber}
		$Bytes = $SerialNumber -split "(.{2})" | ?{$_} | %{[Convert]::ToByte($_,16)}
		$ByteString = [Convert]::ToBase64String($Bytes)
		$Cert.SerialNumber.InvokeSet($ByteString,1)
	}
	if ($AllowSMIME) {$Cert.SmimeCapabilities = $true}
	$SigOID = New-Object -ComObject X509Enrollment.CObjectId
	$SigOID.InitializeFromValue(([Security.Cryptography.Oid]$SignatureAlgorithm).Value)
	$Cert.SignatureInformation.HashAlgorithm = $SigOID
	# completing certificate request template building
	$Cert.Encode()
	
	# interface: http://msdn.microsoft.com/en-us/library/aa377809(VS.85).aspx
	$Request = New-Object -ComObject X509Enrollment.CX509enrollment
	$Request.InitializeFromRequest($Cert)
	$Request.CertificateFriendlyName = $FriendlyName
	$endCert = $Request.CreateRequest($Base64)
	$Request.InstallResponse($AllowUntrustedCertificate,$endCert,$Base64,"")
	switch ($PSCmdlet.ParameterSetName) {
		'__file' {
			$PFXString = $Request.CreatePFX(
				[Runtime.InteropServices.Marshal]::PtrToStringAuto([Runtime.InteropServices.Marshal]::SecureStringToBSTR($Password)),
				$PFXExportEEOnly,
				$Base64
			)
			Set-Content -Path $Path -Value ([Convert]::FromBase64String($PFXString)) -Encoding Byte
		}
	}
}
# SIG # Begin signature block
# MIIT9wYJKoZIhvcNAQcCoIIT6DCCE+QCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUAeqLu7+6JFZ1eT+FMVIQ0Tgq
# Y0Gggg8tMIID7jCCA1egAwIBAgIQfpPr+3zGTlnqS5p31Ab8OzANBgkqhkiG9w0B
# AQUFADCBizELMAkGA1UEBhMCWkExFTATBgNVBAgTDFdlc3Rlcm4gQ2FwZTEUMBIG
# A1UEBxMLRHVyYmFudmlsbGUxDzANBgNVBAoTBlRoYXd0ZTEdMBsGA1UECxMUVGhh
# d3RlIENlcnRpZmljYXRpb24xHzAdBgNVBAMTFlRoYXd0ZSBUaW1lc3RhbXBpbmcg
# Q0EwHhcNMTIxMjIxMDAwMDAwWhcNMjAxMjMwMjM1OTU5WjBeMQswCQYDVQQGEwJV
# UzEdMBsGA1UEChMUU3ltYW50ZWMgQ29ycG9yYXRpb24xMDAuBgNVBAMTJ1N5bWFu
# dGVjIFRpbWUgU3RhbXBpbmcgU2VydmljZXMgQ0EgLSBHMjCCASIwDQYJKoZIhvcN
# AQEBBQADggEPADCCAQoCggEBALGss0lUS5ccEgrYJXmRIlcqb9y4JsRDc2vCvy5Q
# WvsUwnaOQwElQ7Sh4kX06Ld7w3TMIte0lAAC903tv7S3RCRrzV9FO9FEzkMScxeC
# i2m0K8uZHqxyGyZNcR+xMd37UWECU6aq9UksBXhFpS+JzueZ5/6M4lc/PcaS3Er4
# ezPkeQr78HWIQZz/xQNRmarXbJ+TaYdlKYOFwmAUxMjJOxTawIHwHw103pIiq8r3
# +3R8J+b3Sht/p8OeLa6K6qbmqicWfWH3mHERvOJQoUvlXfrlDqcsn6plINPYlujI
# fKVOSET/GeJEB5IL12iEgF1qeGRFzWBGflTBE3zFefHJwXECAwEAAaOB+jCB9zAd
# BgNVHQ4EFgQUX5r1blzMzHSa1N197z/b7EyALt0wMgYIKwYBBQUHAQEEJjAkMCIG
# CCsGAQUFBzABhhZodHRwOi8vb2NzcC50aGF3dGUuY29tMBIGA1UdEwEB/wQIMAYB
# Af8CAQAwPwYDVR0fBDgwNjA0oDKgMIYuaHR0cDovL2NybC50aGF3dGUuY29tL1Ro
# YXd0ZVRpbWVzdGFtcGluZ0NBLmNybDATBgNVHSUEDDAKBggrBgEFBQcDCDAOBgNV
# HQ8BAf8EBAMCAQYwKAYDVR0RBCEwH6QdMBsxGTAXBgNVBAMTEFRpbWVTdGFtcC0y
# MDQ4LTEwDQYJKoZIhvcNAQEFBQADgYEAAwmbj3nvf1kwqu9otfrjCR27T4IGXTdf
# plKfFo3qHJIJRG71betYfDDo+WmNI3MLEm9Hqa45EfgqsZuwGsOO61mWAK3ODE2y
# 0DGmCFwqevzieh1XTKhlGOl5QGIllm7HxzdqgyEIjkHq3dlXPx13SYcqFgZepjhq
# IhKjURmDfrYwggSjMIIDi6ADAgECAhAOz/Q4yP6/NW4E2GqYGxpQMA0GCSqGSIb3
# DQEBBQUAMF4xCzAJBgNVBAYTAlVTMR0wGwYDVQQKExRTeW1hbnRlYyBDb3Jwb3Jh
# dGlvbjEwMC4GA1UEAxMnU3ltYW50ZWMgVGltZSBTdGFtcGluZyBTZXJ2aWNlcyBD
# QSAtIEcyMB4XDTEyMTAxODAwMDAwMFoXDTIwMTIyOTIzNTk1OVowYjELMAkGA1UE
# BhMCVVMxHTAbBgNVBAoTFFN5bWFudGVjIENvcnBvcmF0aW9uMTQwMgYDVQQDEytT
# eW1hbnRlYyBUaW1lIFN0YW1waW5nIFNlcnZpY2VzIFNpZ25lciAtIEc0MIIBIjAN
# BgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAomMLOUS4uyOnREm7Dv+h8GEKU5Ow
# mNutLA9KxW7/hjxTVQ8VzgQ/K/2plpbZvmF5C1vJTIZ25eBDSyKV7sIrQ8Gf2Gi0
# jkBP7oU4uRHFI/JkWPAVMm9OV6GuiKQC1yoezUvh3WPVF4kyW7BemVqonShQDhfu
# ltthO0VRHc8SVguSR/yrrvZmPUescHLnkudfzRC5xINklBm9JYDh6NIipdC6Anqh
# d5NbZcPuF3S8QYYq3AhMjJKMkS2ed0QfaNaodHfbDlsyi1aLM73ZY8hJnTrFxeoz
# C9Lxoxv0i77Zs1eLO94Ep3oisiSuLsdwxb5OgyYI+wu9qU+ZCOEQKHKqzQIDAQAB
# o4IBVzCCAVMwDAYDVR0TAQH/BAIwADAWBgNVHSUBAf8EDDAKBggrBgEFBQcDCDAO
# BgNVHQ8BAf8EBAMCB4AwcwYIKwYBBQUHAQEEZzBlMCoGCCsGAQUFBzABhh5odHRw
# Oi8vdHMtb2NzcC53cy5zeW1hbnRlYy5jb20wNwYIKwYBBQUHMAKGK2h0dHA6Ly90
# cy1haWEud3Muc3ltYW50ZWMuY29tL3Rzcy1jYS1nMi5jZXIwPAYDVR0fBDUwMzAx
# oC+gLYYraHR0cDovL3RzLWNybC53cy5zeW1hbnRlYy5jb20vdHNzLWNhLWcyLmNy
# bDAoBgNVHREEITAfpB0wGzEZMBcGA1UEAxMQVGltZVN0YW1wLTIwNDgtMjAdBgNV
# HQ4EFgQURsZpow5KFB7VTNpSYxc/Xja8DeYwHwYDVR0jBBgwFoAUX5r1blzMzHSa
# 1N197z/b7EyALt0wDQYJKoZIhvcNAQEFBQADggEBAHg7tJEqAEzwj2IwN3ijhCcH
# bxiy3iXcoNSUA6qGTiWfmkADHN3O43nLIWgG2rYytG2/9CwmYzPkSWRtDebDZw73
# BaQ1bHyJFsbpst+y6d0gxnEPzZV03LZc3r03H0N45ni1zSgEIKOq8UvEiCmRDoDR
# EfzdXHZuT14ORUZBbg2w6jiasTraCXEQ/Bx5tIB7rGn0/Zy2DBYr8X9bCT2bW+IW
# yhOBbQAuOA2oKY8s4bL0WqkBrxWcLC9JG9siu8P+eJRRw4axgohd8D20UaF5Mysu
# e7ncIAkTcetqGVvP6KUwVyyJST+5z3/Jvz4iaGNTmr1pdKzFHTx/kuDDvBzYBHUw
# ggaQMIIFeKADAgECAhAGnC2gXFmy7q5ox0B+K5/xMA0GCSqGSIb3DQEBBQUAMG8x
# CzAJBgNVBAYTAlVTMRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3
# dy5kaWdpY2VydC5jb20xLjAsBgNVBAMTJURpZ2lDZXJ0IEFzc3VyZWQgSUQgQ29k
# ZSBTaWduaW5nIENBLTEwHhcNMTMwMTI4MDAwMDAwWhcNMTQwMjA1MTIwMDAwWjBc
# MQswCQYDVQQGEwJMVjEKMAgGA1UECBMBLTENMAsGA1UEBxMEUmlnYTEYMBYGA1UE
# ChMPU3lzYWRtaW5zIExWIElLMRgwFgYDVQQDEw9TeXNhZG1pbnMgTFYgSUswggEi
# MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQChzYQDqMuYBs/jnfLsMvbbuZTV
# wwY1yHJ92TvD7bNwVx1OFEENNGrXkLNz9Ro6XtJ8zcB/80FmxE9jL2ARLd2TmEJt
# aYBvvmGMsS17zCGYDZZU7aVjaKZX2R665V+LWJUEIaHCcY5XjfmeZvCk1tHOtTAX
# qKjUd6fGIWXpxrSP9WKxW7FpTDGzQ2BpkZ+snmPS9yWDgeu709zPeoSTbdEIva6J
# ckzFj0uK7k2BqlLG3dsxBIzUqr+yTbdAuWfhR731iyWHk5GT6XCtBjBmuouKOCT1
# Jn0xmYNAwgdtSiBlTL4A/Rm3YuP57VP+EBrrgA5g7Pekdo9APU+7QqWF51YhAgMB
# AAGjggM5MIIDNTAfBgNVHSMEGDAWgBR7aM4pqsAXvkl64eU/1qf3RY81MjAdBgNV
# HQ4EFgQUgiIRdHkZ2SctPGaLDBBOX67N7NQwDgYDVR0PAQH/BAQDAgeAMBMGA1Ud
# JQQMMAoGCCsGAQUFBwMDMHMGA1UdHwRsMGowM6AxoC+GLWh0dHA6Ly9jcmwzLmRp
# Z2ljZXJ0LmNvbS9hc3N1cmVkLWNzLTIwMTFhLmNybDAzoDGgL4YtaHR0cDovL2Ny
# bDQuZGlnaWNlcnQuY29tL2Fzc3VyZWQtY3MtMjAxMWEuY3JsMIIBxAYDVR0gBIIB
# uzCCAbcwggGzBglghkgBhv1sAwEwggGkMDoGCCsGAQUFBwIBFi5odHRwOi8vd3d3
# LmRpZ2ljZXJ0LmNvbS9zc2wtY3BzLXJlcG9zaXRvcnkuaHRtMIIBZAYIKwYBBQUH
# AgIwggFWHoIBUgBBAG4AeQAgAHUAcwBlACAAbwBmACAAdABoAGkAcwAgAEMAZQBy
# AHQAaQBmAGkAYwBhAHQAZQAgAGMAbwBuAHMAdABpAHQAdQB0AGUAcwAgAGEAYwBj
# AGUAcAB0AGEAbgBjAGUAIABvAGYAIAB0AGgAZQAgAEQAaQBnAGkAQwBlAHIAdAAg
# AEMAUAAvAEMAUABTACAAYQBuAGQAIAB0AGgAZQAgAFIAZQBsAHkAaQBuAGcAIABQ
# AGEAcgB0AHkAIABBAGcAcgBlAGUAbQBlAG4AdAAgAHcAaABpAGMAaAAgAGwAaQBt
# AGkAdAAgAGwAaQBhAGIAaQBsAGkAdAB5ACAAYQBuAGQAIABhAHIAZQAgAGkAbgBj
# AG8AcgBwAG8AcgBhAHQAZQBkACAAaABlAHIAZQBpAG4AIABiAHkAIAByAGUAZgBl
# AHIAZQBuAGMAZQAuMIGCBggrBgEFBQcBAQR2MHQwJAYIKwYBBQUHMAGGGGh0dHA6
# Ly9vY3NwLmRpZ2ljZXJ0LmNvbTBMBggrBgEFBQcwAoZAaHR0cDovL2NhY2VydHMu
# ZGlnaWNlcnQuY29tL0RpZ2lDZXJ0QXNzdXJlZElEQ29kZVNpZ25pbmdDQS0xLmNy
# dDAMBgNVHRMBAf8EAjAAMA0GCSqGSIb3DQEBBQUAA4IBAQAh3nRpJ8WxlJZ8NI1y
# B4iM7RzjL7D57lVj/shWkbCp2znzBLVMGnYVK+Z0QL2PSxpxX52Khhc2MHXTM+Yf
# 74sO5XZm5IMMAnlpK2FeyQBGIKcFmrzkvj3LUcCc7RU0duioVHQ+C+hOQmpmSYiA
# 0zOoJgO4zFy5SKT1mzPEElup1B2aiE+WQZpcEWUv4I+/lYvYIBhyz+WZ2xm4kLbG
# QYR/08cei9X70x02wpgMSK9yKhSzcpwbq+ccnOtFUlTLNyRr9OuRnTi3ZCM8w5Is
# a2+UsnxsF5F5CGsw+GMRT/Jrm2mHMcKIW+qp8reUXattRTjobnbARJSQS3NBt4wp
# wTIZMYIENDCCBDACAQEwgYMwbzELMAkGA1UEBhMCVVMxFTATBgNVBAoTDERpZ2lD
# ZXJ0IEluYzEZMBcGA1UECxMQd3d3LmRpZ2ljZXJ0LmNvbTEuMCwGA1UEAxMlRGln
# aUNlcnQgQXNzdXJlZCBJRCBDb2RlIFNpZ25pbmcgQ0EtMQIQBpwtoFxZsu6uaMdA
# fiuf8TAJBgUrDgMCGgUAoHgwGAYKKwYBBAGCNwIBDDEKMAigAoAAoQKAADAZBgkq
# hkiG9w0BCQMxDAYKKwYBBAGCNwIBBDAcBgorBgEEAYI3AgELMQ4wDAYKKwYBBAGC
# NwIBFTAjBgkqhkiG9w0BCQQxFgQU/ix9ZAr39xxjbsYVBZ5BgjBjq+MwDQYJKoZI
# hvcNAQEBBQAEggEAI0yNayiE8E3Ttsmvd5tvmAUN9ngGpez7qvGtjcUEFXyjRHjW
# d3XieT98UHYL1/+IoX6QiEXP/t5SreMqQmql9wvlhyDt2Qw2+E80MXFCGvKuAJAp
# LvTY3F4i9fY+wCT33B2dDUSAI+mFGbJxl1GgulqSxLOUiGnlpWvtJ07lgSki6xN0
# MTlNCt4xem5P43iRLFtlkEJjYpznClPx0Ipu1IG3gND4hos5jVofxJ5ZcVbMQ07c
# +9Rq4iTccmNE9oV4H0ZRRCrJbNfIart3je0/aSeOpo1WGwZr6hyJEvETGruyDbSW
# uK9kIfAZbVSvpx4clfeP9SIxcyhom1e8xp6daKGCAgswggIHBgkqhkiG9w0BCQYx
# ggH4MIIB9AIBATByMF4xCzAJBgNVBAYTAlVTMR0wGwYDVQQKExRTeW1hbnRlYyBD
# b3Jwb3JhdGlvbjEwMC4GA1UEAxMnU3ltYW50ZWMgVGltZSBTdGFtcGluZyBTZXJ2
# aWNlcyBDQSAtIEcyAhAOz/Q4yP6/NW4E2GqYGxpQMAkGBSsOAwIaBQCgXTAYBgkq
# hkiG9w0BCQMxCwYJKoZIhvcNAQcBMBwGCSqGSIb3DQEJBTEPFw0xMzA0MTQxMDU2
# NTFaMCMGCSqGSIb3DQEJBDEWBBR3BvjwJvcH3OSIjiXabjSSjC3hizANBgkqhkiG
# 9w0BAQEFAASCAQBa1lxJa2qVZmZKDY2Rm7XNJEIoxOzdw+WZViQC9qsi9FrFVPwj
# y3UjlfVm5AynoSXs0+mXgyU4xW3c/xDgFKEyLN+mocD8mx+ufc/bG1l0SWVYITgV
# WiQe5G0oOW/mRQNLhaiJGE3fZuxvTbN0rGYIOeZ5Nsl6yyJpsx96em0skNk2XxVA
# rYoniI/OCP7a9VyqXZr/ScgG18ZreKZikyrjBz6zQh9ibrApLprm/nJ+7go+yeHA
# hqUEwDtiRAyCJfH3DKLPCm6YqWOaT07+LWZSm2DM+mcRydj7gKGoOILfgLhjnUdz
# IJUMgUNRqvIxWuUtU1wAquBSObIOtRD4hkBG
# SIG # End signature block
