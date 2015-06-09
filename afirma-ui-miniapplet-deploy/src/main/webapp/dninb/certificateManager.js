
$( document ).ready(function(){
	certificateManager.init();
})

certificateManager = (function (){
	
	var certStartWith = "-----BEGIN CERTIFICATE-----";
	var certEndWith = "-----END CERTIFICATE-----";
	
	function getCertificate(){
		return $("#selectable").find(".ui-selected").data("cert");
	};
	
	function init(){
		    $( "#selectable" ).selectable({
		        cancel: 'a'
		    });
		  certificateManager.showCertificates();
	}
	
	var getCertificates = function(){
		
		var res = new Array();
		var cert= "-----BEGIN CERTIFICATE-----"+
				"MIIDMjCCAhqgAwIBAgIJAKMfG/B2MKd5MA0GCSqGSIb3DQEBBQUAMBoxCzAJBgNV"+
				"BAYTAkpQMQswCQYDVQQKEwJ6MzAeFw0xMDA1MzEwNjE4MDhaFw0yMDA1MjgwNjE4"+
				"MDhaMBoxCzAJBgNVBAYTAkpQMQswCQYDVQQKEwJ6MzCCASIwDQYJKoZIhvcNAQEB"+
				"BQADggEPADCCAQoCggEBAOKosPD1rOz6Z5wtgVBKA0XMFxfKavF09f9x3N3LlCcF"+
				"/6LTXn6/waJq93bTNBESzdw63qQl77sxFORXCVQ1Ad7CPqRbY4ywVw/8yZri9wH4"+
				"PdSfae/fg5vuN3EesLissFSceQySHbMBeqEAw84cLvPXdlecdNmlwfS6nV5D8Ijt"+
				"HkebP8R14AEpfoag3VS/YS6Hd4hmvZ1e3BJQm3JfFFVzTyZ0AH2oimsxfoQnZf7M"+
				"qyIdCgmeaKYILEyYm6PKhVwZhuD5E+WG4RxNdcGCiC8GRUr2fYnXUT0CtwrYBYgZ"+
				"121cFObeu7Z/aTIsx4MqYLHwaHqMga6U7nOHoEgqY7ECAwEAAaN7MHkwHQYDVR0O"+
				"BBYEFKO4NcUDh3J5c7XD7j4pVXnzIfALMEoGA1UdIwRDMEGAFKO4NcUDh3J5c7XD"+
				"7j4pVXnzIfALoR6kHDAaMQswCQYDVQQGEwJKUDELMAkGA1UEChMCejOCCQCjHxvw"+
				"djCneTAMBgNVHRMEBTADAQH/MA0GCSqGSIb3DQEBBQUAA4IBAQC8JdiwJF22/3nB"+
				"IxJT/gXXN10cub6O+x9q64ls7dpGpBvbi4/lJgZOsZqoJiswU5WOKZ4MTOmMHe4W"+
				"e/MHuhcjsgf9EHHYZQ1reBYi/l9mBBbYFGs0zSv1CyjbwkyF36nr/8sWdYf4ZtXQ"+
				"nzTGvoa6oTOOTmmj3Bwl3CHwonvgAJUCHY/UmWFzH8Sf0dDW7iJBj+ZWfjuSlSQe"+
				"2ninrEpfA4v2V1p3LOH+layZLDMJHkNCq8eoU1MbJi07cHxLWtlwliNOiRboaiYl"+
				"1wtWR7ZY4HZCPeyb0tanf58rBQAXElaCF3fmfHrlpxoJBsQP1NbFrBs2haOIEZ4E"+
				"K3V9/Bpi"+
				"-----END CERTIFICATE-----";
		var cert2 = "-----BEGIN CERTIFICATE-----MIIFZjCCBE6gAwIBAgICAPkwDQYJKoZIhvcNAQEFBQAwNjEQMA4GA1UECgwHUmVuYXVsdDEiMCAG"+
"A1UEAwwZQ2xhc3MgMSBBdXRoZW50aWNhdGlvbiBDQTAeFw0xMzA0MjUwNzAwMTFaFw0xNTA0MjYw"+
"NzAwMTFaMHkxEDAOBgNVBAoMB1JlbmF1bHQxHjAcBgNVBAMMFVJEIChSQ0kgRVMpIElSTi01OTky"+
"MTEsMCoGCSqGSIb3DQEJARYdc3Rlcmdpb3MucmFwdGlzQHJjaWJhbnF1ZS5jb20xFzAVBgoJkiaJ"+
"k/IsZAEBDAdhd3JjaW4xMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAxEjZToJQPMTZ"+
"Gx4Di/BCKoyy1CHrJrH0iOB7LqS0n11DxCvnKCdFSB8/BIYySTH3f3FNnZzKETIPrBlL6IlDJvx2"+
"kJ1sUFLOQ/3ct9guhHTf9gZh70mgph+ISg7QnxHu8KZnRnK4lZl5oCJ2tXkAabovWSW0+wPwotGQ"+
"F5HbYna/nWdEHCTpuVDJLkkhYiYuXmIoc59lsoUtcEdJCHbgUTb0x174w4wPmT9bZE8nHZERaugI"+
"DvHoOtdH6fnP3JQuU671+JyP8aTqGw5xWjVzQ0q5VPOS38/kNFgjHciZmNbQLmiZ7X+671Gyhfb6"+
"D51JkJ1n4WPtfXdCdPRkSltiRQIDAQABo4ICOTCCAjUwHQYDVR0OBBYEFBam437LLXZZ46kEen+l"+
"xeS/GdbkMB8GA1UdIwQYMBaAFJq9r9bZeU+BFIzTo7kIl7aFsYRPMIHlBgNVHSAEgd0wgdowgdcG"+
"CisGAQQBqhEDEgEwgcgwYgYIKwYBBQUHAgEWVmh0dHA6Ly9jcmwucmVuYXVsdC5mci9DZXJ0aWZp"+
"Y2F0aW9uUG9saWNpZXMvUmVuYXVsdEludGVybmFsL0NsYXNzMUF1dGhlbnRpY2F0aW9uQ0EucGRm"+
"MGIGCCsGAQUFBwICMFYwExYHUkVOQVVMVDAIAgYMYcrPIX0aP1JlbmF1bHQgSW50ZXJuYWwgQ2xh"+
"c3MgMSBBdXRoZW50aWNhdGlvbiBDQSBDZXJ0aWZpY2F0aW9uIFBvbGljeTARBglghkgBhvhCAQEE"+
"BAMCBaAwKQYDVR0lBCIwIAYIKwYBBQUHAwIGCCsGAQUFBwMEBgorBgEEAYI3FAICMA4GA1UdDwEB"+
"/wQEAwIF4DAoBgNVHREEITAfgR1zdGVyZ2lvcy5yYXB0aXNAcmNpYmFucXVlLmNvbTBLBgNVHR8E"+
"RDBCMECgPqA8hjpodHRwOi8vY3JsLnJlbmF1bHQuZnIvQ1JML1JlbmF1bHRJbnRlcm5hbC9DbGFz"+
"czFBdXRoQ0EuY3JsMEYGCCsGAQUFBwEBBDowODA2BggrBgEFBQcwAYYqaHR0cDovL2NybC5yZW5h"+
"dWx0LmZyL29jc3AvUmVuYXVsdEludGVybmFsMA0GCSqGSIb3DQEBBQUAA4IBAQBppA/CU2z9mvtv"+
"O/XzUHjWilfI562gxfZ58DR5iJfx7iOQt001Ci4IOYHdLutCdxzn+7LUCqWDDqiunKIHksjNj5aJ"+
"EDl+02YEX06vCvsW3TErTbBItXkVUhU/RU+tKhizsM8FgAWuqhFfRNtLiZqfQp2fTPqZDym49GSF"+
"mG5aAyggYzCVEnAn/7G878zXyITpyxOhrmC3FZO+QXnhaTaZIC3zxOKTcqTi1t8YM+4LAv6YwWI7"+
"fjnOdSP3I2BEiefynlO37dAuhuLu6jCimJzhxPYm4v/oIoXXMr6RwrCCEW5jygkCu9Jh07+NSpDU"+
"FwRPQBTVm6yfv367xQRimWgf-----END CERTIFICATE-----";
		
		var cert3 = "-----BEGIN CERTIFICATE-----MIIFnTCCBIWgAwIBAgICA+owDQYJKoZIhvcNAQEFBQAwgdoxCzAJBgNVBAYTAkVTMRIwEAYDVQQIEwlCYXJjZWxvbmExSDBGBgNVBAcMP0JhcmNlbG9uYSAoc2VlIGN1cnJlbnQgYWRkcmVzcyBhdCBodHRwczovL3d3dy5hbmYuZXMvYWRkcmVzcy8gKTEnMCUGA1UEChMeQU5GIEF1dG9yaWRhZCBkZSBDZXJ0aWZpY2FjaW9uMRcwFQYDVQQLEw5BTkYgQ2xhc2UgMSBDQTETMBEGA1UEBRMKRy02MzI4NzUxMDEWMBQGA1UEAxMNQU5GIFNlcnZlciBDQTAeFw0wNjEyMzEyMzAwMDBaFw0xNDEyMzEyMzAwMDBaMIGmMRswGQYDVQQDExJBTkYgVXN1YXJpbyBBY3Rpdm8xDDAKBgNVBCoTA0FORjEXMBUGA1UEBBMOVXN1YXJpbyBBY3Rpdm8xEjAQBgNVBAUTCTEyMzQ1Njc4WjEeMBwGCSqGSIb3DQEJARYPdGVzdEBwcnVlYmEuY29tMR8wHQYDVQQLExZDbGFzZSAyIHBlcnNvbmEgZmlzaWNhMQswCQYDVQQGEwJFUzCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAj2qAceOf0pyATEM0BxBK7+eGA0HEZWDZpqdhCeVvsI1AqhLWQpWNg65TGXE8ijzxGU/yS94k/34gPgIkla+p/mrDaNsVY69RcLp1hWYcL61rM//In+hXlA3qUK6as942b55YyzNsbJSQPCNgkiGuIQTo1Xfsfk4XZDi+yNSRgUMCAwEAAaOCAiEwggIdMAkGA1UdEwQCMAAwCwYDVR0PBAQDAgbAMBMGCisGAQQBgY8cFAMEBQwDQU5GMBcGCisGAQQBgY8cFAQECQwHVXN1YXJpbzAWBgorBgEEAYGPHBQFBAgMBkFjdGl2bzAZBgorBgEEAYGPHBQGBAsMCTEyMzQ1Njc4WjCBiAYDVR0gBIGAMH4wfAYKKwYBBAGBjxwDBDBuMD0GCCsGAQUFBwICMDEaL0NlcnRpZmljYWRvIGVtaXRpZG8gcGFyYSByZWFsaXphY2nzbiBkZSBwcnVlYmFzMC0GCCsGAQUFBwIBFiFodHRwczovL3d3dy5hbmYuZXMvQUMvZG9jdW1lbnRvcy8wOAYIKwYBBQUHAQEELDAqMCgGCCsGAQUFBzABhhxodHRwOi8vd3d3LmFuZi5lcy9BQy9SQy9vY3NwMDkGA1UdHwQyMDAwLqAsoCqGKGh0dHA6Ly93d3cuYW5mLmVzL0FDL1JDL0FORkFDQ0xBU0VBMS5jcmwwFwYKKwYBBAGBjxwTAQQJDAcxMjMtMzIxMDEGCisGAQQBgY8cKgYEIwwhaHR0cHM6Ly93d3cuYW5mLmVzL0FDL0FDVEFTLzU2Nzg5MBYGCSsGAQQBgY8cEwQJDAczMjEtMTIzMB0GA1UdDgQWBBSxTxAznF2uoOtMW+fJUoDN6B+rJDAfBgNVHSMEGDAWgBS+O/a0MbdzJEg5xVcTlHWqn4E/LDANBgkqhkiG9w0BAQUFAAOCAQEATQgYAOwxrMRTT2Nhx7pqiNsoGT5dJmeunAv+iU5zx/VoEXB/mx+VtyLfMea3VS9LC23404XS7pz5oPwiVPLsMPZtzOcmfacVnSdRn5J7+qOO8MB+OVlXq/QmARn+1XeBCHaTQ6AMc/pdveEoGktaXwEjTslWyRD9dGDzLp04+FndQAbVcI5xRkb4vToRnhQmloUVddhQAO8usOAIb00GJFNTq4lsyZ1qT1HplQl+ngsSD1HBxkhx10Pm3KuvCunAh4um0QnSeeiq9qWIV0UZrFlMwNRXvH9OVTqSGC4PXjw2zOi2GLUfags1decu7gcGjidlELR/WHU/6lrztfdViQ==-----END CERTIFICATE-----";

		var certArray = JSON.parse("[\""+cert+"\",\""+cert2+"\",\""+cert3+"\"]");
		
		for (i = 0; i < certArray.length; i++) { 
			var c = new X509();
			var certif = certArray[i];
			c.readCertPEM(certif);
			var certB64 = certif.substring(certif.lastIndexOf(certStartWith)+certStartWith.length,certif.indexOf(certEndWith))
			c.base64 = certB64;
			res.push(c);
		}	
		return res;
		
	},
	showCertificates = function(){
		var certArray = getCertificates();
		
		for (i = 0; i < certArray.length; i++) { 
			var hSerial    = certArray[i].getSerialNumberHex(); // '009e755e" hexadecimal string
			var sIssuer    = certArray[i].getIssuerString();    // '/C=US/O=z2'
			var sSubject   = certArray[i].getSubjectString();   // '/C=US/O=z2'
			var sNotBefore = certArray[i].getNotBefore();       // '100513235959Z'
			var sNotAfter  = certArray[i].getNotAfter();        // '200513235959Z'
			
			var desdeDate = toLocalDate(sNotBefore);
			var hastaDate = toLocalDate(sNotAfter);
			
			var element = '<li class="ui-widget-content">'+
			'					<div style="display: table;width: 100%;padding-top: 0.5em;padding-bottom: 0.5em;">'+
			'						<div style="display: table-cell;vertical-align: middle;width: 10%;">'+
			'							<img style="width:60px;height:auto;margin-left:1em;margin-right:1em;" alt="" src="'+imgSelector(desdeDate, hastaDate)+'">'+
			'						</div>'+
			'						<div style="height: 100%;display: table-cell;text-align: left;margin: 0 auto;margin-top: 2%;vertical-align: middle;margin-right: 1em;">'+
			'							<div>'+
			'								<h3 style="margin:0">NOMBRE '+getCN(sIssuer)+'</h3>'+
			'								<span>Emisor: '+getCN(sIssuer)+'</span>'+
			'								<br>'+
			'								<span>Valido desde: '+desdeDate.getDate() + '/' + (desdeDate.getMonth() + 1) + '/' +  desdeDate.getFullYear()+'</span>'+
			'								<span>hasta '+ hastaDate.getDate() + '/' + (hastaDate.getMonth() + 1) + '/' +  hastaDate.getFullYear()+'</span>'+
			'								<br>'+
			'								<a href="http://www.google.com" >Haga clic aqu&iacute; para ver las propiedades del certificado</a>'+
			'							</div>'+
			'						</div>'+
			'				</li>';
			var elementAdd = $(element).data("cert", certArray[i]);
			$("#selectable").append(elementAdd);
		}
		
		
	},
	getRDNvalueFromLdapName = function(rdn, princ){
		var principal = princ.replace(/\//gi,",");
		var offset1 = 0;
		offset1 = principal.toLowerCase().indexOf(rdn.toLowerCase(), offset1);
        while ( (offset1 = principal.toLowerCase().indexOf(rdn.toLowerCase(), offset1)) != -1) {
        	offset1 = principal.toLowerCase().indexOf(rdn.toLowerCase(), offset1);

            if (offset1 > 0 && principal.charAt(offset1-1) != ',' && principal.charAt(offset1-1) != ' ') {
                offset1++;
                continue;
            }

            offset1 += rdn.length;
            while (offset1 < principal.length && principal.charAt(offset1) == ' ') {
                offset1++;
            }

            if (offset1 >= principal.length) {
                return null;
            }

            if (principal.charAt(offset1) != '=') {
                continue;
            }

            offset1++;
            while (offset1 < principal.length && principal.charAt(offset1) == ' ') {
                offset1++;
            }

            if (offset1 >= principal.length) {
                return ""; //$NON-NLS-1$
            }

            var offset2;
            if (principal.charAt(offset1) == ',') {
                return ""; //$NON-NLS-1$
            }
            else if (principal.charAt(offset1) == '"') {
                offset1++;
                if (offset1 >= principal.length) {
                    return ""; //$NON-NLS-1$
                }

                offset2 = principal.indexOf('"', offset1);
                if (offset2 == offset1) {
                    return ""; //$NON-NLS-1$
                }
                else if (offset2 != -1) {
                    return principal.substring(offset1, offset2);
                }
                else {
                    return principal.substring(offset1);
                }
            }
            else {
            	if(typeof String.prototype.trim !== 'function') {
          		  String.prototype.trim = function() {
          		    return this.replace(/^\s+|\s+$/g, ''); 
          		  }
          		}
                offset2 = principal.indexOf(',', offset1);
                if (offset2 != -1) {
                    return principal.substring(offset1, offset2).trim();
                }
                return principal.substring(offset1).trim();
            }
        }

        return null;
	},
	getCN = function(principal){
		
		if (principal == null) {
            return null;
        }

        var rdn = getRDNvalueFromLdapName("cn", principal); //$NON-NLS-1$
        if (rdn == null) {
            rdn = getRDNvalueFromLdapName("ou", principal); //$NON-NLS-1$
        }

        if (rdn != null) {
            return rdn;
        }

        var i = principal.indexOf('=');
        if (i != -1) {
            console.warn("No se ha podido obtener el Common Name ni la Organizational Unit, se devolvera el fragmento mas significativo"); //$NON-NLS-1$
      
            return getRDNvalueFromLdapName(principal.substring(0, i).replace("/",""), principal);
        }
        console.warn("Principal no valido, se devolvera la entrada"); //$NON-NLS-1$
        return principal;
	},
	toLocalDate = function(zStr) {
		var year = zStr.substring(0,2);
		var month = zStr.substring(2,4);
		var date = zStr.substring(4,6);
		var hour = zStr.substring(6,8);
		var min = zStr.substring(8,10);
		var sec = zStr.substring(10,12);
		
		if(year < 70){
			year = 2000+Number(year);
		}
		
		// convert to date
		var gmt = new Date(year, month-1, date, hour, min, sec);
		// get offset for local timezone in minutes
		var offset = (new Date()).getTimezoneOffset();
		// get the local date
		return new Date(gmt.getTime() - offset * 60000);
	},
	imgSelector =  function(desde, hasta){
		var today = new Date();
		var res = "resources/dnieicon.png";
		if(today < desde){
			res = "resources/dnieicon_e.png";
		}else if(today > hasta){
			res = "resources/dnieicon_e.png";
		}else if(today.setMonth((today.getMonth() + 1) % 12) > hasta){
			res = "resources/dnieicon_w.png";
		}
		return res;
	};
	
	return{
		showCertificates : showCertificates,
		getCertificate : getCertificate,
		init : init
	}	
	
})();