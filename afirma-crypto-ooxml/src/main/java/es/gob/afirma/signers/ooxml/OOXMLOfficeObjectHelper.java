/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.ooxml;

import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.util.LinkedList;
import java.util.List;

import javax.xml.crypto.XMLStructure;
import javax.xml.crypto.dom.DOMStructure;
import javax.xml.crypto.dsig.SignatureProperties;
import javax.xml.crypto.dsig.SignatureProperty;
import javax.xml.crypto.dsig.XMLObject;
import javax.xml.crypto.dsig.XMLSignatureFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import es.gob.afirma.core.misc.Platform;

final class OOXMLOfficeObjectHelper {

	private static final String MS_DIGITAL_SIGNATURE_SCHEMA = "http://schemas.microsoft.com/office/2006/digsig"; //$NON-NLS-1$
	private static final String NAMESPACE_SPEC_NS = "http://www.w3.org/2000/xmlns/"; //$NON-NLS-1$

	private OOXMLOfficeObjectHelper() {
		// No permitimos la instanciacion
	}

	static XMLObject getOfficeObject(final String nodeId,
			                         final XMLSignatureFactory fac,
			                         final Document document,
			                         final String signatureId,
			                         final String signatureComments,
			                         final String address1,
			                         final String address2,
			                         final String sigType) {

        final List<XMLStructure> objectContent = new LinkedList<>();

        //************************************************************************************
        //************************************************************************************
        //********************** SIGNATURE INFO V1 *******************************************

        final Element signatureInfoV1Element = document.createElementNS(MS_DIGITAL_SIGNATURE_SCHEMA, "SignatureInfoV1"); //$NON-NLS-1$
        signatureInfoV1Element.setAttributeNS(
    		NAMESPACE_SPEC_NS,
    		"xmlns", //$NON-NLS-1$
    		MS_DIGITAL_SIGNATURE_SCHEMA
		);

        // ******************************************************************************
        // *********************** Metadatos vacios *************************************
        signatureInfoV1Element.appendChild(
    		document.createElementNS(
        		MS_DIGITAL_SIGNATURE_SCHEMA,
	    		"SetupID" //$NON-NLS-1$
			)
		);
        signatureInfoV1Element.appendChild(
    		document.createElementNS(
        		MS_DIGITAL_SIGNATURE_SCHEMA,
	    		"SignatureText" //$NON-NLS-1$
			)
		);
        signatureInfoV1Element.appendChild(
    		document.createElementNS(
        		MS_DIGITAL_SIGNATURE_SCHEMA,
	    		"SignatureImage" //$NON-NLS-1$
			)
		);
        // ********************** Fin Metadatos vacios **********************************
        // ******************************************************************************

        // ******************************************************************************
        // **************** Metadatos adicionales V1 ************************************

        if (signatureComments != null) {
	        final Element signatureCommentsElement = document.createElementNS(
        		MS_DIGITAL_SIGNATURE_SCHEMA,
	    		"SignatureComments" //$NON-NLS-1$
			);
	        signatureCommentsElement.setTextContent(signatureComments);
	        signatureInfoV1Element.appendChild(signatureCommentsElement);
        }

        if (Platform.OS.WINDOWS.equals(Platform.getOS())) {
        	final Element windowsVersionElement = document.createElementNS(
    			MS_DIGITAL_SIGNATURE_SCHEMA,
    			"WindowsVersion" //$NON-NLS-1$
			);
        	windowsVersionElement.setTextContent(System.getProperty("os.version")); //$NON-NLS-1$
        	signatureInfoV1Element.appendChild(windowsVersionElement);
        }

        // Indicamos firma generada con Office 16
        final Element officeVersionElement = document.createElementNS(
			MS_DIGITAL_SIGNATURE_SCHEMA,
			"OfficeVersion" //$NON-NLS-1$
		);
    	officeVersionElement.setTextContent("16.0"); //$NON-NLS-1$
    	signatureInfoV1Element.appendChild(officeVersionElement);
    	final Element applicationVersionElement = document.createElementNS(
			MS_DIGITAL_SIGNATURE_SCHEMA,
			"ApplicationVersion" //$NON-NLS-1$
		);
    	applicationVersionElement.setTextContent("16.0"); //$NON-NLS-1$
    	signatureInfoV1Element.appendChild(applicationVersionElement);

        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();

        final Element monitorsElement = document.createElementNS(
    		MS_DIGITAL_SIGNATURE_SCHEMA,
    		"Monitors" //$NON-NLS-1$
		);
        monitorsElement.setTextContent(
    		Integer.toString(
				ge.getScreenDevices().length
			)
		);
        signatureInfoV1Element.appendChild(monitorsElement);

        final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        final Element horizontalResolutionElement = document.createElementNS(
    		MS_DIGITAL_SIGNATURE_SCHEMA,
    		"HorizontalResolutionElement" //$NON-NLS-1$
		);
        horizontalResolutionElement.setTextContent(Integer.toString(screenSize.width));
        signatureInfoV1Element.appendChild(horizontalResolutionElement);

        final Element verticalResolutionElement = document.createElementNS(
    		MS_DIGITAL_SIGNATURE_SCHEMA,
    		"VerticalResolutionElement" //$NON-NLS-1$
		);
        verticalResolutionElement.setTextContent(Integer.toString(screenSize.height));
        signatureInfoV1Element.appendChild(verticalResolutionElement);

        final Element colorDepthElement = document.createElementNS(
    		MS_DIGITAL_SIGNATURE_SCHEMA,
        	"ColorDepth" //$NON-NLS-1$
		);
        colorDepthElement.setTextContent(
    		Integer.toString(
				ge.getScreenDevices()[0].getDisplayMode().getBitDepth()
			)
		);
        signatureInfoV1Element.appendChild(colorDepthElement);

        // Proveedor de firma por defecto

        final Element signatureProviderId = document.createElementNS(
			MS_DIGITAL_SIGNATURE_SCHEMA,
			"SignatureProviderId" //$NON-NLS-1$
		);
        signatureProviderId.setTextContent("{00000000-0000-0000-0000-000000000000}"); //$NON-NLS-1$
    	signatureInfoV1Element.appendChild(signatureProviderId);

    	signatureInfoV1Element.appendChild(
			document.createElementNS(
				MS_DIGITAL_SIGNATURE_SCHEMA,
				"SignatureProviderUrl" //$NON-NLS-1$
			)
		);

    	final Element signatureProviderDetails = document.createElementNS(
			MS_DIGITAL_SIGNATURE_SCHEMA,
			"SignatureProviderDetails" //$NON-NLS-1$
		);
    	signatureProviderDetails.setTextContent("9"); //$NON-NLS-1$
    	signatureInfoV1Element.appendChild(signatureProviderDetails);

    	if (sigType != null && !sigType.isEmpty()) {
	    	final Element signatureType = document.createElementNS(
				MS_DIGITAL_SIGNATURE_SCHEMA,
				"SignatureType" //$NON-NLS-1$
			);
	    	signatureType.setTextContent(sigType);
	    	signatureInfoV1Element.appendChild(signatureType);
    	}

        // **************** Fin Metadatos adicionales V1 ********************************
        // ******************************************************************************

        // ************** FIN SIGNATURE INFO V1 ***********************************************
        // ************************************************************************************
        // ************************************************************************************

        // ************************************************************************************
        // ************************************************************************************
        // ************** SIGNATURE INFO V2 ***************************************************

        final Element signatureInfoV2Element = document.createElementNS(MS_DIGITAL_SIGNATURE_SCHEMA, "SignatureInfoV2"); //$NON-NLS-1$
        signatureInfoV2Element.setAttributeNS(NAMESPACE_SPEC_NS, "xmlns", MS_DIGITAL_SIGNATURE_SCHEMA); //$NON-NLS-1$

        if (address1 != null) {
	        final Element address1Element = document.createElementNS(
        		MS_DIGITAL_SIGNATURE_SCHEMA,
	    		"Address1" //$NON-NLS-1$
			);
	        address1Element.setTextContent(address1);
	        signatureInfoV2Element.appendChild(address1Element);
        }

        if (address2 != null) {
	        final Element address2Element = document.createElementNS(
        		MS_DIGITAL_SIGNATURE_SCHEMA,
	    		"Address2" //$NON-NLS-1$
			);
	        address2Element.setTextContent(address2);
	        signatureInfoV2Element.appendChild(address2Element);
        }

        // ************** FIN SIGNATURE INFO V2 ***********************************************
        // ************************************************************************************
        // ************************************************************************************

        // El nodo idOfficeV1Details agrupa tanto a SignatureInfoV1 como a SignatureInfoV2

        final List<XMLStructure> signatureInfoContent = new LinkedList<>();
        signatureInfoContent.add(new DOMStructure(signatureInfoV1Element));
        signatureInfoContent.add(new DOMStructure(signatureInfoV2Element));


        final SignatureProperty signatureInfoSignatureProperty = fac.newSignatureProperty(
    		signatureInfoContent, "#" + signatureId, "idOfficeV1Details" //$NON-NLS-1$ //$NON-NLS-2$
		);

        final List<SignatureProperty> signaturePropertyContent = new LinkedList<>();
        signaturePropertyContent.add(signatureInfoSignatureProperty);
        final SignatureProperties signatureProperties = fac.newSignatureProperties(signaturePropertyContent, null);
        objectContent.add(signatureProperties);

        return fac.newXMLObject(objectContent, nodeId, null, null);

	}

}
