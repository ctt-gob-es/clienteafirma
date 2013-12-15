/*
 * eID Applet Project.
 * Copyright (C) 2009 FedICT.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version
 * 3.0 as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, see
 * http://www.gnu.org/licenses/.
 */

/*
 * Copyright (C) 2009 FedICT.
 * This file is part of the eID Applet Project.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer;

import java.io.IOException;
import java.security.InvalidAlgorithmParameterException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;

import javax.xml.crypto.MarshalException;
import javax.xml.crypto.dsig.XMLSignatureException;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.xml.sax.SAXException;

import es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer.ooxml.OOXMLSignatureService;
import es.gob.afirma.signers.ooxml.be.fedict.eid.applet.service.signer.ooxml.OOXMLProvider;

/** Contenedor para la implementaci&oacute;n del servicio de firma OOXML. */
public final class OOXMLSignatureServiceContainer {

	private OOXMLSignatureServiceContainer() {
		// No permitimos la instanciacion
	}

    /** Firma digitalmente un documento OOXML.
     * @param ooxml Documento OOXML
     * @param certChain Certificado firmante con su cadena de confianza
     * @param digestAlgorithm Algoritmo de huella digital
     * @param pk Clave privada
     * @param signerCount N&uacute;mero de firma
     * @param signatureComments Raz&oacute;n de la firma
     * @param address1 Direcci&oacute;n donde se ha realizado la firma (campo 1)
     * @param address2 Direcci&oacute;n donde se ha realizado la firma (campo 2)
     * @return Documento OOXML firmado
     * @throws XMLSignatureException
     * @throws MarshalException
     * @throws TransformerException
     * @throws SAXException
     * @throws ParserConfigurationException
     * @throws IOException
     * @throws InvalidAlgorithmParameterException
     * @throws NoSuchAlgorithmException */
    public static byte[] sign(final byte[] ooxml,
                              final X509Certificate[] certChain,
                              final PrivateKey pk,
                              final int signerCount,
                              final String signatureComments,
                              final String address1,
                              final String address2) throws NoSuchAlgorithmException,
                                                            InvalidAlgorithmParameterException,
                                                            IOException,
                                                            ParserConfigurationException,
                                                            SAXException,
                                                            TransformerException,
                                                            MarshalException,
                                                            XMLSignatureException {
        OOXMLProvider.install();

        final OOXMLSignatureService signatureService = new OOXMLSignatureService();

        return signatureService.outputSignedOfficeOpenXMLDocument(
    		ooxml,
    		signatureService.preSign(
				ooxml,
				certChain,
				pk,
				signatureComments,
				address1,
				address2
			)
		);

    }

}
