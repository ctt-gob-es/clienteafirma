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

package es.gob.afirma.be.fedict.eid.applet.service.signer;

import java.io.InputStream;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.util.List;

import es.gob.afirma.be.fedict.eid.applet.service.signer.ooxml.AbstractOOXMLSignatureService;
import es.gob.afirma.be.fedict.eid.applet.service.signer.ooxml.OOXMLProvider;
import es.gob.afirma.core.misc.AOUtil;

/** Contenedor para la implementaci&oacute;n abstracta del servicio de firma OOXML. */
public final class AbstractOOXMLSignatureServiceContainer {

    private static final class OOXMLSignatureService extends AbstractOOXMLSignatureService {

        @Override
        protected String getSignatureDigestAlgorithm() {
            return this.digestAlgorithm;
        }

        private final byte[] ooxml;

        private final String digestAlgorithm;

        public OOXMLSignatureService(final InputStream ooxmlis, final String digestAlgo) {
            try {
                this.ooxml = AOUtil.getDataFromInputStream(ooxmlis);
            }
            catch (final Exception e) {
                throw new IllegalArgumentException("No se ha podido leer el OOXML desde el flujo de entrada"); //$NON-NLS-1$
            }
            if (digestAlgo == null) {
                this.digestAlgorithm = "SHA1"; //$NON-NLS-1$
            }
            else {
                this.digestAlgorithm = digestAlgo;
            }
        }

        @Override
        protected byte[] getOfficeOpenXMLDocument() {
            return this.ooxml;
        }

    }

    /** Firma digitalmente un documento OOXML.
     * @param ooxml Documento OOXML
     * @param certChain Certificado firmante con su cadena de confianza
     * @param digestAlgorithm Algoritmo de huella digital
     * @param pk Clave privada
     * @param signerCount N&uacute;mero de firma
     * @return Documento OOXML firmado
     * @throws Exception Cuando ocurre cualquier problema durante el proceso */
    public byte[] sign(final InputStream ooxml,
                             final List<X509Certificate> certChain,
                             final String digestAlgorithm,
                             final PrivateKey pk,
                             final int signerCount) throws Exception {

        OOXMLProvider.install();

        final OOXMLSignatureService signatureService = new OOXMLSignatureService(ooxml, digestAlgorithm);

        return signatureService.outputSignedOfficeOpenXMLDocument(signatureService.preSign(null, certChain, pk));

    }

}
