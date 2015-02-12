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
 * Copyright (C) 2008-2009 FedICT.
 * This file is part of the eID Applet Project.
 *
 * Licensed under the Apache License, Version 2.0 (the License).
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

package es.gob.afirma.signers.ooxml;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.crypto.Data;
import javax.xml.crypto.OctetStreamData;
import javax.xml.crypto.URIDereferencer;
import javax.xml.crypto.URIReference;
import javax.xml.crypto.URIReferenceException;
import javax.xml.crypto.XMLCryptoContext;
import javax.xml.crypto.dsig.XMLSignatureFactory;

import es.gob.afirma.core.misc.AOUtil;

/** Resuelve referencias dentro del Zip de un documento OOXML. */
public final class OOXMLURIDereferencer implements URIDereferencer {

    private final byte[] ooxml;

    private final URIDereferencer baseUriDereferencer;

    OOXMLURIDereferencer(final byte[] ooxml) {
        if (null == ooxml) {
            throw new IllegalArgumentException("El OOXML es nulo"); //$NON-NLS-1$
        }
        this.baseUriDereferencer = XMLSignatureFactory.getInstance().getURIDereferencer();
        this.ooxml = ooxml.clone();
    }

    @Override
	public Data dereference(final URIReference uriReference, final XMLCryptoContext context) throws URIReferenceException {

        if (null == uriReference) {
            throw new IllegalArgumentException("La referencia no puede ser nula"); //$NON-NLS-1$
        }
        if (null == context) {
            throw new IllegalArgumentException("El contexto de firma no puede ser nulo"); //$NON-NLS-1$
        }

        String uri = uriReference.getURI();
        try {
            uri = URLDecoder.decode(uri, "UTF-8"); //$NON-NLS-1$
        }
        catch (final UnsupportedEncodingException e) {
            Logger.getLogger("es.gob.afirma").warning("No se puede decodificar la URI '" + uri + "': " + e); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }

        try {
            final InputStream dataInputStream = findDataInputStream(uri);
            if (null == dataInputStream) {
                return this.baseUriDereferencer.dereference(uriReference, context);
            }
            final byte[] data = AOUtil.getDataFromInputStream(dataInputStream);
            dataInputStream.close();
            return new OctetStreamData(new ByteArrayInputStream(data), uri, null);
        }
        catch (final IOException e) {
            throw new URIReferenceException("Error de I/O: " + e, e); //$NON-NLS-1$
        }
    }

    private InputStream findDataInputStream(final String uri) throws IOException {
        String entryName;
        if (uri.startsWith("/")) { //$NON-NLS-1$
            entryName = uri.substring(1); // remove '/'
        }
        else {
            entryName = uri;
        }
        if (-1 != entryName.indexOf('?')) {
            entryName = entryName.substring(0, entryName.indexOf('?'));
        }

        final ZipInputStream ooxmlZipInputStream = new ZipInputStream(new ByteArrayInputStream(this.ooxml));
        ZipEntry zipEntry;
        while (null != (zipEntry = ooxmlZipInputStream.getNextEntry())) {
            if (zipEntry.getName().equals(entryName)) {
                return ooxmlZipInputStream;
            }
        }
        return null;
    }
}
