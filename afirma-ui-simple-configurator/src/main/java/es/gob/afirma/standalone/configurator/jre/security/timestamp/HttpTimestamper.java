/*
 * Copyright (c) 2003, 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package es.gob.afirma.standalone.configurator.jre.security.timestamp;

import java.io.BufferedInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.Set;

import es.gob.afirma.standalone.configurator.jre.misc.IOUtils;
import es.gob.afirma.standalone.configurator.jre.security.util.Debug;

/**
 * A timestamper that communicates with a Timestamping Authority (TSA)
 * over HTTP.
 * It supports the Time-Stamp Protocol defined in:
 * <a href="http://www.ietf.org/rfc/rfc3161.txt">RFC 3161</a>.
 *
 * @since 1.5
 * @author Vincent Ryan
 */

public class HttpTimestamper implements Timestamper {

    private static final int CONNECT_TIMEOUT = 15000; // 15 seconds

    // The MIME type for a timestamp query
    private static final String TS_QUERY_MIME_TYPE =
        "application/timestamp-query"; //$NON-NLS-1$

    // The MIME type for a timestamp reply
    private static final String TS_REPLY_MIME_TYPE =
        "application/timestamp-reply"; //$NON-NLS-1$

    private static final Debug debug = Debug.getInstance("ts"); //$NON-NLS-1$

    /*
     * HTTP URI identifying the location of the TSA
     */
    private URI tsaURI = null;

    /**
     * Creates a timestamper that connects to the specified TSA.
     *
     * @param tsa The location of the TSA. It must be an HTTP or HTTPS URI.
     * @throws IllegalArgumentException if tsaURI is not an HTTP or HTTPS URI
     */
    public HttpTimestamper(final URI tsaURI) {
        if (!tsaURI.getScheme().equalsIgnoreCase("http") && //$NON-NLS-1$
                !tsaURI.getScheme().equalsIgnoreCase("https")) { //$NON-NLS-1$
            throw new IllegalArgumentException(
                    "TSA must be an HTTP or HTTPS URI"); //$NON-NLS-1$
        }
        this.tsaURI = tsaURI;
    }

    /**
     * Connects to the TSA and requests a timestamp.
     *
     * @param tsQuery The timestamp query.
     * @return The result of the timestamp query.
     * @throws IOException The exception is thrown if a problem occurs while
     *         communicating with the TSA.
     */
    @Override
	public TSResponse generateTimestamp(final TSRequest tsQuery) throws IOException {

        final HttpURLConnection connection =
            (HttpURLConnection) this.tsaURI.toURL().openConnection();
        connection.setDoOutput(true);
        connection.setUseCaches(false); // ignore cache
        connection.setRequestProperty("Content-Type", TS_QUERY_MIME_TYPE); //$NON-NLS-1$
        connection.setRequestMethod("POST"); //$NON-NLS-1$
        // Avoids the "hang" when a proxy is required but none has been set.
        connection.setConnectTimeout(CONNECT_TIMEOUT);

        if (debug != null) {
            final Set<Map.Entry<String, List<String>>> headers =
                connection.getRequestProperties().entrySet();
            debug.println(connection.getRequestMethod() + " " + this.tsaURI + //$NON-NLS-1$
                " HTTP/1.1"); //$NON-NLS-1$
            for (final Map.Entry<String, List<String>> e : headers) {
                debug.println("  " + e); //$NON-NLS-1$
            }
            debug.println();
        }
        connection.connect(); // No HTTP authentication is performed

        // Send the request
        DataOutputStream output = null;
        try {
            output = new DataOutputStream(connection.getOutputStream());
            final byte[] request = tsQuery.encode();
            output.write(request, 0, request.length);
            output.flush();
            if (debug != null) {
                debug.println("sent timestamp query (length=" + //$NON-NLS-1$
                        request.length + ")"); //$NON-NLS-1$
            }
        } finally {
            if (output != null) {
                output.close();
            }
        }

        // Receive the reply
        BufferedInputStream input = null;
        byte[] replyBuffer = null;
        try {
            input = new BufferedInputStream(connection.getInputStream());
            if (debug != null) {
                String header = connection.getHeaderField(0);
                debug.println(header);
                int i = 1;
                while ((header = connection.getHeaderField(i)) != null) {
                    final String key = connection.getHeaderFieldKey(i);
                    debug.println("  " + ((key==null) ? "" : key + ": ") + //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                        header);
                    i++;
                }
                debug.println();
            }
            verifyMimeType(connection.getContentType());

            final int contentLength = connection.getContentLength();
            replyBuffer = IOUtils.readFully(input, contentLength, false);

            if (debug != null) {
                debug.println("received timestamp response (length=" + //$NON-NLS-1$
                        replyBuffer.length + ")"); //$NON-NLS-1$
            }
        } finally {
            if (input != null) {
                input.close();
            }
        }
        return new TSResponse(replyBuffer);
    }

    /*
     * Checks that the MIME content type is a timestamp reply.
     *
     * @param contentType The MIME content type to be checked.
     * @throws IOException The exception is thrown if a mismatch occurs.
     */
    private static void verifyMimeType(final String contentType) throws IOException {
        if (! TS_REPLY_MIME_TYPE.equalsIgnoreCase(contentType)) {
            throw new IOException("MIME Content-Type is not " + //$NON-NLS-1$
                TS_REPLY_MIME_TYPE);
        }
    }
}
