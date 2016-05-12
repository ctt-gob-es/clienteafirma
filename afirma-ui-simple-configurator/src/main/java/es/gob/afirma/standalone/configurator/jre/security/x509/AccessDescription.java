/*
 * Copyright (c) 2003, 2011, Oracle and/or its affiliates. All rights reserved.
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

package es.gob.afirma.standalone.configurator.jre.security.x509;

import java.io.IOException;

import es.gob.afirma.standalone.configurator.jre.security.util.DerInputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerOutputStream;
import es.gob.afirma.standalone.configurator.jre.security.util.DerValue;
import es.gob.afirma.standalone.configurator.jre.security.util.ObjectIdentifier;

/**
 * @author      Ram Marti
 */

public final class AccessDescription {

    private int myhash = -1;

    private final ObjectIdentifier accessMethod;

    private final GeneralName accessLocation;

    public static final ObjectIdentifier Ad_OCSP_Id =
        ObjectIdentifier.newInternal(new int[] {1, 3, 6, 1, 5, 5, 7, 48, 1});

    public static final ObjectIdentifier Ad_CAISSUERS_Id =
        ObjectIdentifier.newInternal(new int[] {1, 3, 6, 1, 5, 5, 7, 48, 2});

    public static final ObjectIdentifier Ad_TIMESTAMPING_Id =
        ObjectIdentifier.newInternal(new int[] {1, 3, 6, 1, 5, 5, 7, 48, 3});

    public static final ObjectIdentifier Ad_CAREPOSITORY_Id =
        ObjectIdentifier.newInternal(new int[] {1, 3, 6, 1, 5, 5, 7, 48, 5});

    public AccessDescription(final ObjectIdentifier accessMethod, final GeneralName accessLocation) {
        this.accessMethod = accessMethod;
        this.accessLocation = accessLocation;
    }

    public AccessDescription(final DerValue derValue) throws IOException {
        final DerInputStream derIn = derValue.getData();
        this.accessMethod = derIn.getOID();
        this.accessLocation = new GeneralName(derIn.getDerValue());
    }

    public ObjectIdentifier getAccessMethod() {
        return this.accessMethod;
    }

    public GeneralName getAccessLocation() {
        return this.accessLocation;
    }

    public void encode(final DerOutputStream out) throws IOException {
        final DerOutputStream tmp = new DerOutputStream();
        tmp.putOID(this.accessMethod);
        this.accessLocation.encode(tmp);
        out.write(DerValue.tag_Sequence, tmp);
    }

    @Override
	public int hashCode() {
        if (this.myhash == -1) {
            this.myhash = this.accessMethod.hashCode() + this.accessLocation.hashCode();
        }
        return this.myhash;
    }

    @Override
	public boolean equals(final Object obj) {
        if (obj == null || (!(obj instanceof AccessDescription))) {
            return false;
        }
        final AccessDescription that = (AccessDescription)obj;

        if (this == that) {
            return true;
        }
        return (this.accessMethod.equals((Object)that.getAccessMethod()) &&
            this.accessLocation.equals(that.getAccessLocation()));
    }

    @Override
	public String toString() {
        String method = null;
        if (this.accessMethod.equals((Object)Ad_CAISSUERS_Id)) {
            method = "caIssuers";
        } else if (this.accessMethod.equals((Object)Ad_CAREPOSITORY_Id)) {
            method = "caRepository";
        } else if (this.accessMethod.equals((Object)Ad_TIMESTAMPING_Id)) {
            method = "timeStamping";
        } else if (this.accessMethod.equals((Object)Ad_OCSP_Id)) {
            method = "ocsp";
        } else {
            method = this.accessMethod.toString();
        }
        return ("\n   accessMethod: " + method +
                "\n   accessLocation: " + this.accessLocation.toString() + "\n");
    }
}
