/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.keystores.filters.rfc;

import java.io.IOException;
import java.security.cert.X509Certificate;
import java.util.List;

import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.ASN1Primitive;
import org.spongycastle.asn1.ASN1Sequence;
import org.spongycastle.asn1.DEROctetString;
import org.spongycastle.asn1.x509.qualified.QCStatement;

import es.gob.afirma.keystores.CertificateFilter;

/** Filtro de certificados que limita el uso &uacute;nicamente a los certificados generados en
 * un SSCD.
 * Se usa como referencia el atributo QcSSCD, tal y como se indica en el Anexo II de los
 * <i>Perfiles de certificados electr&oacute;nicos</i>, aprobado por el <i>Consejo Superior de
 * Administraci&oacute;n Electr&oacute;nica</i>, en reuni&oacute;n de la Comisi&oacute;n
 * Permanente de 30 de mayo de 2012.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class SscdFilter extends CertificateFilter {

	private static final String QC_STATEMENTS_OID = "1.3.6.1.5.5.7.1.3"; //$NON-NLS-1$
	private static final ASN1ObjectIdentifier QC_SSCD_OID = new ASN1ObjectIdentifier("0.4.0.1862.1.4"); //$NON-NLS-1$

	@Override
	public boolean matches(final X509Certificate cert) {

		if (cert == null) {
			return false;
		}

		final byte [] qcStatementsValue = cert.getExtensionValue(QC_STATEMENTS_OID);
		if (qcStatementsValue == null) {
			return false;
		}

		final QCStatements qcs;
		try {
			final DEROctetString qcsRaw = (DEROctetString) ASN1Primitive.fromByteArray(qcStatementsValue);
			final ASN1Sequence seq  = (ASN1Sequence) ASN1Primitive.fromByteArray(
				qcsRaw.getOctets()
			);
			qcs = QCStatements.getInstance(seq);
		}
		catch (final IOException e) {
			return false;
		}

		final List<QCStatement> qcss = qcs.getQCStatement();

		for (final QCStatement qc : qcss) {
			if (QC_SSCD_OID.equals(qc.getStatementId())) {
				return true;
			}
		}
		return false;
	}
}
