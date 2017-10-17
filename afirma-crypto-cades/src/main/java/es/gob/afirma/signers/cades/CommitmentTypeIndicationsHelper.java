/* Copyright (C) 2011 [Gobierno de Espana]
 * This file is part of "Cliente @Firma".
 * "Cliente @Firma" is free software; you can redistribute it and/or modify it under the terms of:
 *   - the GNU General Public License as published by the Free Software Foundation;
 *     either version 2 of the License, or (at your option) any later version.
 *   - or The European Software License; either version 1.1 or (at your option) any later version.
 * You may contact the copyright holder at: soporte.afirma@seap.minhap.es
 */

package es.gob.afirma.signers.cades;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.spongycastle.asn1.ASN1ObjectIdentifier;
import org.spongycastle.asn1.DERSequence;
import org.spongycastle.asn1.esf.CommitmentTypeIdentifier;
import org.spongycastle.asn1.esf.CommitmentTypeIndication;

/** Clase de utilidad para la gesti&oacute;n de los <i>CommitmentTypeIndication</i> de CAdES.
 * @author Tom&aacute;s Garc&iacute;a-Mer&aacute;s */
public final class CommitmentTypeIndicationsHelper {

	private static final ASN1ObjectIdentifier COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_ORIGIN = CommitmentTypeIdentifier.proofOfOrigin;
	private static final ASN1ObjectIdentifier COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_RECEIPT = CommitmentTypeIdentifier.proofOfReceipt;
	private static final ASN1ObjectIdentifier COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_DELIVERY = CommitmentTypeIdentifier.proofOfDelivery;
	private static final ASN1ObjectIdentifier COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_SENDER = CommitmentTypeIdentifier.proofOfSender;
	private static final ASN1ObjectIdentifier COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_APPROVAL = CommitmentTypeIdentifier.proofOfApproval;
	private static final ASN1ObjectIdentifier COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_CREATION = CommitmentTypeIdentifier.proofOfCreation;
	private static final Map<String, ASN1ObjectIdentifier> COMMITMENT_TYPE_IDENTIFIERS = new HashMap<>(6);
	static {
		COMMITMENT_TYPE_IDENTIFIERS.put("1", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_ORIGIN); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("2", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_RECEIPT); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("3", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_DELIVERY); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("4", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_SENDER); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("5", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_APPROVAL); //$NON-NLS-1$
		COMMITMENT_TYPE_IDENTIFIERS.put("6", COMMITMENT_TYPE_IDENTIFIER_PROOF_OF_CREATION); //$NON-NLS-1$
	}

	private static final Logger LOGGER = Logger.getLogger("es.gob.afirma");	//$NON-NLS-1$

	private CommitmentTypeIndicationsHelper() {
		// No permitimos la instanciacion
	}

    /** Genera la lista de <i>CommitmentTypeIndication</i> a incluir en la firma a partir de los par&aacute;metros
     * adicionales.
     * <pre>
     * CommitmentTypeIndication ::= SEQUENCE {
     *   commitmentTypeId           CommitmentTypeIdentifier,
     *   commitmentTypeQualifier    SEQUENCE SIZE (1..MAX) OF CommitmentTypeQualifier OPTIONAL
     * }
     *
     * CommitmentTypeIdentifier ::= OBJECT IDENTIFIER
     *
     * CommitmentTypeQualifier ::= SEQUENCE {
     *   commitmentTypeIdentifier   CommitmentTypeIdentifier,
     *   qualifier                  ANY DEFINED BY commitmentTypeIdentifier
     * }
     * </pre>
     * @param xParams Par&aacute;metros adicionales con la informaci&oacute;n de los CommitmentTypeIndication a incluir
     * @return Lista de <i>CommitmentTypeIndication</i> a incluir en la firma CAdES */
	public static List<CommitmentTypeIndicationBean> getCommitmentTypeIndications(final Properties xParams) {

    	final List<CommitmentTypeIndicationBean> ret = new ArrayList<>();

		if (xParams == null) {
			return ret;
		}

		String tmpStr = xParams.getProperty(CAdESExtraParams.COMMITMENT_TYPE_INDICATIONS);
		if (tmpStr == null) {
			return ret;
		}

		final int nCtis;
		try {
			nCtis = Integer.parseInt(tmpStr);
			if (nCtis < 1) {
				throw new NumberFormatException();
			}
		}
		catch(final Exception e) {
			LOGGER.severe(
				"El parametro adicional 'CommitmentTypeIndications' debe contener un valor numerico entero (el valor actual es " + tmpStr + "), no se anadira el CommitmentTypeIndication: " + e //$NON-NLS-1$ //$NON-NLS-2$
			);
			return ret;
		}

		String identifier;
		List<String> commitmentTypeQualifiers;

		for(int i = 0; i <= nCtis; i++) {

			// Identifier
			tmpStr = xParams.getProperty(CAdESExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + Integer.toString(i) + CAdESExtraParams.COMMITMENT_TYPE_INDICATION_IDENTIFIER);
			if (tmpStr == null) {
				continue;
			}
			final ASN1ObjectIdentifier ident = COMMITMENT_TYPE_IDENTIFIERS.get(tmpStr);
			if (ident == null)  {
				LOGGER.severe(
					"El identificador del CommitmentTypeIndication " + i + " no es un tipo soportado (" + tmpStr + "), se omitira y se continuara con el siguiente" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				);
				continue;
			}
			identifier = ident.toString();

			// Qualifiers
			tmpStr = xParams.getProperty(CAdESExtraParams.COMMITMENT_TYPE_INDICATION_PREFIX + Integer.toString(i) + CAdESExtraParams.COMMITMENT_TYPE_INDICATION_QUALIFIERS);
			if (tmpStr == null) {
				commitmentTypeQualifiers = null;
			}
			else {
				commitmentTypeQualifiers = new ArrayList<>();
				final String[] ctqs = tmpStr.split(Pattern.quote("|")); //$NON-NLS-1$
				for (final String ctq : ctqs) {
					commitmentTypeQualifiers.add(ctq);
				}
			}

			ret.add(new CommitmentTypeIndicationBean(identifier, commitmentTypeQualifiers));

		}

    	return ret;
	}


    static CommitmentTypeIndication generateCommitmentTypeIndication(final CommitmentTypeIndicationBean ctib) {

    	if (ctib == null) {
    		throw new IllegalArgumentException(
				"El CommitmentTypeIndicationBean no puede ser nulo" //$NON-NLS-1$
			);
    	}

    	final ASN1ObjectIdentifier id;
    	try {
    		id = new ASN1ObjectIdentifier(ctib.getIdentifier());
    	}
    	catch(final Exception e) {
    		throw new IllegalArgumentException(
				"El identificador del CommitmentTypeIndication debe ser un OID valido: " + e, e //$NON-NLS-1$
			);
    	}

    	if (ctib.getQualifiers() == null || ctib.getQualifiers().size() < 1) {
    		return new CommitmentTypeIndication(id);
    	}

    	final String[] strQuals = ctib.getQualifiers().toArray(new String[0]);
    	final ASN1ObjectIdentifier[] qualifiers = new  ASN1ObjectIdentifier[strQuals.length];
    	for (int i=0; i<strQuals.length; i++) {
    		try {
    			qualifiers[i] = new ASN1ObjectIdentifier(strQuals[i]);
    		}
    		catch(final Exception e) {
    			throw new IllegalArgumentException(
					"El calificador proporcionado no es un OID (" + strQuals[i] + "): " + e, e //$NON-NLS-1$ //$NON-NLS-2$
				);
    		}
    	}

    	return new CommitmentTypeIndication(
			id,
			new DERSequence(qualifiers)
		);

    }

}
