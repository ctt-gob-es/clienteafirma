package es.gob.afirma.standalone.ui.preferences;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

enum TimeStampPdfTypeResource {

	SIGN(				SimpleAfirmaMessages.getString("TimeStampPdfTypeResource.0") , 1), 	//$NON-NLS-1$
	DOCUMENT(			SimpleAfirmaMessages.getString("TimeStampPdfTypeResource.1") , 2), 	//$NON-NLS-1$
	SIGN_AND_DOCUMENT(	SimpleAfirmaMessages.getString("TimeStampPdfTypeResource.2") , 3); 	//$NON-NLS-1$


	private final String typeName;
	private final int typeIndex;

	private TimeStampPdfTypeResource(final String name, final int index) {
		this.typeName = name;
		this.typeIndex = index;
	}

	@Override
	public String toString() {
		return this.typeName;
	}

	int getIndex() {
		return this.typeIndex;
	}

	static TimeStampPdfTypeResource getName(final int index) {
		switch (index) {
		case 1:
			return SIGN;
		case 2:
			return DOCUMENT;
		case 3:
			return SIGN_AND_DOCUMENT;
		default:
			throw new IllegalArgumentException();
		}
	}

	static TimeStampPdfTypeResource[] getAllTimeStampPdfTypeResources() {
		return new TimeStampPdfTypeResource[] {
			SIGN,
			DOCUMENT,
			SIGN_AND_DOCUMENT
		};
	}
}
