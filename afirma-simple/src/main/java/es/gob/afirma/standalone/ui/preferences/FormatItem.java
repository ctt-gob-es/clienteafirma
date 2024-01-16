package es.gob.afirma.standalone.ui.preferences;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

/** Formato de firma. */
public final class FormatItem {

	private final String name;
	private boolean isRecommended;

	public FormatItem(final String name) {
		this.name = name;
		this.isRecommended = false;
	}

	@Override
	public String toString() {
		if (this.isRecommended) {
			return this.name + " " + SimpleAfirmaMessages.getString("ChangeFormatDialog.2");  //$NON-NLS-1$//$NON-NLS-2$
		}
		return this.name;
	}
	
	public boolean isRecommended() {
		return this.isRecommended;
	}
	
	public void setRecommended(final boolean recommended) {
		this.isRecommended = recommended;
	}

	public String getName() {
		return this.name;
	}
}
