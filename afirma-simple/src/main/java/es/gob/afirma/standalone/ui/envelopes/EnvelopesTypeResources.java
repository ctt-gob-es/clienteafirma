package es.gob.afirma.standalone.ui.envelopes;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

enum EnvelopesTypeResources {

		AUTHENTICATED(	SimpleAfirmaMessages.getString("MenuDigitalEnvelope.5"), 	0), //$NON-NLS-1$
		SIGNED(			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.6"), 	1), //$NON-NLS-1$
		SIMPLE(			SimpleAfirmaMessages.getString("MenuDigitalEnvelope.7"), 	2); //$NON-NLS-1$

		private final String envTypeName;
		private final int envTypeIndex;

		private EnvelopesTypeResources(final String name, final int index) {
			this.envTypeName = name;
			this.envTypeIndex = index;
		}

		@Override
		public String toString() {
			return this.envTypeName;
		}

		int getIndex() {
			return this.envTypeIndex;
		}

		static EnvelopesTypeResources getName(final int index) {
			switch (index) {
			case 0:
				return AUTHENTICATED;
			case 1:
				return SIGNED;
			case 2:
				return SIMPLE;
			default:
				throw new IllegalArgumentException();
			}
		}

		static EnvelopesTypeResources[] getAllEnvelopesTypeResources() {
			return new EnvelopesTypeResources[] {
				AUTHENTICATED,
				SIGNED,
				SIMPLE
			};
		}
	}