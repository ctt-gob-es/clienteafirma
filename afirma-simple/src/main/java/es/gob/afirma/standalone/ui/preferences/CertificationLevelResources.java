package es.gob.afirma.standalone.ui.preferences;

import es.gob.afirma.standalone.SimpleAfirmaMessages;

enum CertificationLevelResources {

		ORDINARY(			SimpleAfirmaMessages.getString("CertificationLevelResources.0"), 	0), //$NON-NLS-1$
		AUTHOR(				SimpleAfirmaMessages.getString("CertificationLevelResources.1"), 	1), //$NON-NLS-1$
		AUTHOR_CERTIFICATED(SimpleAfirmaMessages.getString("CertificationLevelResources.2"), 	2), //$NON-NLS-1$
		CERTIFICATED(		SimpleAfirmaMessages.getString("CertificationLevelResources.3"), 	3), //$NON-NLS-1$
		ASK_BEFORE_SIGN(	SimpleAfirmaMessages.getString("CertificationLevelResources.4"), 	4); //$NON-NLS-1$

		private final String certLevelName;
		private final int certLevelIndex;

		private CertificationLevelResources(final String name, final int index) {
			this.certLevelName = name;
			this.certLevelIndex = index;
		}

		@Override
		public String toString() {
			return this.certLevelName;
		}

		int getIndex() {
			return this.certLevelIndex;
		}

		static CertificationLevelResources getName(final int index) {
			switch (index) {
			case 0:
				return ORDINARY;
			case 1:
				return AUTHOR;
			case 2:
				return AUTHOR_CERTIFICATED;
			case 3:
				return CERTIFICATED;
			case 4:
				return ASK_BEFORE_SIGN;
			default:
				throw new IllegalArgumentException();
			}
		}

		static CertificationLevelResources[] getAllCertificationLevelResources() {
			return new CertificationLevelResources[] {
				ORDINARY,
				AUTHOR,
				AUTHOR_CERTIFICATED,
				CERTIFICATED,
				ASK_BEFORE_SIGN
			};
		}
	}