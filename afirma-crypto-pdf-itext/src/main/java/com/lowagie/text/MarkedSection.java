/*
 * $Id: MarkedSection.java 3373 2008-05-12 16:21:24Z xlv $
 *
 * Copyright 2007 by Bruno Lowagie.
 *
 * The contents of this file are subject to the Mozilla Public License Version 1.1
 * (the "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the License.
 *
 * The Original Code is 'iText, a free JAVA-PDF library'.
 *
 * The Initial Developer of the Original Code is Bruno Lowagie. Portions created by
 * the Initial Developer are Copyright (C) 1999-2007 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000-2007 by Paulo Soares. All Rights Reserved.
 *
 * Contributor(s): all the names of the contributors are added in the source code
 * where applicable.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * LGPL license (the "GNU LIBRARY GENERAL PUBLIC LICENSE"), in which case the
 * provisions of LGPL are applicable instead of those above.  If you wish to
 * allow use of your version of this file only under the terms of the LGPL
 * License and not to allow others to use your version of this file under
 * the MPL, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the LGPL.
 * If you do not delete the provisions above, a recipient may use your version
 * of this file under either the MPL or the GNU LIBRARY GENERAL PUBLIC LICENSE.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the MPL as stated above or under the terms of the GNU
 * Library General Public License as published by the Free Software Foundation;
 * either version 2 of the License, or any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library general Public License for more
 * details.
 *
 * If you didn't download this code from the following link, you should check if
 * you aren't using an obsolete version:
 * http://www.lowagie.com/iText/
 */

package com.lowagie.text;

import java.util.Iterator;

/**
 * Wrapper that allows to add properties to a Chapter/Section object.
 * Before iText 1.5 every 'basic building block' implemented the MarkupAttributes interface.
 * By setting attributes, you could add markup to the corresponding XML and/or HTML tag.
 * This functionality was hardly used by anyone, so it was removed, and replaced by
 * the MarkedObject functionality.
 */

class MarkedSection extends MarkedObject {

	/** This is the title of this section. */
	private MarkedObject title = null;

    /**
     * Processes the element by adding it (or the different parts) to an
     * <CODE>ElementListener</CODE>.
     *
     * @param       listener        an <CODE>ElementListener</CODE>
     * @return <CODE>true</CODE> if the element was processed successfully
     */
    @Override
	public boolean process(final ElementListener listener) {
        try {
        	Element element;
            for (final Iterator i = ((Section)this.element).iterator(); i.hasNext(); ) {
            	element = (Element)i.next();
                listener.add(element);
            }
            return true;
        }
        catch(final DocumentException de) {
            return false;
        }
    }

	// public methods

	/**
	 * Sets the title of this section.
	 *
	 * @param	title	the new title
	 */
	public void setTitle(final MarkedObject title) {
		if (title.element instanceof Paragraph) {
			this.title = title;
		}
	}

	/**
	 * Gets the title of this MarkedSection.
	 * @return	a MarkObject with a Paragraph containing the title of a Section
	 * @since	iText 2.0.8
	 */
    public MarkedObject getTitle() {
    	final Paragraph result = Section.constructTitle((Paragraph)this.title.element, ((Section)this.element).numbers, ((Section)this.element).numberDepth, ((Section)this.element).numberStyle);
        final MarkedObject mo = new MarkedObject(result);
        mo.markupAttributes = this.title.markupAttributes;
        return mo;
    }

	/**
	 * Sets the depth of the sectionnumbers that will be shown preceding the title.
	 * <P>
	 * If the numberdepth is 0, the sections will not be numbered. If the numberdepth
	 * is 1, the section will be numbered with their own number. If the numberdepth is
	 * higher (for instance x > 1), the numbers of x - 1 parents will be shown.
	 *
	 * @param	numberDepth		the new numberDepth
	 */
	public void setNumberDepth(final int numberDepth) {
		((Section)this.element).setNumberDepth(numberDepth);
	}

	/**
	 * Sets the indentation of this <CODE>Section</CODE> on the left side.
	 *
	 * @param	indentation		the indentation
	 */
	public void setIndentationLeft(final float indentation) {
		((Section)this.element).setIndentationLeft(indentation);
	}

	/**
	 * Sets the indentation of this <CODE>Section</CODE> on the right side.
	 *
	 * @param	indentation		the indentation
	 */
	public void setIndentationRight(final float indentation) {
		((Section)this.element).setIndentationRight(indentation);
	}

	/**
	 * Sets the indentation of the content of this <CODE>Section</CODE>.
	 *
	 * @param	indentation		the indentation
	 */
	public void setIndentation(final float indentation) {
		((Section)this.element).setIndentation(indentation);
	}

	/** Setter for property bookmarkOpen.
	 * @param bookmarkOpen false if the bookmark children are not
	 * visible.
	 */
	public void setBookmarkOpen(final boolean bookmarkOpen) {
	 	((Section)this.element).setBookmarkOpen(bookmarkOpen);
	}

	/**
	 * Setter for property triggerNewPage.
	 * @param triggerNewPage true if a new page has to be triggered.
	 */
	public void setTriggerNewPage(final boolean triggerNewPage) {
	  	((Section)this.element).setTriggerNewPage(triggerNewPage);
	}

	/**
	 * Sets the bookmark title. The bookmark title is the same as the section title but
	 * can be changed with this method.
	 * @param bookmarkTitle the bookmark title
	 */
	public void setBookmarkTitle(final String bookmarkTitle) {
	  	((Section)this.element).setBookmarkTitle(bookmarkTitle);
	}

}
