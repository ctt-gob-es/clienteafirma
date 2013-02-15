/*
 * $Id: PdfPages.java 3934 2009-05-27 11:23:23Z blowagie $
 *
 * Copyright 1999, 2000, 2001, 2002 Bruno Lowagie
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
 * the Initial Developer are Copyright (C) 1999, 2000, 2001, 2002 by Bruno Lowagie.
 * All Rights Reserved.
 * Co-Developer of the code is Paulo Soares. Portions created by the Co-Developer
 * are Copyright (C) 2000, 2001, 2002 by Paulo Soares. All Rights Reserved.
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

package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.ArrayList;

import com.lowagie.text.Document;
import com.lowagie.text.ExceptionConverter;

/**
 * <CODE>PdfPages</CODE> is the PDF Pages-object.
 * <P>
 * The Pages of a document are accessible through a tree of nodes known as the Pages tree.
 * This tree defines the ordering of the pages in the document.<BR>
 * This object is described in the 'Portable Document Format Reference Manual version 1.3'
 * section 6.3 (page 71-73)
 *
 * @see		PdfPage
 */

class PdfPages {

    private final ArrayList pages = new ArrayList();
    private final ArrayList parents = new ArrayList();
    private final int leafSize = 10;
    private final PdfWriter writer;
    private PdfIndirectReference topParent;

    // constructors

/**
 * Constructs a <CODE>PdfPages</CODE>-object.
 */

    PdfPages(final PdfWriter writer) {
        this.writer = writer;
    }

    void addPage(final PdfDictionary page) {
        try {
            if (this.pages.size() % this.leafSize == 0) {
				this.parents.add(this.writer.getPdfIndirectReference());
			}
            final PdfIndirectReference parent = (PdfIndirectReference)this.parents.get(this.parents.size() - 1);
            page.put(PdfName.PARENT, parent);
            final PdfIndirectReference current = this.writer.getCurrentPage();
            this.writer.addToBody(page, current);
            this.pages.add(current);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    PdfIndirectReference addPageRef(final PdfIndirectReference pageRef) {
        try {
            if (this.pages.size() % this.leafSize == 0) {
				this.parents.add(this.writer.getPdfIndirectReference());
			}
            this.pages.add(pageRef);
            return (PdfIndirectReference)this.parents.get(this.parents.size() - 1);
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    // returns the top parent to include in the catalog
    PdfIndirectReference writePageTree() throws IOException {
        if (this.pages.isEmpty()) {
			throw new IOException("The document has no pages.");
		}
        int leaf = 1;
        ArrayList tParents = this.parents;
        ArrayList tPages = this.pages;
        ArrayList nextParents = new ArrayList();
        while (true) {
            leaf *= this.leafSize;
            final int stdCount = this.leafSize;
            int rightCount = tPages.size() % this.leafSize;
            if (rightCount == 0) {
				rightCount = this.leafSize;
			}
            for (int p = 0; p < tParents.size(); ++p) {
                int count;
                int thisLeaf = leaf;
                if (p == tParents.size() - 1) {
                    count = rightCount;
                    thisLeaf = this.pages.size() % leaf;
                    if (thisLeaf == 0) {
						thisLeaf = leaf;
					}
                } else {
					count = stdCount;
				}
                final PdfDictionary top = new PdfDictionary(PdfName.PAGES);
                top.put(PdfName.COUNT, new PdfNumber(thisLeaf));
                final PdfArray kids = new PdfArray();
                final ArrayList internal = kids.getArrayList();
                internal.addAll(tPages.subList(p * stdCount, p * stdCount + count));
                top.put(PdfName.KIDS, kids);
                if (tParents.size() > 1) {
                    if (p % this.leafSize == 0) {
						nextParents.add(this.writer.getPdfIndirectReference());
					}
                    top.put(PdfName.PARENT, (PdfIndirectReference)nextParents.get(p / this.leafSize));
                }
                else {
                	top.put(PdfName.ITXT, new PdfString(Document.getRelease()));
                }
                this.writer.addToBody(top, (PdfIndirectReference)tParents.get(p));
            }
            if (tParents.size() == 1) {
                this.topParent = (PdfIndirectReference)tParents.get(0);
                return this.topParent;
            }
            tPages = tParents;
            tParents = nextParents;
            nextParents = new ArrayList();
        }
    }








}