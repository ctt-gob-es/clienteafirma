/*
 * $Id: PdfTemplate.java 3929 2009-05-22 13:26:41Z blowagie $
 *
 * Copyright 2001, 2002 Paulo Soares
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

import com.lowagie.text.Rectangle;

/**
 * Implements the form XObject.
 */

public class PdfTemplate extends PdfContentByte {
    static final int TYPE_TEMPLATE = 1;
    static final int TYPE_IMPORTED = 2;
    public static final int TYPE_PATTERN = 3;
    protected int type;
    /** The indirect reference to this template */
    protected PdfIndirectReference thisReference;

    /** The resources used by this template */
    protected PageResources pageResources;


    /** The bounding box of this template */
    protected Rectangle bBox = new Rectangle(0, 0);

    protected PdfArray matrix;

    protected PdfTransparencyGroup group;

    protected PdfOCG layer;

    /**
     *Creates a <CODE>PdfTemplate</CODE>.
     */

    protected PdfTemplate() {
        super(null);
        this.type = TYPE_TEMPLATE;
    }

    /**
     * Creates new PdfTemplate
     *
     * @param wr the <CODE>PdfWriter</CODE>
     */

    PdfTemplate(final PdfWriter wr) {
        super(wr);
        this.type = TYPE_TEMPLATE;
        this.pageResources = new PageResources();
        this.pageResources.addDefaultColor(wr.getDefaultColorspace());
        this.thisReference = this.writer.getPdfIndirectReference();
    }

    /**
     * Creates a new template.
     * <P>
     * Creates a new template that is nothing more than a form XObject. This template can be included
     * in this template or in another template. Templates are only written
     * to the output when the document is closed permitting things like showing text in the first page
     * that is only defined in the last page.
     *
     * @param writer the PdfWriter to use
     * @param width the bounding box width
     * @param height the bounding box height
     * @return the created template
     */
    static PdfTemplate createTemplate(final PdfWriter writer, final float width, final float height) {
        return createTemplate(writer, width, height, null);
    }

    private static PdfTemplate createTemplate(final PdfWriter writer, final float width, final float height, final PdfName forcedName) {
        final PdfTemplate template = new PdfTemplate(writer);
        template.setWidth(width);
        template.setHeight(height);
        writer.addDirectTemplateSimple(template, forcedName);
        return template;
    }

    /**
     * Sets the bounding width of this template.
     *
     * @param width the bounding width
     */

    public void setWidth(final float width) {
        this.bBox.setLeft(0);
        this.bBox.setRight(width);
    }

    /**
     * Sets the bounding height of this template.
     *
     * @param height the bounding height
     */

    public void setHeight(final float height) {
        this.bBox.setBottom(0);
        this.bBox.setTop(height);
    }

    /**
     * Gets the bounding width of this template.
     *
     * @return width the bounding width
     */
    public float getWidth() {
        return this.bBox.getWidth();
    }

    /**
     * Gets the bounding height of this template.
     *
     * @return height the bounding height
     */

    public float getHeight() {
        return this.bBox.getHeight();
    }

    public Rectangle getBoundingBox() {
        return this.bBox;
    }

    public void setBoundingBox(final Rectangle bBox) {
        this.bBox = bBox;
    }

    /**
     * Sets the layer this template belongs to.
     * @param layer the layer this template belongs to
     */
    public void setLayer(final PdfOCG layer) {
        this.layer = layer;
    }

    /**
     * Gets the layer this template belongs to.
     * @return the layer this template belongs to or <code>null</code> for no layer defined
     */
    public PdfOCG getLayer() {
        return this.layer;
    }

    void setMatrix(final float a, final float b, final float c, final float d, final float e, final float f) {
		this.matrix = new PdfArray();
		this.matrix.add(new PdfNumber(a));
		this.matrix.add(new PdfNumber(b));
		this.matrix.add(new PdfNumber(c));
		this.matrix.add(new PdfNumber(d));
		this.matrix.add(new PdfNumber(e));
		this.matrix.add(new PdfNumber(f));
	}

	PdfArray getMatrix() {
		return this.matrix;
	}

    /**
     * Gets the indirect reference to this template.
     *
     * @return the indirect reference to this template
     */

    public PdfIndirectReference getIndirectReference() {
    	// uncomment the null check as soon as we're sure all examples still work
    	if (this.thisReference == null /* && writer != null */) {
    		this.thisReference = this.writer.getPdfIndirectReference();
    	}
        return this.thisReference;
    }

    void beginVariableText() {
        this.content.append("/Tx BMC ");
    }

    void endVariableText() {
        this.content.append("EMC ");
    }

    /**
     * Constructs the resources used by this template.
     *
     * @return the resources used by this template
     */

    PdfObject getResources() {
        return getPageResources().getResources();
    }

    /**
     * Gets the stream representing this template.
     *
     * @param	compressionLevel	the compressionLevel
     * @return the stream representing this template
     * @since	2.1.3	(replacing the method without param compressionLevel)
     */
    PdfStream getFormXObject(final int compressionLevel) throws IOException {
        return new PdfFormXObject(this, compressionLevel);
    }

    /**
     * Gets a duplicate of this <CODE>PdfTemplate</CODE>. All
     * the members are copied by reference but the buffer stays different.
     * @return a copy of this <CODE>PdfTemplate</CODE>
     */

    @Override
	public PdfContentByte getDuplicate() {
        final PdfTemplate tpl = new PdfTemplate();
        tpl.writer = this.writer;
        tpl.pdf = this.pdf;
        tpl.thisReference = this.thisReference;
        tpl.pageResources = this.pageResources;
        tpl.bBox = new Rectangle(this.bBox);
        tpl.group = this.group;
        tpl.layer = this.layer;
        if (this.matrix != null) {
            tpl.matrix = new PdfArray(this.matrix);
        }
        tpl.separator = this.separator;
        return tpl;
    }

    public int getType() {
        return this.type;
    }

    @Override
	PageResources getPageResources() {
        return this.pageResources;
    }

    /** Getter for property group.
     * @return Value of property group.
     *
     */
    public PdfTransparencyGroup getGroup() {
        return this.group;
    }

    /** Setter for property group.
     * @param group New value of property group.
     *
     */
    public void setGroup(final PdfTransparencyGroup group) {
        this.group = group;
    }

}