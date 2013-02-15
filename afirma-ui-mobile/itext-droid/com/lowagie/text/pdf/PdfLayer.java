/*
 * Copyright 2004 by Paulo Soares.
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

import java.util.ArrayList;
/**
 * An optional content group is a dictionary representing a collection of graphics
 * that can be made visible or invisible dynamically by users of viewer applications.
 * In iText they are referenced as layers.
 *
 * @author Paulo Soares (psoares@consiste.pt)
 */
class PdfLayer extends PdfDictionary implements PdfOCG {
    private PdfIndirectReference ref;
    private ArrayList children;
    private PdfLayer parent;
    private final String title;

    /**
     * Holds value of property on.
     */
    private boolean on = true;

    /**
     * Holds value of property onPanel.
     */
    private boolean onPanel = true;

    PdfLayer(final String title) {
        this.title = title;
    }




    String getTitle() {
        return this.title;
    }

    /**
     * Adds a child layer. Nested layers can only have one parent.
     * @param child the child layer
     */
    void addChild(final PdfLayer child) {
        if (child.parent != null) {
			throw new IllegalArgumentException("The layer '" + ((PdfString)child.get(PdfName.NAME)).toUnicodeString() + "' already has a parent.");
		}
        child.parent = this;
        if (this.children == null) {
			this.children = new ArrayList();
		}
        this.children.add(child);
    }


    /**
     * Gets the parent layer.
     * @return the parent layer or <CODE>null</CODE> if the layer has no parent
     */
    public PdfLayer getParent() {
        return this.parent;
    }

    /**
     * Gets the children layers.
     * @return the children layers or <CODE>null</CODE> if the layer has no children
     */
    public ArrayList getChildren() {
        return this.children;
    }

    /**
     * Gets the <CODE>PdfIndirectReference</CODE> that represents this layer.
     * @return the <CODE>PdfIndirectReference</CODE> that represents this layer
     */
    @Override
	public PdfIndirectReference getRef() {
        return this.ref;
    }

    /**
     * Sets the <CODE>PdfIndirectReference</CODE> that represents this layer.
     * This can only be done from PdfStamperImp.
     * @param	ref	The reference to the OCG object
     * @since	2.1.2
     */
    void setRef(final PdfIndirectReference ref) {
    	this.ref = ref;
    }

    /**
     * Sets the name of this layer.
     * @param name the name of this layer
     */
    public void setName(final String name) {
        put(PdfName.NAME, new PdfString(name, PdfObject.TEXT_UNICODE));
    }

    /**
     * Gets the dictionary representing the layer. It just returns <CODE>this</CODE>.
     * @return the dictionary representing the layer
     */
    @Override
	public PdfObject getPdfObject() {
        return this;
    }

    /**
     * Gets the initial visibility of the layer.
     * @return the initial visibility of the layer
     */
    public boolean isOn() {
        return this.on;
    }

    /**
     * Sets the initial visibility of the layer.
     * @param on the initial visibility of the layer
     */
    public void setOn(final boolean on) {
        this.on = on;
    }

    private PdfDictionary getUsage() {
        PdfDictionary usage = (PdfDictionary)get(PdfName.USAGE);
        if (usage == null) {
            usage = new PdfDictionary();
            put(PdfName.USAGE, usage);
        }
        return usage;
    }





    /**
     * Specifies the recommended state for content in this
     * group when the document (or part of it) is saved by a viewer application to a format
     * that does not support optional content (for example, an earlier version of
     * PDF or a raster image format).
     * @param export the export state
     */
    public void setExport(final boolean export) {
        final PdfDictionary usage = getUsage();
        final PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.EXPORTSTATE, export ? PdfName.ON : PdfName.OFF);
        usage.put(PdfName.EXPORT, dic);
    }





    /**
     * Indicates that the group should be set to that state when the
     * document is opened in a viewer application.
     * @param view the view state
     */
    public void setView(final boolean view) {
        final PdfDictionary usage = getUsage();
        final PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.VIEWSTATE, view ? PdfName.ON : PdfName.OFF);
        usage.put(PdfName.VIEW, dic);
    }

    /**
     * Gets the layer visibility in Acrobat's layer panel
     * @return the layer visibility in Acrobat's layer panel
     */
    public boolean isOnPanel() {
        return this.onPanel;
    }

    /**
     * Sets the visibility of the layer in Acrobat's layer panel. If <CODE>false</CODE>
     * the layer cannot be directly manipulated by the user. Note that any children layers will
     * also be absent from the panel.
     * @param onPanel the visibility of the layer in Acrobat's layer panel
     */
    public void setOnPanel(final boolean onPanel) {
        this.onPanel = onPanel;
    }

}
