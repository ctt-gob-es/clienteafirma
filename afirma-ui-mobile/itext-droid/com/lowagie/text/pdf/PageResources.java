/*
 * $Id: PageResources.java 3117 2008-01-31 05:53:22Z xlv $
 *
 * Copyright 2003-2005 by Paulo Soares.
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

import java.util.HashMap;

class PageResources {

    private final PdfDictionary fontDictionary = new PdfDictionary();
    private final PdfDictionary xObjectDictionary = new PdfDictionary();
    private final PdfDictionary colorDictionary = new PdfDictionary();
    private final PdfDictionary patternDictionary = new PdfDictionary();
    private final PdfDictionary shadingDictionary = new PdfDictionary();
    private final PdfDictionary extGStateDictionary = new PdfDictionary();
    private final PdfDictionary propertyDictionary = new PdfDictionary();
    private HashMap forbiddenNames;
    private PdfDictionary originalResources;
    private int namePtr[] = {0};
    private HashMap usedNames;

    PageResources() {
    }

    void setOriginalResources(final PdfDictionary resources, final int newNamePtr[]) {
        if (newNamePtr != null) {
			this.namePtr = newNamePtr;
		}
        this.forbiddenNames = new HashMap();
        this.usedNames = new HashMap();
        if (resources == null) {
			return;
		}
        this.originalResources = new PdfDictionary();
        this.originalResources.merge(resources);
        for (final Object element : resources.getKeys()) {
            final PdfName key = (PdfName)element;
            final PdfObject sub = PdfReader.getPdfObject(resources.get(key));
            if (sub != null && sub.isDictionary()) {
                final PdfDictionary dic = (PdfDictionary)sub;
                for (final Object element2 : dic.getKeys()) {
                    this.forbiddenNames.put(element2, null);
                }
                final PdfDictionary dic2 = new PdfDictionary();
                dic2.merge(dic);
                this.originalResources.put(key, dic2);
            }
        }
    }

    private PdfName translateName(final PdfName name) {
        PdfName translated = name;
        if (this.forbiddenNames != null) {
            translated = (PdfName)this.usedNames.get(name);
            if (translated == null) {
                while (true) {
                    translated = new PdfName("Xi" + this.namePtr[0]++);
                    if (!this.forbiddenNames.containsKey(translated)) {
						break;
					}
                }
                this.usedNames.put(name, translated);
            }
        }
        return translated;
    }

    PdfName addFont(PdfName name, final PdfIndirectReference reference) {
        name = translateName(name);
        this.fontDictionary.put(name, reference);
        return name;
    }

    PdfName addXObject(PdfName name, final PdfIndirectReference reference) {
        name = translateName(name);
        this.xObjectDictionary.put(name, reference);
        return name;
    }

    PdfName addColor(PdfName name, final PdfIndirectReference reference) {
        name = translateName(name);
        this.colorDictionary.put(name, reference);
        return name;
    }



    void addDefaultColor(final PdfDictionary dic) {
        this.colorDictionary.merge(dic);
    }

    void addDefaultColorDiff(final PdfDictionary dic) {
        this.colorDictionary.mergeDifferent(dic);
    }

    PdfName addShading(PdfName name, final PdfIndirectReference reference) {
        name = translateName(name);
        this.shadingDictionary.put(name, reference);
        return name;
    }

    PdfName addPattern(PdfName name, final PdfIndirectReference reference) {
        name = translateName(name);
        this.patternDictionary.put(name, reference);
        return name;
    }

    PdfName addExtGState(PdfName name, final PdfIndirectReference reference) {
        name = translateName(name);
        this.extGStateDictionary.put(name, reference);
        return name;
    }

    PdfName addProperty(PdfName name, final PdfIndirectReference reference) {
        name = translateName(name);
        this.propertyDictionary.put(name, reference);
        return name;
    }

    PdfDictionary getResources() {
       final PdfResources resources = new PdfResources();
        if (this.originalResources != null) {
			resources.putAll(this.originalResources);
		}
        resources.put(PdfName.PROCSET, new PdfLiteral("[/PDF /Text /ImageB /ImageC /ImageI]"));
        resources.add(PdfName.FONT, this.fontDictionary);
        resources.add(PdfName.XOBJECT, this.xObjectDictionary);
        resources.add(PdfName.COLORSPACE, this.colorDictionary);
        resources.add(PdfName.PATTERN, this.patternDictionary);
        resources.add(PdfName.SHADING, this.shadingDictionary);
        resources.add(PdfName.EXTGSTATE, this.extGStateDictionary);
        resources.add(PdfName.PROPERTIES, this.propertyDictionary);
        return resources;
    }

    boolean hasResources() {
        return this.fontDictionary.size() > 0
            || this.xObjectDictionary.size() > 0
            || this.colorDictionary.size() > 0
            || this.patternDictionary.size() > 0
            || this.shadingDictionary.size() > 0
            || this.extGStateDictionary.size() > 0
            || this.propertyDictionary.size() > 0;
    }
}