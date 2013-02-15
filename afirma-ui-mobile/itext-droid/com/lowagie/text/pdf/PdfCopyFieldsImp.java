/*
 * $Id: PdfCopyFieldsImp.java 3763 2009-03-06 17:30:30Z blowagie $
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

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.exceptions.BadPasswordException;

/**
 *
 * @author  psoares
 */
class PdfCopyFieldsImp extends PdfWriter {

    private static final PdfName iTextTag = new PdfName("_iTextTag_");
    private static final Integer zero = new Integer(0);
    private final ArrayList readers = new ArrayList();
    HashMap readers2intrefs = new HashMap();
    private final HashMap pages2intrefs = new HashMap();
    private final HashMap visited = new HashMap();
    ArrayList fields = new ArrayList();
    private RandomAccessFileOrArray file;
    private final HashMap fieldTree = new HashMap();
    private final ArrayList pageRefs = new ArrayList();
    private final ArrayList pageDics = new ArrayList();
    private final PdfDictionary resources = new PdfDictionary();
    private PdfDictionary form;
    private boolean closing = false;
    private final Document nd;
    private HashMap tabOrder;
    private final ArrayList calculationOrder = new ArrayList();
    private ArrayList calculationOrderRefs;
    private boolean hasSignature;

    PdfCopyFieldsImp(final OutputStream os, final Calendar globalDate) throws DocumentException {
        this(os, '\0', globalDate);
    }

    private PdfCopyFieldsImp(final OutputStream os, final char pdfVersion, final Calendar globalDate) throws DocumentException {
        super(new PdfDocument(globalDate), os);
        this.pdf.addWriter(this);
        if (pdfVersion != 0) {
			super.setPdfVersion(pdfVersion);
		}
        this.nd = new Document();
        this.nd.addDocListener(this.pdf);
    }

    void addDocument(PdfReader reader, final List pagesToKeep) throws DocumentException, IOException {
        if (!this.readers2intrefs.containsKey(reader) && reader.isTampered()) {
			throw new DocumentException("The document was reused.");
		}
        reader = new PdfReader(reader);
        reader.selectPages(pagesToKeep);
        if (reader.getNumberOfPages() == 0) {
			return;
		}
        reader.setTampered(false);
        addDocument(reader);
    }

    void addDocument(PdfReader reader) throws DocumentException, IOException {
        if (!reader.isOpenedWithFullPermissions()) {
			throw new BadPasswordException("PdfReader not opened with owner password");
		}
        openDoc();
        if (this.readers2intrefs.containsKey(reader)) {
            reader = new PdfReader(reader);
        }
        else {
            if (reader.isTampered()) {
				throw new DocumentException("The document was reused.");
			}
            reader.consolidateNamedDestinations();
            reader.setTampered(true);
        }
        reader.shuffleSubsetNames();
        this.readers2intrefs.put(reader, new IntHashtable());
        this.readers.add(reader);
        final int len = reader.getNumberOfPages();
        final IntHashtable refs = new IntHashtable();
        for (int p = 1; p <= len; ++p) {
            refs.put(reader.getPageOrigRef(p).getNumber(), 1);
            reader.releasePage(p);
        }
        this.pages2intrefs.put(reader, refs);
        this.visited.put(reader, new IntHashtable());
        this.fields.add(reader.getAcroFields());
        updateCalculationOrder(reader);
    }

    private static String getCOName(final PdfReader reader, PRIndirectReference ref) {
        String name = "";
        while (ref != null) {
            final PdfObject obj = PdfReader.getPdfObject(ref);
            if (obj == null || obj.type() != PdfObject.DICTIONARY) {
				break;
			}
            final PdfDictionary dic = (PdfDictionary)obj;
            final PdfString t = dic.getAsString(PdfName.T);
            if (t != null) {
                name = t.toUnicodeString()+ "." + name;
            }
            ref = (PRIndirectReference)dic.get(PdfName.PARENT);
        }
        if (name.endsWith(".")) {
			name = name.substring(0, name.length() - 1);
		}
        return name;
    }

    /**
     * @since	2.1.5; before 2.1.5 the method was private
     */
    protected void updateCalculationOrder(final PdfReader reader) {
        final PdfDictionary catalog = reader.getCatalog();
        final PdfDictionary acro = catalog.getAsDict(PdfName.ACROFORM);
        if (acro == null) {
			return;
		}
        final PdfArray co = acro.getAsArray(PdfName.CO);
        if (co == null || co.size() == 0) {
			return;
		}
        final AcroFields af = reader.getAcroFields();
        for (int k = 0; k < co.size(); ++k) {
            final PdfObject obj = co.getPdfObject(k);
            if (obj == null || !obj.isIndirect()) {
				continue;
			}
            String name = getCOName(reader, (PRIndirectReference)obj);
            if (af.getFieldItem(name) == null) {
				continue;
			}
            name = "." + name;
            if (this.calculationOrder.contains(name)) {
				continue;
			}
            this.calculationOrder.add(name);
        }
    }

    private void propagate(final PdfObject obj, final PdfIndirectReference refo, final boolean restricted) throws IOException {
        if (obj == null) {
			return;
		}
//        if (refo != null)
//            addToBody(obj, refo);
        if (obj instanceof PdfIndirectReference) {
			return;
		}
        switch (obj.type()) {
            case PdfObject.DICTIONARY:
            case PdfObject.STREAM: {
                final PdfDictionary dic = (PdfDictionary)obj;
                for (final Object element : dic.getKeys()) {
                    final PdfName key = (PdfName)element;
                    if (restricted && (key.equals(PdfName.PARENT) || key.equals(PdfName.KIDS))) {
						continue;
					}
                    final PdfObject ob = dic.get(key);
                    if (ob != null && ob.isIndirect()) {
                        final PRIndirectReference ind = (PRIndirectReference)ob;
                        if (!setVisited(ind) && !isPage(ind)) {
                            final PdfIndirectReference ref = getNewReference(ind);
                            propagate(PdfReader.getPdfObjectRelease(ind), ref, restricted);
                        }
                    } else {
						propagate(ob, null, restricted);
					}
                }
                break;
            }
            case PdfObject.ARRAY: {
                //PdfArray arr = new PdfArray();
                for (final Iterator it = ((PdfArray)obj).listIterator(); it.hasNext();) {
                    final PdfObject ob = (PdfObject)it.next();
                    if (ob != null && ob.isIndirect()) {
                        final PRIndirectReference ind = (PRIndirectReference)ob;
                        if (!isVisited(ind) && !isPage(ind)) {
                            final PdfIndirectReference ref = getNewReference(ind);
                            propagate(PdfReader.getPdfObjectRelease(ind), ref, restricted);
                        }
                    } else {
						propagate(ob, null, restricted);
					}
                }
                break;
            }
            case PdfObject.INDIRECT: {
                throw new RuntimeException("Reference pointing to reference.");
            }
        }
    }

    private void adjustTabOrder(final PdfArray annots, final PdfIndirectReference ind, final PdfNumber nn) {
        final int v = nn.intValue();
        ArrayList t = (ArrayList)this.tabOrder.get(annots);
        if (t == null) {
            t = new ArrayList();
            final int size = annots.size() - 1;
            for (int k = 0; k < size; ++k) {
                t.add(zero);
            }
            t.add(new Integer(v));
            this.tabOrder.put(annots, t);
            annots.add(ind);
        }
        else {
            int size = t.size() - 1;
            for (int k = size; k >= 0; --k) {
                if (((Integer)t.get(k)).intValue() <= v) {
                    t.add(k + 1, new Integer(v));
                    annots.add(k + 1, ind);
                    size = -2;
                    break;
                }
            }
            if (size != -2) {
                t.add(0, new Integer(v));
                annots.add(0, ind);
            }
        }
    }

    private PdfArray branchForm(final HashMap level, final PdfIndirectReference parent, final String fname) throws IOException {
        final PdfArray arr = new PdfArray();
        for (final Iterator it = level.entrySet().iterator(); it.hasNext();) {
            final Map.Entry entry = (Map.Entry) it.next();
            final String name = (String) entry.getKey();
            final Object obj = entry.getValue();
            final PdfIndirectReference ind = getPdfIndirectReference();
            final PdfDictionary dic = new PdfDictionary();
            if (parent != null) {
				dic.put(PdfName.PARENT, parent);
			}
            dic.put(PdfName.T, new PdfString(name, PdfObject.TEXT_UNICODE));
            final String fname2 = fname + "." + name;
            final int coidx = this.calculationOrder.indexOf(fname2);
            if (coidx >= 0) {
				this.calculationOrderRefs.set(coidx, ind);
			}
            if (obj instanceof HashMap) {
                dic.put(PdfName.KIDS, branchForm((HashMap)obj, ind, fname2));
                arr.add(ind);
                addToBody(dic, ind);
            }
            else {
                final ArrayList list = (ArrayList)obj;
                dic.mergeDifferent((PdfDictionary)list.get(0));
                if (list.size() == 3) {
                    dic.mergeDifferent((PdfDictionary)list.get(2));
                    final int page = ((Integer)list.get(1)).intValue();
                    final PdfDictionary pageDic = (PdfDictionary)this.pageDics.get(page - 1);
                    PdfArray annots = pageDic.getAsArray(PdfName.ANNOTS);
                    if (annots == null) {
                        annots = new PdfArray();
                        pageDic.put(PdfName.ANNOTS, annots);
                    }
                    final PdfNumber nn = (PdfNumber)dic.get(iTextTag);
                    dic.remove(iTextTag);
                    adjustTabOrder(annots, ind, nn);
                }
                else {
                    final PdfArray kids = new PdfArray();
                    for (int k = 1; k < list.size(); k += 2) {
                        final int page = ((Integer)list.get(k)).intValue();
                        final PdfDictionary pageDic = (PdfDictionary)this.pageDics.get(page - 1);
                        PdfArray annots = pageDic.getAsArray(PdfName.ANNOTS);
                        if (annots == null) {
                            annots = new PdfArray();
                            pageDic.put(PdfName.ANNOTS, annots);
                        }
                        final PdfDictionary widget = new PdfDictionary();
                        widget.merge((PdfDictionary)list.get(k + 1));
                        widget.put(PdfName.PARENT, ind);
                        final PdfNumber nn = (PdfNumber)widget.get(iTextTag);
                        widget.remove(iTextTag);
                        final PdfIndirectReference wref = addToBody(widget).getIndirectReference();
                        adjustTabOrder(annots, wref, nn);
                        kids.add(wref);
                        propagate(widget, null, false);
                    }
                    dic.put(PdfName.KIDS, kids);
                }
                arr.add(ind);
                addToBody(dic, ind);
                propagate(dic, null, false);
            }
        }
        return arr;
    }

    private void createAcroForms() throws IOException {
        if (this.fieldTree.isEmpty()) {
			return;
		}
        this.form = new PdfDictionary();
        this.form.put(PdfName.DR, this.resources);
        propagate(this.resources, null, false);
        this.form.put(PdfName.DA, new PdfString("/Helv 0 Tf 0 g "));
        this.tabOrder = new HashMap();
        this.calculationOrderRefs = new ArrayList(this.calculationOrder);
        this.form.put(PdfName.FIELDS, branchForm(this.fieldTree, null, ""));
        if (this.hasSignature) {
			this.form.put(PdfName.SIGFLAGS, new PdfNumber(3));
		}
        final PdfArray co = new PdfArray();
        for (int k = 0; k < this.calculationOrderRefs.size(); ++k) {
            final Object obj = this.calculationOrderRefs.get(k);
            if (obj instanceof PdfIndirectReference) {
				co.add((PdfIndirectReference)obj);
			}
        }
        if (co.size() > 0) {
			this.form.put(PdfName.CO, co);
		}
    }

    @Override
	public void close() {
        if (this.closing) {
            super.close();
            return;
        }
        this.closing = true;
        try {
            closeIt();
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }

    /**
     * Creates the new PDF by merging the fields and forms.
     */
    private void closeIt() throws IOException {
        for (int k = 0; k < this.readers.size(); ++k) {
            ((PdfReader)this.readers.get(k)).removeFields();
        }
        for (int r = 0; r < this.readers.size(); ++r) {
            final PdfReader reader = (PdfReader)this.readers.get(r);
            for (int page = 1; page <= reader.getNumberOfPages(); ++page) {
                this.pageRefs.add(getNewReference(reader.getPageOrigRef(page)));
                this.pageDics.add(reader.getPageN(page));
            }
        }
        mergeFields();
        createAcroForms();
        for (int r = 0; r < this.readers.size(); ++r) {
                final PdfReader reader = (PdfReader)this.readers.get(r);
                for (int page = 1; page <= reader.getNumberOfPages(); ++page) {
                    final PdfDictionary dic = reader.getPageN(page);
                    final PdfIndirectReference pageRef = getNewReference(reader.getPageOrigRef(page));
                    final PdfIndirectReference parent = this.root.addPageRef(pageRef);
                    dic.put(PdfName.PARENT, parent);
                    propagate(dic, pageRef, false);
                }
        }
        for (final Iterator it = this.readers2intrefs.entrySet().iterator(); it.hasNext();) {
            final Map.Entry entry = (Map.Entry) it.next();
            final PdfReader reader = (PdfReader) entry.getKey();
            try {
                this.file = reader.getSafeFile();
                this.file.reOpen();
                final IntHashtable t = (IntHashtable) entry.getValue();
                final int keys[] = t.toOrderedKeys();
                for (final int key : keys) {
                    final PRIndirectReference ref = new PRIndirectReference(reader, key);
                    addToBody(PdfReader.getPdfObjectRelease(ref), t.get(key));
                }
            }
            finally {
                try {
                    this.file.close();
                    reader.close();
                }
                catch (final Exception e) {
                    // empty on purpose
                }
            }
        }
        this.pdf.close();
    }

    private void addPageOffsetToField(final HashMap fd, final int pageOffset) {
        if (pageOffset == 0) {
			return;
		}
        for (final Iterator it = fd.values().iterator(); it.hasNext();) {
            final AcroFields.Item item = (AcroFields.Item)it.next();
            for (int k = 0; k < item.size(); ++k) {
                final int p = item.getPage(k).intValue();
                item.forcePage(k, p + pageOffset);
            }
        }
    }

    private void createWidgets(final ArrayList list, final AcroFields.Item item) {
        for (int k = 0; k < item.size(); ++k) {
            list.add(item.getPage(k));
            final PdfDictionary merged = item.getMerged(k);
            final PdfObject dr = merged.get(PdfName.DR);
            if (dr != null) {
				PdfFormField.mergeResources(this.resources, (PdfDictionary)PdfReader.getPdfObject(dr));
			}
            final PdfDictionary widget = new PdfDictionary();
            for (final Object element : merged.getKeys()) {
                final PdfName key = (PdfName)element;
                if (widgetKeys.containsKey(key)) {
					widget.put(key, merged.get(key));
				}
            }
            widget.put(iTextTag, new PdfNumber(item.getTabOrder(k).intValue() + 1));
            list.add(widget);
        }
    }

    private void mergeField(final String name, final AcroFields.Item item) {
        HashMap map = this.fieldTree;
        final StringTokenizer tk = new StringTokenizer(name, ".");
        if (!tk.hasMoreTokens()) {
			return;
		}
        while (true) {
            final String s = tk.nextToken();
            Object obj = map.get(s);
            if (tk.hasMoreTokens()) {
                if (obj == null) {
                    obj = new HashMap();
                    map.put(s, obj);
                    map = (HashMap)obj;
                    continue;
                }
                else if (obj instanceof HashMap) {
					map = (HashMap)obj;
				} else {
					return;
				}
            }
            else {
                if (obj instanceof HashMap) {
					return;
				}
                final PdfDictionary merged = item.getMerged(0);
                if (obj == null) {
                    final PdfDictionary field = new PdfDictionary();
                    if (PdfName.SIG.equals(merged.get(PdfName.FT))) {
						this.hasSignature = true;
					}
                    for (final Object element : merged.getKeys()) {
                        final PdfName key = (PdfName)element;
                        if (fieldKeys.containsKey(key)) {
							field.put(key, merged.get(key));
						}
                    }
                    final ArrayList list = new ArrayList();
                    list.add(field);
                    createWidgets(list, item);
                    map.put(s, list);
                }
                else {
                    final ArrayList list = (ArrayList)obj;
                    final PdfDictionary field = (PdfDictionary)list.get(0);
                    final PdfName type1 = (PdfName)field.get(PdfName.FT);
                    final PdfName type2 = (PdfName)merged.get(PdfName.FT);
                    if (type1 == null || !type1.equals(type2)) {
						return;
					}
                    int flag1 = 0;
                    final PdfObject f1 = field.get(PdfName.FF);
                    if (f1 != null && f1.isNumber()) {
						flag1 = ((PdfNumber)f1).intValue();
					}
                    int flag2 = 0;
                    final PdfObject f2 = merged.get(PdfName.FF);
                    if (f2 != null && f2.isNumber()) {
						flag2 = ((PdfNumber)f2).intValue();
					}
                    if (type1.equals(PdfName.BTN)) {
                        if (((flag1 ^ flag2) & PdfFormField.FF_PUSHBUTTON) != 0) {
							return;
						}
                        if ((flag1 & PdfFormField.FF_PUSHBUTTON) == 0 && ((flag1 ^ flag2) & PdfFormField.FF_RADIO) != 0) {
							return;
						}
                    }
                    else if (type1.equals(PdfName.CH)) {
                        if (((flag1 ^ flag2) & PdfFormField.FF_COMBO) != 0) {
							return;
						}
                    }
                    createWidgets(list, item);
                }
                return;
            }
        }
    }

    void mergeWithMaster(final HashMap fd) {
        for (final Iterator it = fd.entrySet().iterator(); it.hasNext();) {
            final Map.Entry entry = (Map.Entry) it.next();
            final String name = (String) entry.getKey();
            mergeField(name, (AcroFields.Item) entry.getValue());
        }
    }

    void mergeFields() {
        int pageOffset = 0;
        for (int k = 0; k < this.fields.size(); ++k) {
            final HashMap fd = ((AcroFields)this.fields.get(k)).getFields();
            addPageOffsetToField(fd, pageOffset);
            mergeWithMaster(fd);
            pageOffset += ((PdfReader)this.readers.get(k)).getNumberOfPages();
        }
    }

    @Override
	public PdfIndirectReference getPageReference(final int page) {
        return (PdfIndirectReference)this.pageRefs.get(page - 1);
    }

    @Override
	protected PdfDictionary getCatalog(final PdfIndirectReference rootObj) {
        try {
            final PdfDictionary cat = this.pdf.getCatalog(rootObj);
            if (this.form != null) {
                final PdfIndirectReference ref = addToBody(this.form).getIndirectReference();
                cat.put(PdfName.ACROFORM, ref);
            }
            return cat;
        }
        catch (final IOException e) {
            throw new ExceptionConverter(e);
        }
    }

    private PdfIndirectReference getNewReference(final PRIndirectReference ref) {
        return new PdfIndirectReference(0, getNewObjectNumber(ref.getReader(), ref.getNumber(), 0));
    }

    @Override
	protected int getNewObjectNumber(final PdfReader reader, final int number, final int generation) {
        final IntHashtable refs = (IntHashtable)this.readers2intrefs.get(reader);
        int n = refs.get(number);
        if (n == 0) {
            n = getIndirectReferenceNumber();
            refs.put(number, n);
        }
        return n;
    }


    /**
     * Sets a reference to "visited" in the copy process.
     * @param	ref	the reference that needs to be set to "visited"
     * @return	true if the reference was set to visited
     */
    private boolean setVisited(final PRIndirectReference ref) {
        final IntHashtable refs = (IntHashtable)this.visited.get(ref.getReader());
        if (refs != null) {
			return refs.put(ref.getNumber(), 1) != 0;
		} else {
			return false;
		}
    }

    /**
     * Checks if a reference has already been "visited" in the copy process.
     * @param	ref	the reference that needs to be checked
     * @return	true if the reference was already visited
     */
    private boolean isVisited(final PRIndirectReference ref) {
        final IntHashtable refs = (IntHashtable)this.visited.get(ref.getReader());
        if (refs != null) {
			return refs.containsKey(ref.getNumber());
		} else {
			return false;
		}
    }



    /**
     * Checks if a reference refers to a page object.
     * @param	ref	the reference that needs to be checked
     * @return	true is the reference refers to a page object.
     */
    private boolean isPage(final PRIndirectReference ref) {
        final IntHashtable refs = (IntHashtable)this.pages2intrefs.get(ref.getReader());
        if (refs != null) {
			return refs.containsKey(ref.getNumber());
		} else {
			return false;
		}
    }

    @Override
	RandomAccessFileOrArray getReaderFile(final PdfReader reader) {
            return this.file;
    }

    public void openDoc() {
        if (!this.nd.isOpen()) {
			this.nd.open();
		}
    }

    private static final HashMap widgetKeys = new HashMap();
    protected static final HashMap fieldKeys = new HashMap();
    static {
        final Integer one = new Integer(1);
        widgetKeys.put(PdfName.SUBTYPE, one);
        widgetKeys.put(PdfName.CONTENTS, one);
        widgetKeys.put(PdfName.RECT, one);
        widgetKeys.put(PdfName.NM, one);
        widgetKeys.put(PdfName.M, one);
        widgetKeys.put(PdfName.F, one);
        widgetKeys.put(PdfName.BS, one);
        widgetKeys.put(PdfName.BORDER, one);
        widgetKeys.put(PdfName.AP, one);
        widgetKeys.put(PdfName.AS, one);
        widgetKeys.put(PdfName.C, one);
        widgetKeys.put(PdfName.A, one);
        widgetKeys.put(PdfName.STRUCTPARENT, one);
        widgetKeys.put(PdfName.OC, one);
        widgetKeys.put(PdfName.H, one);
        widgetKeys.put(PdfName.MK, one);
        widgetKeys.put(PdfName.DA, one);
        widgetKeys.put(PdfName.Q, one);
        fieldKeys.put(PdfName.AA, one);
        fieldKeys.put(PdfName.FT, one);
        fieldKeys.put(PdfName.TU, one);
        fieldKeys.put(PdfName.TM, one);
        fieldKeys.put(PdfName.FF, one);
        fieldKeys.put(PdfName.V, one);
        fieldKeys.put(PdfName.DV, one);
        fieldKeys.put(PdfName.DS, one);
        fieldKeys.put(PdfName.RV, one);
        fieldKeys.put(PdfName.OPT, one);
        fieldKeys.put(PdfName.MAXLEN, one);
        fieldKeys.put(PdfName.TI, one);
        fieldKeys.put(PdfName.I, one);
        fieldKeys.put(PdfName.LOCK, one);
        fieldKeys.put(PdfName.SV, one);
    }
}
