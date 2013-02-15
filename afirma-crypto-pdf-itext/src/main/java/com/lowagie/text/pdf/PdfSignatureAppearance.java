/*
 * $Id: PdfSignatureAppearance.java 3905 2009-04-24 10:40:24Z blowagie $
 *
 * Copyright 2004-2006 by Paulo Soares.
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

import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.security.PrivateKey;
import java.security.cert.CRL;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ExceptionConverter;
import com.lowagie.text.Font;
import com.lowagie.text.Image;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;

/**
 * This class takes care of the cryptographic options and appearances that form a signature.
 */
public class PdfSignatureAppearance {

    /**
     * The rendering mode is just the description
     */
    public static final int SignatureRenderDescription = 0;
    /**
     * The rendering mode is the name of the signer and the description
     */
    private static final int SignatureRenderNameAndDescription = 1;
    /**
     * The rendering mode is an image and the description
     */
    private static final int SignatureRenderGraphicAndDescription = 2;





    public static final int NOT_CERTIFIED = 0;




    private static final float TOP_SECTION = 0.3f;
    private static final float MARGIN = 2;
    private Rectangle rect;
    private Rectangle pageRect;
    private final PdfTemplate app[] = new PdfTemplate[5];
    private PdfTemplate frm;
    private final PdfStamperImp writer;
    private String layer2Text;
    private String reason;
    private String location;
    private Calendar signDate;
    private String provider;
    private int page = 1;
    private String fieldName;
    private PrivateKey privKey;
    private Certificate[] certChain;
    private CRL[] crlList;
    private PdfName filter;
    private boolean newField;
    private ByteBuffer sigout;
    private OutputStream originalout;
    private File tempFile;
    private PdfDictionary cryptoDictionary;
    private PdfStamper stamper;
    private boolean preClosed = false;
    private PdfSigGenericPKCS sigStandard;
    private int range[];
    private RandomAccessFile raf;
    private byte bout[];
    private int boutLen;
    private byte externalDigest[];
    private byte externalRSAdata[];
    private String digestEncryptionAlgorithm;
    private HashMap exclusionLocations;

    PdfSignatureAppearance(final PdfStamperImp writer, final Calendar globalDate) {
        this.writer = writer;
        this.signDate = globalDate!=null ? globalDate : new GregorianCalendar();
        this.fieldName = getNewSigName();
    }

    private int render = SignatureRenderDescription;

    /**
    * Gets the rendering mode for this signature.
    * @return the rendering mode for this signature
    */
    public int getRender() {
        return this.render;
    }

    /**
     * Sets the rendering mode for this signature.
     * The rendering modes can be the constants <CODE>SignatureRenderDescription</CODE>,
     * <CODE>SignatureRenderNameAndDescription</CODE> or <CODE>SignatureRenderGraphicAndDescription</CODE>.
     * The two last modes should be used with Acrobat 6 layer type.
     * @param render the render mode
     */
    public void setRender(final int render) {
        this.render = render;
    }

    private Image signatureGraphic = null;

    /**
    * Gets the Image object to render.
    * @return the image
    */
    public Image getSignatureGraphic() {
        return this.signatureGraphic;
    }

    /**
     * Sets the Image object to render when Render is set to <CODE>SignatureRenderGraphicAndDescription</CODE>
     * @param signatureGraphic image rendered. If <CODE>null</CODE> the mode is defaulted
     * to <CODE>SignatureRenderDescription</CODE>
     */
    public void setSignatureGraphic(final Image signatureGraphic) {
        this.signatureGraphic = signatureGraphic;
    }

    /**
     * Sets the signature text identifying the signer.
     * @param text the signature text identifying the signer. If <CODE>null</CODE> or not set
     * a standard description will be used
     */
    public void setLayer2Text(final String text) {
        this.layer2Text = text;
    }

    /**
     * Gets the signature text identifying the signer if set by setLayer2Text().
     * @return the signature text identifying the signer
     */
    public String getLayer2Text() {
        return this.layer2Text;
    }

    /**
     * Sets the text identifying the signature status.
     * @param text the text identifying the signature status. If <CODE>null</CODE> or not set
     * the description "Signature Not Verified" will be used
     */
    public void setLayer4Text(final String text) {
        this.layer4Text = text;
    }

    /**
     * Gets the text identifying the signature status if set by setLayer4Text().
     * @return the text identifying the signature status
     */
    public String getLayer4Text() {
        return this.layer4Text;
    }

    /**
     * Gets the rectangle representing the signature dimensions.
     * @return the rectangle representing the signature dimensions. It may be <CODE>null</CODE>
     * or have zero width or height for invisible signatures
     */
    public Rectangle getRect() {
        return this.rect;
    }

    /**
     * Gets the visibility status of the signature.
     * @return the visibility status of the signature
     */
    public boolean isInvisible() {
        return this.rect == null || this.rect.getWidth() == 0 || this.rect.getHeight() == 0;
    }

    /**
     * Sets the cryptographic parameters.
     * @param privKey the private key
     * @param certChain the certificate chain
     * @param crlList the certificate revocation list. It may be <CODE>null</CODE>
     * @param filter the crytographic filter type. It can be SELF_SIGNED, VERISIGN_SIGNED or WINCER_SIGNED
     */
    public void setCrypto(final PrivateKey privKey, final Certificate[] certChain, final CRL[] crlList, final PdfName filter) {
        this.privKey = privKey;
        this.certChain = certChain;
        this.crlList = crlList;
        this.filter = filter;
    }

    /**
     * Sets the signature to be visible. It creates a new visible signature field.
     * @param pageRect the position and dimension of the field in the page
     * @param page the page to place the field. The fist page is 1
     * @param fieldName the field name or <CODE>null</CODE> to generate automatically a new field name
     */
    public void setVisibleSignature(final Rectangle pageRect, final int page, final String fieldName) {
        if (fieldName != null) {
            if (fieldName.indexOf('.') >= 0) {
				throw new IllegalArgumentException("Field names cannot contain a dot."); //$NON-NLS-1$
			}
            final AcroFields af = this.writer.getAcroFields();
            final AcroFields.Item item = af.getFieldItem(fieldName);
            if (item != null) {
				throw new IllegalArgumentException("The field " + fieldName + " already exists."); //$NON-NLS-1$ //$NON-NLS-2$
			}
            this.fieldName = fieldName;
        }
        if (page < 1 || page > this.writer.reader.getNumberOfPages()) {
			throw new IllegalArgumentException("Invalid page number: " + page); //$NON-NLS-1$
		}
        this.pageRect = new Rectangle(pageRect);
        this.pageRect.normalize();
        this.rect = new Rectangle(this.pageRect.getWidth(), this.pageRect.getHeight());
        this.page = page;
        this.newField = true;
    }

    /**
     * Sets the signature to be visible. An empty signature field with the same name must already exist.
     * @param fieldName the existing empty signature field name
     */
    public void setVisibleSignature(final String fieldName) {
        final AcroFields af = this.writer.getAcroFields();
        final AcroFields.Item item = af.getFieldItem(fieldName);
        if (item == null) {
			throw new IllegalArgumentException("The field " + fieldName + " does not exist."); //$NON-NLS-1$ //$NON-NLS-2$
		}
        final PdfDictionary merged = item.getMerged(0);
        if (!PdfName.SIG.equals(PdfReader.getPdfObject(merged.get(PdfName.FT)))) {
			throw new IllegalArgumentException("The field " + fieldName + " is not a signature field."); //$NON-NLS-1$ //$NON-NLS-2$
		}
        this.fieldName = fieldName;
        final PdfArray r = merged.getAsArray(PdfName.RECT);
        final float llx = r.getAsNumber(0).floatValue();
        final float lly = r.getAsNumber(1).floatValue();
        final float urx = r.getAsNumber(2).floatValue();
        final float ury = r.getAsNumber(3).floatValue();
        this.pageRect = new Rectangle(llx, lly, urx, ury);
        this.pageRect.normalize();
        this.page = item.getPage(0).intValue();
        final int rotation = this.writer.reader.getPageRotation(this.page);
        final Rectangle pageSize = this.writer.reader.getPageSizeWithRotation(this.page);
        switch (rotation) {
            case 90:
                this.pageRect = new Rectangle(
                this.pageRect.getBottom(),
                pageSize.getTop() - this.pageRect.getLeft(),
                this.pageRect.getTop(),
                pageSize.getTop() - this.pageRect.getRight());
                break;
            case 180:
                this.pageRect = new Rectangle(
                pageSize.getRight() - this.pageRect.getLeft(),
                pageSize.getTop() - this.pageRect.getBottom(),
                pageSize.getRight() - this.pageRect.getRight(),
                pageSize.getTop() - this.pageRect.getTop());
                break;
            case 270:
                this.pageRect = new Rectangle(
                pageSize.getRight() - this.pageRect.getBottom(),
                this.pageRect.getLeft(),
                pageSize.getRight() - this.pageRect.getTop(),
                this.pageRect.getRight());
                break;
        }
        if (rotation != 0) {
			this.pageRect.normalize();
		}
        this.rect = new Rectangle(this.pageRect.getWidth(), this.pageRect.getHeight());
    }



    /**
     * Gets the template that aggregates all appearance layers. This corresponds to the /FRM resource.
     * <p>
     * Consult <A HREF="http://partners.adobe.com/asn/developer/pdfs/tn/PPKAppearances.pdf">PPKAppearances.pdf</A>
     * for further details.
     * @return the template that aggregates all appearance layers
     */
    public PdfTemplate getTopLayer() {
        if (this.frm == null) {
            this.frm = new PdfTemplate(this.writer);
            this.frm.setBoundingBox(this.rect);
            this.writer.addDirectTemplateSimple(this.frm, new PdfName("FRM")); //$NON-NLS-1$
        }
        return this.frm;
    }

    /**
     * Gets the main appearance layer.
     * <p>
     * Consult <A HREF="http://partners.adobe.com/asn/developer/pdfs/tn/PPKAppearances.pdf">PPKAppearances.pdf</A>
     * for further details.
     * @return the main appearance layer
     * @throws DocumentException on error
     */
    public PdfTemplate getAppearance() throws DocumentException {
        if (isInvisible()) {
            final PdfTemplate t = new PdfTemplate(this.writer);
            t.setBoundingBox(new Rectangle(0, 0));
            this.writer.addDirectTemplateSimple(t, null);
            return t;
        }
        if (this.app[0] == null) {
            final PdfTemplate t = this.app[0] = new PdfTemplate(this.writer);
            t.setBoundingBox(new Rectangle(100, 100));
            this.writer.addDirectTemplateSimple(t, new PdfName("n0")); //$NON-NLS-1$
            t.setLiteral("% DSBlank\n"); //$NON-NLS-1$
        }
        if (this.app[1] == null && !this.acro6Layers) {
            final PdfTemplate t = this.app[1] = new PdfTemplate(this.writer);
            t.setBoundingBox(new Rectangle(100, 100));
            this.writer.addDirectTemplateSimple(t, new PdfName("n1")); //$NON-NLS-1$
            t.setLiteral(questionMark);
        }
        if (this.app[2] == null) {
            String text;
            if (this.layer2Text == null) {
                final StringBuffer buf = new StringBuffer();
                buf.append("Digitally signed by ").append(PdfPKCS7.getSubjectFields((X509Certificate)this.certChain[0]).getField("CN")).append('\n'); //$NON-NLS-1$ //$NON-NLS-2$
                final SimpleDateFormat sd = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss z"); //$NON-NLS-1$
                buf.append("Date: ").append(sd.format(this.signDate.getTime())); //$NON-NLS-1$
                if (this.reason != null) {
					buf.append('\n').append("Reason: ").append(this.reason); //$NON-NLS-1$
				}
                if (this.location != null) {
					buf.append('\n').append("Location: ").append(this.location); //$NON-NLS-1$
				}
                text = buf.toString();
            } else {
				text = this.layer2Text;
			}
            final PdfTemplate t = this.app[2] = new PdfTemplate(this.writer);
            t.setBoundingBox(this.rect);
            this.writer.addDirectTemplateSimple(t, new PdfName("n2")); //$NON-NLS-1$
            if (this.image != null) {
                if (this.imageScale == 0) {
                    t.addImage(this.image, this.rect.getWidth(), 0, 0, this.rect.getHeight(), 0, 0);
                }
                else {
                    float usableScale = this.imageScale;
                    if (this.imageScale < 0) {
						usableScale = Math.min(this.rect.getWidth() / this.image.getWidth(), this.rect.getHeight() / this.image.getHeight());
					}
                    final float w = this.image.getWidth() * usableScale;
                    final float h = this.image.getHeight() * usableScale;
                    final float x = (this.rect.getWidth() - w) / 2;
                    final float y = (this.rect.getHeight() - h) / 2;
                    t.addImage(this.image, w, 0, 0, h, x, y);
                }
            }
            Font font;
            if (this.layer2Font == null) {
				font = new Font();
			} else {
				font = new Font(this.layer2Font);
			}
            float size = font.getSize();

            Rectangle dataRect = null;
            Rectangle signatureRect = null;

            if (this.render == SignatureRenderNameAndDescription ||
                this.render == SignatureRenderGraphicAndDescription && this.signatureGraphic != null) {
                // origin is the bottom-left
                signatureRect = new Rectangle(
                    MARGIN,
                    MARGIN,
                    this.rect.getWidth() / 2 - MARGIN,
                    this.rect.getHeight() - MARGIN);
                dataRect = new Rectangle(
                    this.rect.getWidth() / 2 +  MARGIN / 2,
                    MARGIN,
                    this.rect.getWidth() - MARGIN / 2,
                    this.rect.getHeight() - MARGIN);

                if (this.rect.getHeight() > this.rect.getWidth()) {
                    signatureRect = new Rectangle(
                        MARGIN,
                        this.rect.getHeight() / 2,
                        this.rect.getWidth() - MARGIN,
                        this.rect.getHeight());
                    dataRect = new Rectangle(
                        MARGIN,
                        MARGIN,
                        this.rect.getWidth() - MARGIN,
                        this.rect.getHeight() / 2 - MARGIN);
                }
            }
            else {
                dataRect = new Rectangle(
                    MARGIN,
                    MARGIN,
                    this.rect.getWidth() - MARGIN,
                    this.rect.getHeight() * (1 - TOP_SECTION) - MARGIN);
            }

            if (this.render == SignatureRenderNameAndDescription) {
                final String signedBy = PdfPKCS7.getSubjectFields((X509Certificate)this.certChain[0]).getField("CN"); //$NON-NLS-1$
                final Rectangle sr2 = new Rectangle(signatureRect.getWidth() - MARGIN, signatureRect.getHeight() - MARGIN );
                final float signedSize = fitText(font, signedBy, sr2, -1, this.runDirection);

                final ColumnText ct2 = new ColumnText(t);
                ct2.setRunDirection(this.runDirection);
                ct2.setSimpleColumn(new Phrase(signedBy, font), signatureRect.getLeft(), signatureRect.getBottom(), signatureRect.getRight(), signatureRect.getTop(), signedSize, Element.ALIGN_LEFT);

                ct2.go();
            }
            else if (this.render == SignatureRenderGraphicAndDescription) {
                final ColumnText ct2 = new ColumnText(t);
                ct2.setRunDirection(this.runDirection);
                ct2.setSimpleColumn(signatureRect.getLeft(), signatureRect.getBottom(), signatureRect.getRight(), signatureRect.getTop(), 0, Element.ALIGN_RIGHT);

                final Image im = Image.getInstance(this.signatureGraphic);
                im.scaleToFit(signatureRect.getWidth(), signatureRect.getHeight());

                final Paragraph p = new Paragraph();
                // must calculate the point to draw from to make image appear in middle of column
                float x = 0;
                // experimentation found this magic number to counteract Adobe's signature graphic, which
                // offsets the y co-ordinate by 15 units
                float y = -im.getScaledHeight() + 15;

                x = x + (signatureRect.getWidth() - im.getScaledWidth()) / 2;
                y = y - (signatureRect.getHeight() - im.getScaledHeight()) / 2;
                p.add(new Chunk(im, x + (signatureRect.getWidth() - im.getScaledWidth()) / 2, y, false));
                ct2.addElement(p);
                ct2.go();
            }

            if (size <= 0) {
                final Rectangle sr = new Rectangle(dataRect.getWidth(), dataRect.getHeight());
                size = fitText(font, text, sr, 12, this.runDirection);
            }
            final ColumnText ct = new ColumnText(t);
            ct.setRunDirection(this.runDirection);
            ct.setSimpleColumn(new Phrase(text, font), dataRect.getLeft(), dataRect.getBottom(), dataRect.getRight(), dataRect.getTop(), size, Element.ALIGN_LEFT);
            ct.go();
        }
        if (this.app[3] == null && !this.acro6Layers) {
            final PdfTemplate t = this.app[3] = new PdfTemplate(this.writer);
            t.setBoundingBox(new Rectangle(100, 100));
            this.writer.addDirectTemplateSimple(t, new PdfName("n3")); //$NON-NLS-1$
            t.setLiteral("% DSBlank\n"); //$NON-NLS-1$
        }
        if (this.app[4] == null && !this.acro6Layers) {
            final PdfTemplate t = this.app[4] = new PdfTemplate(this.writer);
            t.setBoundingBox(new Rectangle(0, this.rect.getHeight() * (1 - TOP_SECTION), this.rect.getRight(), this.rect.getTop()));
            this.writer.addDirectTemplateSimple(t, new PdfName("n4")); //$NON-NLS-1$
            Font font;
            if (this.layer2Font == null) {
				font = new Font();
			} else {
				font = new Font(this.layer2Font);
			}
            float size = font.getSize();
            String text = "Signature Not Verified"; //$NON-NLS-1$
            if (this.layer4Text != null) {
				text = this.layer4Text;
			}
            final Rectangle sr = new Rectangle(this.rect.getWidth() - 2 * MARGIN, this.rect.getHeight() * TOP_SECTION - 2 * MARGIN);
            size = fitText(font, text, sr, 15, this.runDirection);
            final ColumnText ct = new ColumnText(t);
            ct.setRunDirection(this.runDirection);
            ct.setSimpleColumn(new Phrase(text, font), MARGIN, 0, this.rect.getWidth() - MARGIN, this.rect.getHeight() - MARGIN, size, Element.ALIGN_LEFT);
            ct.go();
        }
        final int rotation = this.writer.reader.getPageRotation(this.page);
        Rectangle rotated = new Rectangle(this.rect);
        int n = rotation;
        while (n > 0) {
            rotated = rotated.rotate();
            n -= 90;
        }
        if (this.frm == null) {
            this.frm = new PdfTemplate(this.writer);
            this.frm.setBoundingBox(rotated);
            this.writer.addDirectTemplateSimple(this.frm, new PdfName("FRM")); //$NON-NLS-1$
            float scale = Math.min(this.rect.getWidth(), this.rect.getHeight()) * 0.9f;
            final float x = (this.rect.getWidth() - scale) / 2;
            final float y = (this.rect.getHeight() - scale) / 2;
            scale /= 100;
            if (rotation == 90) {
				this.frm.concatCTM(0, 1, -1, 0, this.rect.getHeight(), 0);
			} else if (rotation == 180) {
				this.frm.concatCTM(-1, 0, 0, -1, this.rect.getWidth(), this.rect.getHeight());
			} else if (rotation == 270) {
				this.frm.concatCTM(0, -1, 1, 0, 0, this.rect.getWidth());
			}
            this.frm.addTemplate(this.app[0], 0, 0);
            if (!this.acro6Layers) {
				this.frm.addTemplate(this.app[1], scale, 0, 0, scale, x, y);
			}
            this.frm.addTemplate(this.app[2], 0, 0);
            if (!this.acro6Layers) {
                this.frm.addTemplate(this.app[3], scale, 0, 0, scale, x, y);
                this.frm.addTemplate(this.app[4], 0, 0);
            }
        }
        final PdfTemplate napp = new PdfTemplate(this.writer);
        napp.setBoundingBox(rotated);
        this.writer.addDirectTemplateSimple(napp, null);
        napp.addTemplate(this.frm, 0, 0);
        return napp;
    }

    /**
     * Fits the text to some rectangle adjusting the font size as needed.
     * @param font the font to use
     * @param text the text
     * @param rect the rectangle where the text must fit
     * @param maxFontSize the maximum font size
     * @param runDirection the run direction
     * @return the calculated font size that makes the text fit
     */
    private static float fitText(final Font font, final String text, final Rectangle rect, float maxFontSize, final int runDirection) {
        try {
            ColumnText ct = null;
            int status = 0;
            if (maxFontSize <= 0) {
                int cr = 0;
                int lf = 0;
                final char t[] = text.toCharArray();
                for (final char element : t) {
                    if (element == '\n') {
						++lf;
					} else if (element == '\r') {
						++cr;
					}
                }
                final int minLines = Math.max(cr, lf) + 1;
                maxFontSize = Math.abs(rect.getHeight()) / minLines - 0.001f;
            }
            font.setSize(maxFontSize);
            final Phrase ph = new Phrase(text, font);
            ct = new ColumnText(null);
            ct.setSimpleColumn(ph, rect.getLeft(), rect.getBottom(), rect.getRight(), rect.getTop(), maxFontSize, Element.ALIGN_LEFT);
            ct.setRunDirection(runDirection);
            status = ct.go(true);
            if ((status & ColumnText.NO_MORE_TEXT) != 0) {
				return maxFontSize;
			}
            final float precision = 0.1f;
            float min = 0;
            float max = maxFontSize;
            float size = maxFontSize;
            for (int k = 0; k < 50; ++k) { //just in case it doesn't converge
                size = (min + max) / 2;
                ct = new ColumnText(null);
                font.setSize(size);
                ct.setSimpleColumn(new Phrase(text, font), rect.getLeft(), rect.getBottom(), rect.getRight(), rect.getTop(), size, Element.ALIGN_LEFT);
                ct.setRunDirection(runDirection);
                status = ct.go(true);
                if ((status & ColumnText.NO_MORE_TEXT) != 0) {
                    if (max - min < size * precision) {
						return size;
					}
                    min = size;
                } else {
					max = size;
				}
            }
            return size;
        }
        catch (final Exception e) {
            throw new ExceptionConverter(e);
        }
    }



    /**
     * Gets the signing reason.
     * @return the signing reason
     */
    public String getReason() {
        return this.reason;
    }

    /**
     * Sets the signing reason.
     * @param reason the signing reason
     */
    public void setReason(final String reason) {
        this.reason = reason;
    }

    /**
     * Gets the signing location.
     * @return the signing location
     */
    public String getLocation() {
        return this.location;
    }

    /**
     * Sets the signing location.
     * @param location the signing location
     */
    public void setLocation(final String location) {
        this.location = location;
    }

    /**
     * Returns the Cryptographic Service Provider that will sign the document.
     * @return provider the name of the provider, for example "SUN",
     * or <code>null</code> to use the default provider.
     */
    public String getProvider() {
        return this.provider;
    }

    /**
     * Sets the Cryptographic Service Provider that will sign the document.
     *
     * @param provider the name of the provider, for example "SUN", or
     * <code>null</code> to use the default provider.
     */
    public void setProvider(final String provider) {
        this.provider = provider;
    }

    /**
     * Gets the private key.
     * @return the private key
     */
    public java.security.PrivateKey getPrivKey() {
        return this.privKey;
    }

    /**
     * Gets the certificate chain.
     * @return the certificate chain
     */
    public java.security.cert.Certificate[] getCertChain() {
        return this.certChain;
    }

    /**
     * Gets the certificate revocation list.
     * @return the certificate revocation list
     */
    public java.security.cert.CRL[] getCrlList() {
        return this.crlList;
    }

    /**
     * Gets the filter used to sign the document.
     * @return the filter used to sign the document
     */
    public com.lowagie.text.pdf.PdfName getFilter() {
        return this.filter;
    }

    /**
     * Checks if a new field was created.
     * @return <CODE>true</CODE> if a new field was created, <CODE>false</CODE> if signing
     * an existing field or if the signature is invisible
     */
    public boolean isNewField() {
        return this.newField;
    }

    /**
     * Gets the page number of the field.
     * @return the page number of the field
     */
    public int getPage() {
        return this.page;
    }

    /**
     * Gets the field name.
     * @return the field name
     */
    public java.lang.String getFieldName() {
        return this.fieldName;
    }

    /**
     * Gets the rectangle that represent the position and dimension of the signature in the page.
     * @return the rectangle that represent the position and dimension of the signature in the page
     */
    public com.lowagie.text.Rectangle getPageRect() {
        return this.pageRect;
    }

    /**
     * Gets the signature date.
     * @return the signature date
     */
    public java.util.Calendar getSignDate() {
        return this.signDate;
    }

    /**
     * Sets the signature date.
     * @param signDate the signature date
     */
    public void setSignDate(final java.util.Calendar signDate) {
        this.signDate = signDate;
    }



    void setSigout(final com.lowagie.text.pdf.ByteBuffer sigout) {
        this.sigout = sigout;
    }



    void setOriginalout(final java.io.OutputStream originalout) {
        this.originalout = originalout;
    }

    /**
     * Gets the temporary file.
     * @return the temporary file or <CODE>null</CODE> is the document is created in memory
     */
    public java.io.File getTempFile() {
        return this.tempFile;
    }

    void setTempFile(final java.io.File tempFile) {
        this.tempFile = tempFile;
    }

    /**
     * Gets a new signature fied name that doesn't clash with any existing name.
     * @return a new signature fied name
     */
    public String getNewSigName() {
        final AcroFields af = this.writer.getAcroFields();
        String name = "Signature"; //$NON-NLS-1$
        int step = 0;
        boolean found = false;
        while (!found) {
            ++step;
            String n1 = name + step;
            if (af.getFieldItem(n1) != null) {
				continue;
			}
            n1 += "."; //$NON-NLS-1$
            found = true;
            for (final Iterator it = af.getFields().keySet().iterator(); it.hasNext();) {
                final String fn = (String)it.next();
                if (fn.startsWith(n1)) {
                    found = false;
                    break;
                }
            }
        }
        name += step;
        return name;
    }

    /**
     * This is the first method to be called when using external signatures. The general sequence is:
     * preClose(), getDocumentBytes() and close().
     * <p>
     * If calling preClose() <B>dont't</B> call PdfStamper.close().
     * <p>
     * No external signatures are allowed if this method is called.
     * @param globalDate
     * @throws IOException on error
     * @throws DocumentException on error
     */
    void preClose(final Calendar globalDate) throws IOException, DocumentException {
        preClose(null, globalDate);
    }

    /**
     * This is the first method to be called when using external signatures. The general sequence is:
     * preClose(), getDocumentBytes() and close().
     * <p>
     * If calling preClose() <B>dont't</B> call PdfStamper.close().
     * <p>
     * If using an external signature <CODE>exclusionSizes</CODE> must contain at least
     * the <CODE>PdfName.CONTENTS</CODE> key with the size that it will take in the
     * document. Note that due to the hex string coding this size should be
     * byte_size*2+2.
     * @param exclusionSizes a <CODE>HashMap</CODE> with names and sizes to be excluded in the signature
     * calculation. The key is a <CODE>PdfName</CODE> and the value an
     * <CODE>Integer</CODE>. At least the <CODE>PdfName.CONTENTS</CODE> must be present
     * @throws IOException on error
     * @throws DocumentException on error
     */
    public void preClose(final HashMap exclusionSizes) throws IOException, DocumentException {
    	preClose(exclusionSizes, null);
    }

    /**
     * This is the first method to be called when using external signatures. The general sequence is:
     * preClose(), getDocumentBytes() and close().
     * <p>
     * If calling preClose() <B>dont't</B> call PdfStamper.close().
     * <p>
     * If using an external signature <CODE>exclusionSizes</CODE> must contain at least
     * the <CODE>PdfName.CONTENTS</CODE> key with the size that it will take in the
     * document. Note that due to the hex string coding this size should be
     * byte_size*2+2.
     * @param exclusionSizes a <CODE>HashMap</CODE> with names and sizes to be excluded in the signature
     * calculation. The key is a <CODE>PdfName</CODE> and the value an
     * <CODE>Integer</CODE>. At least the <CODE>PdfName.CONTENTS</CODE> must be present
     * @param globalDate
     * @throws IOException on error
     * @throws DocumentException on error
     */
    public void preClose(final HashMap exclusionSizes, final Calendar globalDate) throws IOException, DocumentException {
        if (this.preClosed) {
            throw new DocumentException("Document already pre closed."); //$NON-NLS-1$
        }
        this.preClosed = true;
        final AcroFields af = this.writer.getAcroFields();
        final String name = getFieldName();
        final boolean fieldExists = !(isInvisible() || isNewField());
        final PdfIndirectReference refSig = this.writer.getPdfIndirectReference();
        this.writer.setSigFlags(3);
        if (fieldExists) {
            final PdfDictionary widget = af.getFieldItem(name).getWidget(0);
            this.writer.markUsed(widget);
            widget.put(PdfName.P, this.writer.getPageReference(getPage()));
            widget.put(PdfName.V, refSig);
            final PdfObject obj = PdfReader.getPdfObjectRelease(widget.get(PdfName.F));
            int flags = 0;
            if (obj != null && obj.isNumber()) {
				flags = ((PdfNumber)obj).intValue();
			}
            flags |= PdfAnnotation.FLAGS_LOCKED;
            widget.put(PdfName.F, new PdfNumber(flags));
            final PdfDictionary ap = new PdfDictionary();
            ap.put(PdfName.N, getAppearance().getIndirectReference());
            widget.put(PdfName.AP, ap);
        }
        else {
            final PdfFormField sigField = PdfFormField.createSignature(this.writer);
            sigField.setFieldName(name);
            sigField.put(PdfName.V, refSig);
            sigField.setFlags(PdfAnnotation.FLAGS_PRINT | PdfAnnotation.FLAGS_LOCKED);

            final int pagen = getPage();
            if (!isInvisible()) {
				sigField.setWidget(getPageRect(), null);
			} else {
				sigField.setWidget(new Rectangle(0, 0), null);
			}
            sigField.setAppearance(PdfAnnotation.APPEARANCE_NORMAL, getAppearance());
            sigField.setPage(pagen);
            this.writer.addAnnotation(sigField, pagen);
        }

        this.exclusionLocations = new HashMap();
        if (this.cryptoDictionary == null) {
            if (PdfName.ADOBE_PPKLITE.equals(getFilter())) {
				this.sigStandard = new PdfSigGenericPKCS.PPKLite(getProvider());
			} else if (PdfName.ADOBE_PPKMS.equals(getFilter())) {
				this.sigStandard = new PdfSigGenericPKCS.PPKMS(getProvider());
			} else if (PdfName.VERISIGN_PPKVS.equals(getFilter())) {
				this.sigStandard = new PdfSigGenericPKCS.VeriSign(getProvider());
			} else {
				throw new IllegalArgumentException("Unknown filter: " + getFilter()); //$NON-NLS-1$
			}
            this.sigStandard.setExternalDigest(this.externalDigest, this.externalRSAdata, this.digestEncryptionAlgorithm);
            if (getReason() != null) {
				this.sigStandard.setReason(getReason());
			}
            if (getLocation() != null) {
				this.sigStandard.setLocation(getLocation());
			}
            if (getContact() != null) {
				this.sigStandard.setContact(getContact());
			}
            this.sigStandard.put(PdfName.M, new PdfDate(getSignDate()));
            this.sigStandard.setSignInfo(getPrivKey(), getCertChain(), getCrlList());
            final PdfString contents = (PdfString)this.sigStandard.get(PdfName.CONTENTS);
            PdfLiteral lit = new PdfLiteral((contents.toString().length() + (PdfName.ADOBE_PPKLITE.equals(getFilter())?0:64)) * 2 + 2);
            this.exclusionLocations.put(PdfName.CONTENTS, lit);
            this.sigStandard.put(PdfName.CONTENTS, lit);
            lit = new PdfLiteral(80);
            this.exclusionLocations.put(PdfName.BYTERANGE, lit);
            this.sigStandard.put(PdfName.BYTERANGE, lit);
            if (this.certificationLevel > 0) {
                addDocMDP(this.sigStandard);
            }
            if (this.signatureEvent != null) {
				this.signatureEvent.getSignatureDictionary(this.sigStandard);
			}
            this.writer.addToBody(this.sigStandard, refSig, false);
        }
        else {
            PdfLiteral lit = new PdfLiteral(80);
            this.exclusionLocations.put(PdfName.BYTERANGE, lit);
            this.cryptoDictionary.put(PdfName.BYTERANGE, lit);
            for (final Iterator it = exclusionSizes.entrySet().iterator(); it.hasNext();) {
                final Map.Entry entry = (Map.Entry)it.next();
                final PdfName key = (PdfName)entry.getKey();
                final Integer v = (Integer)entry.getValue();
                lit = new PdfLiteral(v.intValue());
                this.exclusionLocations.put(key, lit);
                this.cryptoDictionary.put(key, lit);
            }
            if (this.certificationLevel > 0) {
				addDocMDP(this.cryptoDictionary);
			}
            if (this.signatureEvent != null) {
				this.signatureEvent.getSignatureDictionary(this.cryptoDictionary);
			}
            this.writer.addToBody(this.cryptoDictionary, refSig, false);
        }
        if (this.certificationLevel > 0) {
          // add DocMDP entry to root
             final PdfDictionary docmdp = new PdfDictionary();
             docmdp.put(new PdfName("DocMDP"), refSig); //$NON-NLS-1$
             this.writer.reader.getCatalog().put(new PdfName("Perms"), docmdp); //$NON-NLS-1$
        }

        this.writer.close(this.stamper.getMoreInfo(), globalDate!=null ? globalDate : new GregorianCalendar());

        this.range = new int[this.exclusionLocations.size() * 2];
        final int byteRangePosition = ((PdfLiteral)this.exclusionLocations.get(PdfName.BYTERANGE)).getPosition();
        this.exclusionLocations.remove(PdfName.BYTERANGE);
        int idx = 1;
        for (final Iterator it = this.exclusionLocations.values().iterator(); it.hasNext();) {
            final PdfLiteral lit = (PdfLiteral)it.next();
            final int n = lit.getPosition();
            this.range[idx++] = n;
            this.range[idx++] = lit.getPosLength() + n;
        }
        Arrays.sort(this.range, 1, this.range.length - 1);
        for (int k = 3; k < this.range.length - 2; k += 2) {
			this.range[k] -= this.range[k - 1];
		}

        if (this.tempFile == null) {
            this.bout = this.sigout.getBuffer();
            this.boutLen = this.sigout.size();
            this.range[this.range.length - 1] = this.boutLen - this.range[this.range.length - 2];
            final ByteBuffer bf = new ByteBuffer();
            bf.append('[');
            for (final int element : this.range) {
				bf.append(element).append(' ');
			}
            bf.append(']');
            System.arraycopy(bf.getBuffer(), 0, this.bout, byteRangePosition, bf.size());
        }
        else {
            try {
                this.raf = new RandomAccessFile(this.tempFile, "rw"); //$NON-NLS-1$
                final int boutLen = (int)this.raf.length();
                this.range[this.range.length - 1] = boutLen - this.range[this.range.length - 2];
                final ByteBuffer bf = new ByteBuffer();
                bf.append('[');
                for (final int element : this.range) {
					bf.append(element).append(' ');
				}
                bf.append(']');
                this.raf.seek(byteRangePosition);
                this.raf.write(bf.getBuffer(), 0, bf.size());
            }
            catch (final IOException e) {
                try{this.raf.close();}catch(final Exception ee){}
                try{this.tempFile.delete();}catch(final Exception ee){}
                throw e;
            }
        }
    }

    /**
     * This is the last method to be called when using external signatures. The general sequence is:
     * preClose(), getDocumentBytes() and close().
     * <p>
     * <CODE>update</CODE> is a <CODE>PdfDictionary</CODE> that must have exactly the
     * same keys as the ones provided in {@link #preClose(HashMap)}.
     * @param update a <CODE>PdfDictionary</CODE> with the key/value that will fill the holes defined
     * in {@link #preClose(HashMap)}
     * @throws DocumentException on error
     * @throws IOException on error
     */
    public void close(final PdfDictionary update) throws IOException, DocumentException {
        try {
            if (!this.preClosed) {
				throw new DocumentException("preClose() must be called first."); //$NON-NLS-1$
			}
            final ByteBuffer bf = new ByteBuffer();
            for (final Object element : update.getKeys()) {
                final PdfName key = (PdfName)element;
                final PdfObject obj = update.get(key);
                final PdfLiteral lit = (PdfLiteral)this.exclusionLocations.get(key);
                if (lit == null) {
					throw new IllegalArgumentException("The key " + key.toString() + " didn't reserve space in preClose()."); //$NON-NLS-1$ //$NON-NLS-2$
				}
                bf.reset();
                obj.toPdf(null, bf);
                if (bf.size() > lit.getPosLength()) {
					throw new IllegalArgumentException("The key " + key.toString() + " is too big. Is " + bf.size() + ", reserved " + lit.getPosLength()); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				}
                if (this.tempFile == null) {
					System.arraycopy(bf.getBuffer(), 0, this.bout, lit.getPosition(), bf.size());
				} else {
                    this.raf.seek(lit.getPosition());
                    this.raf.write(bf.getBuffer(), 0, bf.size());
                }
            }
            if (update.size() != this.exclusionLocations.size()) {
				throw new IllegalArgumentException("The update dictionary has less keys than required."); //$NON-NLS-1$
			}
            if (this.tempFile == null) {
                this.originalout.write(this.bout, 0, this.boutLen);
            }
            else {
                if (this.originalout != null) {
                    this.raf.seek(0);
                    int length = (int)this.raf.length();
                    final byte buf[] = new byte[8192];
                    while (length > 0) {
                        final int r = this.raf.read(buf, 0, Math.min(buf.length, length));
                        if (r < 0) {
							throw new EOFException("Unexpected EOF"); //$NON-NLS-1$
						}
                        this.originalout.write(buf, 0, r);
                        length -= r;
                    }
                }
            }
        }
        finally {
            if (this.tempFile != null) {
                try{this.raf.close();}catch(final Exception ee){}
                if (this.originalout != null) {
					try{this.tempFile.delete();}catch(final Exception ee){}
				}
            }
            if (this.originalout != null) {
				try{this.originalout.close();}catch(final Exception e){}
			}
        }
    }

    private void addDocMDP(final PdfDictionary crypto) {
        final PdfDictionary reference = new PdfDictionary();
        final PdfDictionary transformParams = new PdfDictionary();
        transformParams.put(PdfName.P, new PdfNumber(this.certificationLevel));
        transformParams.put(PdfName.V, new PdfName("1.2")); //$NON-NLS-1$
        transformParams.put(PdfName.TYPE, PdfName.TRANSFORMPARAMS);
        reference.put(PdfName.TRANSFORMMETHOD, PdfName.DOCMDP);
        reference.put(PdfName.TYPE, PdfName.SIGREF);
        reference.put(PdfName.TRANSFORMPARAMS, transformParams);
        reference.put(new PdfName("DigestValue"), new PdfString("aa")); //$NON-NLS-1$ //$NON-NLS-2$
        final PdfArray loc = new PdfArray();
        loc.add(new PdfNumber(0));
        loc.add(new PdfNumber(0));
        reference.put(new PdfName("DigestLocation"), loc); //$NON-NLS-1$
        reference.put(new PdfName("DigestMethod"), new PdfName("MD5")); //$NON-NLS-1$ //$NON-NLS-2$
        reference.put(PdfName.DATA, this.writer.reader.getTrailer().get(PdfName.ROOT));
        final PdfArray types = new PdfArray();
        types.add(reference);
        crypto.put(PdfName.REFERENCE, types);
    }

    /**
     * Gets the document bytes that are hashable when using external signatures. The general sequence is:
     * preClose(), getRangeStream() and close().
     * <p>
     * @return the document bytes that are hashable
     */
    public InputStream getRangeStream() {
        return new PdfSignatureAppearance.RangeStream(this.raf, this.bout, this.range);
    }

    /**
     * Gets the user made signature dictionary. This is the dictionary at the /V key.
     * @return the user made signature dictionary
     */
    public com.lowagie.text.pdf.PdfDictionary getCryptoDictionary() {
        return this.cryptoDictionary;
    }

    /**
     * Sets a user made signature dictionary. This is the dictionary at the /V key.
     * @param cryptoDictionary a user made signature dictionary
     */
    public void setCryptoDictionary(final com.lowagie.text.pdf.PdfDictionary cryptoDictionary) {
        this.cryptoDictionary = cryptoDictionary;
    }

    /**
     * Gets the <CODE>PdfStamper</CODE> associated with this instance.
     * @return the <CODE>PdfStamper</CODE> associated with this instance
     */
    public com.lowagie.text.pdf.PdfStamper getStamper() {
        return this.stamper;
    }

    void setStamper(final com.lowagie.text.pdf.PdfStamper stamper) {
        this.stamper = stamper;
    }

    /**
     * Checks if the document is in the process of closing.
     * @return <CODE>true</CODE> if the document is in the process of closing,
     * <CODE>false</CODE> otherwise
     */
    public boolean isPreClosed() {
        return this.preClosed;
    }

    /**
     * Gets the instance of the standard signature dictionary. This instance
     * is only available after pre close.
     * <p>
     * The main use is to insert external signatures.
     * @return the instance of the standard signature dictionary
     */
    public com.lowagie.text.pdf.PdfSigGenericPKCS getSigStandard() {
        return this.sigStandard;
    }

    /**
     * Gets the signing contact.
     * @return the signing contact
     */
    public String getContact() {
        return this.contact;
    }

    /**
     * Sets the signing contact.
     * @param contact the signing contact
     */
    public void setContact(final String contact) {
        this.contact = contact;
    }

    /**
     * Gets the n2 and n4 layer font.
     * @return the n2 and n4 layer font
     */
    public Font getLayer2Font() {
        return this.layer2Font;
    }

    /**
     * Sets the n2 and n4 layer font. If the font size is zero, auto-fit will be used.
     * @param layer2Font the n2 and n4 font
     */
    public void setLayer2Font(final Font layer2Font) {
        this.layer2Font = layer2Font;
    }

    /**
     * Gets the Acrobat 6.0 layer mode.
     * @return the Acrobat 6.0 layer mode
     */
    public boolean isAcro6Layers() {
        return this.acro6Layers;
    }

    /**
     * Acrobat 6.0 and higher recommends that only layer n2 and n4 be present. This method sets that mode.
     * @param acro6Layers if <code>true</code> only the layers n2 and n4 will be present
     */
    public void setAcro6Layers(final boolean acro6Layers) {
        this.acro6Layers = acro6Layers;
    }

    /** Sets the run direction in the n2 and n4 layer.
     * @param runDirection the run direction
     */
    public void setRunDirection(final int runDirection) {
        if (runDirection < PdfWriter.RUN_DIRECTION_DEFAULT || runDirection > PdfWriter.RUN_DIRECTION_RTL) {
			throw new RuntimeException("Invalid run direction: " + runDirection); //$NON-NLS-1$
		}
        this.runDirection = runDirection;
    }

    /** Gets the run direction.
     * @return the run direction
     */
    public int getRunDirection() {
        return this.runDirection;
    }

    /**
     * Getter for property signatureEvent.
     * @return Value of property signatureEvent.
     */
    public SignatureEvent getSignatureEvent() {
        return this.signatureEvent;
    }

    /**
     * Sets the signature event to allow modification of the signature dictionary.
     * @param signatureEvent the signature event
     */
    public void setSignatureEvent(final SignatureEvent signatureEvent) {
        this.signatureEvent = signatureEvent;
    }

    /**
     * Gets the background image for the layer 2.
     * @return the background image for the layer 2
     */
    public Image getImage() {
        return this.image;
    }

    /**
     * Sets the background image for the layer 2.
     * @param image the background image for the layer 2
     */
    public void setImage(final Image image) {
        this.image = image;
    }

    /**
     * Gets the scaling to be applied to the background image.
     * @return the scaling to be applied to the background image
     */
    public float getImageScale() {
        return this.imageScale;
    }

    /**
     * Sets the scaling to be applied to the background image. If it's zero the image
     * will fully fill the rectangle. If it's less than zero the image will fill the rectangle but
     * will keep the proportions. If it's greater than zero that scaling will be applied.
     * In any of the cases the image will always be centered. It's zero by default.
     * @param imageScale the scaling to be applied to the background image
     */
    public void setImageScale(final float imageScale) {
        this.imageScale = imageScale;
    }

    /**
     * Commands to draw a yellow question mark in a stream content
     */
    private static final String questionMark =
        "% DSUnknown\n" + //$NON-NLS-1$
        "q\n" + //$NON-NLS-1$
        "1 G\n" + //$NON-NLS-1$
        "1 g\n" + //$NON-NLS-1$
        "0.1 0 0 0.1 9 0 cm\n" + //$NON-NLS-1$
        "0 J 0 j 4 M []0 d\n" + //$NON-NLS-1$
        "1 i \n" + //$NON-NLS-1$
        "0 g\n" + //$NON-NLS-1$
        "313 292 m\n" + //$NON-NLS-1$
        "313 404 325 453 432 529 c\n" + //$NON-NLS-1$
        "478 561 504 597 504 645 c\n" + //$NON-NLS-1$
        "504 736 440 760 391 760 c\n" + //$NON-NLS-1$
        "286 760 271 681 265 626 c\n" + //$NON-NLS-1$
        "265 625 l\n" + //$NON-NLS-1$
        "100 625 l\n" + //$NON-NLS-1$
        "100 828 253 898 381 898 c\n" + //$NON-NLS-1$
        "451 898 679 878 679 650 c\n" + //$NON-NLS-1$
        "679 555 628 499 538 435 c\n" + //$NON-NLS-1$
        "488 399 467 376 467 292 c\n" + //$NON-NLS-1$
        "313 292 l\n" + //$NON-NLS-1$
        "h\n" + //$NON-NLS-1$
        "308 214 170 -164 re\n" + //$NON-NLS-1$
        "f\n" + //$NON-NLS-1$
        "0.44 G\n" + //$NON-NLS-1$
        "1.2 w\n" + //$NON-NLS-1$
        "1 1 0.4 rg\n" + //$NON-NLS-1$
        "287 318 m\n" + //$NON-NLS-1$
        "287 430 299 479 406 555 c\n" + //$NON-NLS-1$
        "451 587 478 623 478 671 c\n" + //$NON-NLS-1$
        "478 762 414 786 365 786 c\n" + //$NON-NLS-1$
        "260 786 245 707 239 652 c\n" + //$NON-NLS-1$
        "239 651 l\n" + //$NON-NLS-1$
        "74 651 l\n" + //$NON-NLS-1$
        "74 854 227 924 355 924 c\n" + //$NON-NLS-1$
        "425 924 653 904 653 676 c\n" + //$NON-NLS-1$
        "653 581 602 525 512 461 c\n" + //$NON-NLS-1$
        "462 425 441 402 441 318 c\n" + //$NON-NLS-1$
        "287 318 l\n" + //$NON-NLS-1$
        "h\n" + //$NON-NLS-1$
        "282 240 170 -164 re\n" + //$NON-NLS-1$
        "B\n" + //$NON-NLS-1$
        "Q\n"; //$NON-NLS-1$

    /**
     * Holds value of property contact.
     */
    private String contact;

    /**
     * Holds value of property layer2Font.
     */
    private Font layer2Font;

    /**
     * Holds value of property layer4Text.
     */
    private String layer4Text;

    /**
     * Holds value of property acro6Layers.
     */
    private boolean acro6Layers;

    /**
     * Holds value of property runDirection.
     */
    private int runDirection = PdfWriter.RUN_DIRECTION_NO_BIDI;

    /**
     * Holds value of property signatureEvent.
     */
    private SignatureEvent signatureEvent;

    /**
     * Holds value of property image.
     */
    private Image image;

    /**
     * Holds value of property imageScale.
     */
    private float imageScale;

    /**
     *
     */
    private static class RangeStream extends InputStream {
        private final byte b[] = new byte[1];
        private final RandomAccessFile raf;
        private final byte bout[];
        private final int range[];
        private int rangePosition = 0;

        private RangeStream(final RandomAccessFile raf, final byte bout[], final int range[]) {
            this.raf = raf;
            this.bout = bout;
            this.range = range;
        }

        /**
         * @see java.io.InputStream#read()
         */
        @Override
		public int read() throws IOException {
            final int n = read(this.b);
            if (n != 1) {
				return -1;
			}
            return this.b[0] & 0xff;
        }

        /**
         * @see java.io.InputStream#read(byte[], int, int)
         */
        @Override
		public int read(final byte[] b, final int off, final int len) throws IOException {
            if (b == null) {
                throw new NullPointerException();
            } else if (off < 0 || off > b.length || len < 0 ||
            off + len > b.length || off + len < 0) {
                throw new IndexOutOfBoundsException();
            } else if (len == 0) {
                return 0;
            }
            if (this.rangePosition >= this.range[this.range.length - 2] + this.range[this.range.length - 1]) {
                return -1;
            }
            for (int k = 0; k < this.range.length; k += 2) {
                final int start = this.range[k];
                final int end = start + this.range[k + 1];
                if (this.rangePosition < start) {
					this.rangePosition = start;
				}
                if (this.rangePosition >= start && this.rangePosition < end) {
                    final int lenf = Math.min(len, end - this.rangePosition);
                    if (this.raf == null) {
						System.arraycopy(this.bout, this.rangePosition, b, off, lenf);
					} else {
                        this.raf.seek(this.rangePosition);
                        this.raf.readFully(b, off, lenf);
                    }
                    this.rangePosition += lenf;
                    return lenf;
                }
            }
            return -1;
        }
    }

    /**
     * An interface to retrieve the signature dictionary for modification.
     */
    private interface SignatureEvent {
        /**
         * Allows modification of the signature dictionary.
         * @param sig the signature dictionary
         */
        public void getSignatureDictionary(PdfDictionary sig);
    }

    private int certificationLevel = NOT_CERTIFIED;

    /**
     * Gets the certified status of this document.
     * @return the certified status
     */
    public int getCertificationLevel() {
        return this.certificationLevel;
    }

    /**
     * Sets the document type to certified instead of simply signed.
     * @param certificationLevel the values can be: <code>NOT_CERTIFIED</code>, <code>CERTIFIED_NO_CHANGES_ALLOWED</code>,
     * <code>CERTIFIED_FORM_FILLING</code> and <code>CERTIFIED_FORM_FILLING_AND_ANNOTATIONS</code>
     */
    public void setCertificationLevel(final int certificationLevel) {
        this.certificationLevel = certificationLevel;
    }
}