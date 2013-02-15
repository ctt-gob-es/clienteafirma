/*
 * Copyright 1999-2004 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.lowagie.text.pdf.hyphenation;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Hashtable;

import com.lowagie.text.pdf.BaseFont;

/**
 * This class is the main entry point to the hyphenation package.
 * You can use only the static methods or create an instance.
 *
 * @author Carlos Villegas <cav@uniscope.co.jp>
 */
public class Hyphenator {

    /** TODO: Don't use statics */
    private static Hashtable hyphenTrees = new Hashtable();

    private HyphenationTree hyphenTree = null;
    private int remainCharCount = 2;
    private int pushCharCount = 2;
    private static final String defaultHyphLocation = "com/lowagie/text/pdf/hyphenation/hyph/";

    /** Holds value of property hyphenDir. */
    private static String hyphenDir = "";

    /**
     * @param lang
     * @param country
     * @param leftMin
     * @param rightMin
     */
    public Hyphenator(final String lang, final String country, final int leftMin,
                      final int rightMin) {
        this.hyphenTree = getHyphenationTree(lang, country);
        this.remainCharCount = leftMin;
        this.pushCharCount = rightMin;
    }

    /**
     * @param lang
     * @param country
     * @return the hyphenation tree
     */
    private static HyphenationTree getHyphenationTree(final String lang,
            final String country) {
        String key = lang;
        // check whether the country code has been used
        if (country != null && !country.equals("none")) {
            key += "_" + country;
        }
            // first try to find it in the cache
        if (hyphenTrees.containsKey(key)) {
            return (HyphenationTree)hyphenTrees.get(key);
        }
        if (hyphenTrees.containsKey(lang)) {
            return (HyphenationTree)hyphenTrees.get(lang);
        }

        HyphenationTree hTree = getResourceHyphenationTree(key);
        if (hTree == null) {
			hTree = getFileHyphenationTree(key);
		}
        // put it into the pattern cache
        if (hTree != null) {
            hyphenTrees.put(key, hTree);
        }
        return hTree;
    }

    /**
     * @param key
     * @return a hyphenation tree
     */
    private static HyphenationTree getResourceHyphenationTree(final String key) {
        try {
            InputStream stream = BaseFont.getResourceStream(defaultHyphLocation + key + ".xml");
            if (stream == null && key.length() > 2) {
				stream = BaseFont.getResourceStream(defaultHyphLocation + key.substring(0, 2) + ".xml");
			}
            if (stream == null) {
				return null;
			}
            final HyphenationTree hTree = new HyphenationTree();
            hTree.loadSimplePatterns(stream);
            return hTree;
        }
        catch (final Exception e) {
            return null;
        }
    }

    /**
     * @param key
     * @return a hyphenation tree
     */
    private static HyphenationTree getFileHyphenationTree(final String key) {
        try {
            if (hyphenDir == null) {
				return null;
			}
            InputStream stream = null;
            File hyphenFile = new File(hyphenDir, key + ".xml");
            if (hyphenFile.canRead()) {
				stream = new FileInputStream(hyphenFile);
			}
            if (stream == null && key.length() > 2) {
                hyphenFile = new File(hyphenDir, key.substring(0, 2) + ".xml");
                if (hyphenFile.canRead()) {
					stream = new FileInputStream(hyphenFile);
				}
            }
            if (stream == null) {
				return null;
			}
            final HyphenationTree hTree = new HyphenationTree();
            hTree.loadSimplePatterns(stream);
            return hTree;
        }
        catch (final Exception e) {
            return null;
        }
    }





    /**
     * @param min
     */
    public void setMinRemainCharCount(final int min) {
        this.remainCharCount = min;
    }

    /**
     * @param min
     */
    public void setMinPushCharCount(final int min) {
        this.pushCharCount = min;
    }





    /**
     * @param word
     * @return a hyphenation object
     */
    public Hyphenation hyphenate(final String word) {
        if (this.hyphenTree == null) {
            return null;
        }
        return this.hyphenTree.hyphenate(word, this.remainCharCount, this.pushCharCount);
    }





}
