/*
 * This class is based on org.apache.IntHashMap.commons.lang
 * http://jakarta.apache.org/commons/lang/xref/org/apache/commons/lang/IntHashMap.html
 * It was adapted by Bruno Lowagie for use in iText,
 * reusing methods that were written by Paulo Soares.
 * Instead of being a hashtable that stores objects with an int as key,
 * it stores int values with an int as key.
 *
 * This is the original license of the original class IntHashMap:
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Note: originally released under the GNU LGPL v2.1,
 * but rereleased by the original author under the ASF license (above).
 */

package com.lowagie.text.pdf;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

/***
 * <p>A hash map that uses primitive ints for the key rather than objects.</p>
 *
 * <p>Note that this class is for internal optimization purposes only, and may
 * not be supported in future releases of Jakarta Commons Lang.  Utilities of
 * this sort may be included in future releases of Jakarta Commons Collections.</p>
 *
 * @author Justin Couch
 * @author Alex Chaffee (alex@apache.org)
 * @author Stephen Colebourne
 * @author Bruno Lowagie (change Objects as keys into int values)
 * @author Paulo Soares (added extra methods)
 */
class IntHashtable implements Cloneable {

    /***
     * The hash table data.
     */
    private transient Entry table[];

    /***
     * The total number of entries in the hash table.
     */
    private transient int count;

    /***
     * The table is rehashed when its size exceeds this threshold.  (The
     * value of this field is (int)(capacity * loadFactor).)
     *
     * @serial
     */
    private int threshold;

    /***
     * The load factor for the hashtable.
     *
     * @serial
     */
    private final float loadFactor;

    /***
     * <p>Constructs a new, empty hashtable with a default capacity and load
     * factor, which is <code>20</code> and <code>0.75</code> respectively.</p>
     */
    public IntHashtable() {
        this(150, 0.75f);
    }



    /***
     * <p>Constructs a new, empty hashtable with the specified initial
     * capacity and the specified load factor.</p>
     *
     * @param initialCapacity the initial capacity of the hashtable.
     * @param loadFactor the load factor of the hashtable.
     * @throws IllegalArgumentException  if the initial capacity is less
     *             than zero, or if the load factor is nonpositive.
     */
    private IntHashtable(int initialCapacity, final float loadFactor) {
        super();
        if (initialCapacity < 0) {
            throw new IllegalArgumentException("Illegal Capacity: " + initialCapacity);
        }
        if (loadFactor <= 0) {
            throw new IllegalArgumentException("Illegal Load: " + loadFactor);
        }
        if (initialCapacity == 0) {
            initialCapacity = 1;
        }
        this.loadFactor = loadFactor;
        this.table = new Entry[initialCapacity];
        this.threshold = (int) (initialCapacity * loadFactor);
    }

    /***
     * <p>Returns the number of keys in this hashtable.</p>
     *
     * @return  the number of keys in this hashtable.
     */
    int size() {
        return this.count;
    }

    /***
     * <p>Tests if this hashtable maps no keys to values.</p>
     *
     * @return  <code>true</code> if this hashtable maps no keys to values;
     *          <code>false</code> otherwise.
     */
    public boolean isEmpty() {
        return this.count == 0;
    }





    /***
     * <p>Tests if the specified int is a key in this hashtable.</p>
     *
     * @param  key  possible key.
     * @return <code>true</code> if and only if the specified int is a
     *    key in this hashtable, as determined by the <tt>equals</tt>
     *    method; <code>false</code> otherwise.
     * @see #contains(int)
     */
    boolean containsKey(final int key) {
        final Entry tab[] = this.table;
        final int hash = key;
        final int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index]; e != null; e = e.next) {
            if (e.hash == hash && e.key == key) {
                return true;
            }
        }
        return false;
    }

    /***
     * <p>Returns the value to which the specified key is mapped in this map.</p>
     *
     * @param   key   a key in the hashtable.
     * @return  the value to which the key is mapped in this hashtable;
     *          <code>null</code> if the key is not mapped to any value in
     *          this hashtable.
     * @see     #put(int, int)
     */
    int get(final int key) {
        final Entry tab[] = this.table;
        final int hash = key;
        final int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index]; e != null; e = e.next) {
            if (e.hash == hash && e.key == key) {
                return e.value;
            }
        }
        return 0;
    }

    /***
     * <p>Increases the capacity of and internally reorganizes this
     * hashtable, in order to accommodate and access its entries more
     * efficiently.</p>
     *
     * <p>This method is called automatically when the number of keys
     * in the hashtable exceeds this hashtable's capacity and load
     * factor.</p>
     */
    private void rehash() {
        final int oldCapacity = this.table.length;
        final Entry oldMap[] = this.table;

        final int newCapacity = oldCapacity * 2 + 1;
        final Entry newMap[] = new Entry[newCapacity];

        this.threshold = (int) (newCapacity * this.loadFactor);
        this.table = newMap;

        for (int i = oldCapacity; i-- > 0;) {
            for (Entry old = oldMap[i]; old != null;) {
                final Entry e = old;
                old = old.next;

                final int index = (e.hash & 0x7FFFFFFF) % newCapacity;
                e.next = newMap[index];
                newMap[index] = e;
            }
        }
    }

    /***
     * <p>Maps the specified <code>key</code> to the specified
     * <code>value</code> in this hashtable. The key cannot be
     * <code>null</code>. </p>
     *
     * <p>The value can be retrieved by calling the <code>get</code> method
     * with a key that is equal to the original key.</p>
     *
     * @param key     the hashtable key.
     * @param value   the value.
     * @return the previous value of the specified key in this hashtable,
     *         or <code>null</code> if it did not have one.
     * @throws  NullPointerException  if the key is <code>null</code>.
     * @see     #get(int)
     */
    int put(final int key, final int value) {
        // Makes sure the key is not already in the hashtable.
        Entry tab[] = this.table;
        final int hash = key;
        int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index]; e != null; e = e.next) {
            if (e.hash == hash && e.key == key) {
                final int old = e.value;
                e.value = value;
                return old;
            }
        }

        if (this.count >= this.threshold) {
            // Rehash the table if the threshold is exceeded
            rehash();

            tab = this.table;
            index = (hash & 0x7FFFFFFF) % tab.length;
        }

         // Creates the new entry.
         final Entry e = new Entry(hash, key, value, tab[index]);
         tab[index] = e;
         this.count++;
         return 0;
    }

    /***
     * <p>Removes the key (and its corresponding value) from this
     * hashtable.</p>
     *
     * <p>This method does nothing if the key is not present in the
     * hashtable.</p>
     *
     * @param   key   the key that needs to be removed.
     * @return  the value to which the key had been mapped in this hashtable,
     *          or <code>null</code> if the key did not have a mapping.
     */
    int remove(final int key) {
        final Entry tab[] = this.table;
        final int hash = key;
        final int index = (hash & 0x7FFFFFFF) % tab.length;
        for (Entry e = tab[index], prev = null; e != null; prev = e, e = e.next) {
            if (e.hash == hash && e.key == key) {
                if (prev != null) {
                    prev.next = e.next;
                } else {
                    tab[index] = e.next;
                }
                this.count--;
                final int oldValue = e.value;
                e.value = 0;
                return oldValue;
            }
        }
        return 0;
    }

    /***
     * <p>Clears this hashtable so that it contains no keys.</p>
     */
    void clear() {
    	final Entry tab[] = this.table;
        for (int index = tab.length; --index >= 0;) {
            tab[index] = null;
        }
        this.count = 0;
	}

    /***
     * <p>Innerclass that acts as a datastructure to create a new entry in the
     * table.</p>
     */
    private static class Entry {
        private final int hash;
        private final int key;
        private int value;
        private Entry next;

        /***
         * <p>Create a new entry with the given values.</p>
         *
         * @param hash The code used to hash the int with
         * @param key The key used to enter this in the table
         * @param value The value for this key
         * @param next A reference to the next entry in the table
         */
        private Entry(final int hash, final int key, final int value, final Entry next) {
            this.hash = hash;
            this.key = key;
            this.value = value;
            this.next = next;
        }

        // extra methods for inner class Entry by Paulo
        public int getKey() {
        	return this.key;
        }
        public int getValue() {
        	return this.value;
        }
        @Override
		protected Object clone() {
        	final Entry entry = new Entry(this.hash, this.key, this.value, this.next != null ? (Entry)this.next.clone() : null);
        	return entry;
        }
    }

    // extra inner class by Paulo
    private static class IntHashtableIterator implements Iterator {
        private int index;
        private final Entry table[];
        private Entry entry;

        private IntHashtableIterator(final Entry table[]) {
        	this.table = table;
        	this.index = table.length;
        }
        @Override
		public boolean hasNext() {
        	if (this.entry != null) {
        		return true;
        	}
        	while (this.index-- > 0) {
        	    if ((this.entry = this.table[this.index]) != null) {
        	        return true;
        	    }
        	}
        	return false;
        }

        @Override
		public Object next() {
            if (this.entry == null) {
                while (this.index-- > 0 && (this.entry = this.table[this.index]) == null) {
					;
				}
            }
            if (this.entry != null) {
            	final Entry e = this.entry;
            	this.entry = e.next;
            	return e;
            }
        	throw new NoSuchElementException("IntHashtableIterator");
        }
        @Override
		public void remove() {
        	throw new UnsupportedOperationException("remove() not supported.");
        }
    }

// extra methods by Paulo Soares:

    public Iterator getEntryIterator() {
        return new IntHashtableIterator(this.table);
    }

    int[] toOrderedKeys() {
    	final int res[] = getKeys();
    	Arrays.sort(res);
    	return res;
    }

    public int[] getKeys() {
    	final int res[] = new int[this.count];
    	int ptr = 0;
    	int index = this.table.length;
    	Entry entry = null;
    	while (true) {
    		if (entry == null) {
				while (index-- > 0 && (entry = this.table[index]) == null) {
					;
				}
			}
    		if (entry == null) {
				break;
			}
    		final Entry e = entry;
    		entry = e.next;
    		res[ptr++] = e.key;
    	}
    	return res;
    }

    public int getOneKey() {
    	if (this.count == 0) {
			return 0;
		}
    	int index = this.table.length;
    	Entry entry = null;
    	while (index-- > 0 && (entry = this.table[index]) == null) {
			;
		}
    	if (entry == null) {
			return 0;
		}
    	return entry.key;
    }

    @Override
	public Object clone() {
    	try {
    		final IntHashtable t = (IntHashtable)super.clone();
    		t.table = new Entry[this.table.length];
    		for (int i = this.table.length ; i-- > 0 ; ) {
    			t.table[i] = this.table[i] != null
    			? (Entry)this.table[i].clone() : null;
    		}
    		return t;
    	} catch (final CloneNotSupportedException e) {
    		// this shouldn't happen, since we are Cloneable
    		throw new InternalError();
    	}
    }
}