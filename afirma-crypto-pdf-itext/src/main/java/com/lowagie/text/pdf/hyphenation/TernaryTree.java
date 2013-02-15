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

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Stack;

/**
 * <h2>Ternary Search Tree.</h2>
 *
 * <p>A ternary search tree is a hybrid between a binary tree and
 * a digital search tree (trie). Keys are limited to strings.
 * A data value of type char is stored in each leaf node.
 * It can be used as an index (or pointer) to the data.
 * Branches that only contain one key are compressed to one node
 * by storing a pointer to the trailer substring of the key.
 * This class is intended to serve as base class or helper class
 * to implement Dictionary collections or the like. Ternary trees
 * have some nice properties as the following: the tree can be
 * traversed in sorted order, partial matches (wildcard) can be
 * implemented, retrieval of all keys within a given distance
 * from the target, etc. The storage requirements are higher than
 * a binary tree but a lot less than a trie. Performance is
 * comparable with a hash table, sometimes it outperforms a hash
 * function (most of the time can determine a miss faster than a hash).</p>
 *
 * <p>The main purpose of this java port is to serve as a base for
 * implementing TeX's hyphenation algorithm (see The TeXBook,
 * appendix H). Each language requires from 5000 to 15000 hyphenation
 * patterns which will be keys in this tree. The strings patterns
 * are usually small (from 2 to 5 characters), but each char in the
 * tree is stored in a node. Thus memory usage is the main concern.
 * We will sacrifice 'elegance' to keep memory requirements to the
 * minimum. Using java's char type as pointer (yes, I know pointer
 * it is a forbidden word in java) we can keep the size of the node
 * to be just 8 bytes (3 pointers and the data char). This gives
 * room for about 65000 nodes. In my tests the English patterns
 * took 7694 nodes and the German patterns 10055 nodes,
 * so I think we are safe.</p>
 *
 * <p>All said, this is a map with strings as keys and char as value.
 * Pretty limited!. It can be extended to a general map by
 * using the string representation of an object and using the
 * char value as an index to an array that contains the object
 * values.</p>
 *
 * @author cav@uniscope.co.jp
 */

class TernaryTree implements Cloneable, Serializable {

    /**
     * We use 4 arrays to represent a node. I guess I should have created
     * a proper node class, but somehow Knuth's pascal code made me forget
     * we now have a portable language with virtual memory management and
     * automatic garbage collection! And now is kind of late, furthermore,
     * if it ain't broken, don't fix it.
     */

    private static final long serialVersionUID = 5313366505322983510L;

	/**
     * Pointer to low branch and to rest of the key when it is
     * stored directly in this node, we don't have unions in java!
     */
    protected char[] lo;

    /**
     * Pointer to high branch.
     */
    protected char[] hi;

    /**
     * Pointer to equal branch and to data when this node is a string terminator.
     */
    protected char[] eq;

    /**
     * <P>The character stored in this node: splitchar.
     * Two special values are reserved:</P>
     * <ul><li>0x0000 as string terminator</li>
     * <li>0xFFFF to indicate that the branch starting at
     * this node is compressed</li></ul>
     * <p>This shouldn't be a problem if we give the usual semantics to
     * strings since 0xFFFF is guaranteed not to be an Unicode character.</p>
     */
    protected char[] sc;

    /**
     * This vector holds the trailing of the keys when the branch is compressed.
     */
    protected CharVector kv;

    protected char root;
    private char freenode;
    private int length;    // number of items in tree

    private static final int BLOCK_SIZE = 2048;    // allocation size for arrays

    TernaryTree() {
        init();
    }

    private void init() {
        this.root = 0;
        this.freenode = 1;
        this.length = 0;
        this.lo = new char[BLOCK_SIZE];
        this.hi = new char[BLOCK_SIZE];
        this.eq = new char[BLOCK_SIZE];
        this.sc = new char[BLOCK_SIZE];
        this.kv = new CharVector();
    }

    /**
     * Branches are initially compressed, needing
     * one node per key plus the size of the string
     * key. They are decompressed as needed when
     * another key with same prefix
     * is inserted. This saves a lot of space,
     * specially for long keys.
     */
    void insert(final String key, final char val) {
        // make sure we have enough room in the arrays
        int len = key.length()
                  + 1;    // maximum number of nodes that may be generated
        if (this.freenode + len > this.eq.length) {
            redimNodeArrays(this.eq.length + BLOCK_SIZE);
        }
        final char strkey[] = new char[len--];
        key.getChars(0, len, strkey, 0);
        strkey[len] = 0;
        this.root = insert(this.root, strkey, 0, val);
    }

    void insert(final char[] key, final int start, final char val) {
        final int len = strlen(key) + 1;
        if (this.freenode + len > this.eq.length) {
            redimNodeArrays(this.eq.length + BLOCK_SIZE);
        }
        this.root = insert(this.root, key, start, val);
    }

    /**
     * The actual insertion function, recursive version.
     */
    private char insert(char p, final char[] key, final int start, final char val) {
        final int len = strlen(key, start);
        if (p == 0) {
            // this means there is no branch, this node will start a new branch.
            // Instead of doing that, we store the key somewhere else and create
            // only one node with a pointer to the key
            p = this.freenode++;
            this.eq[p] = val;           // holds data
            this.length++;
            this.hi[p] = 0;
            if (len > 0) {
                this.sc[p] = 0xFFFF;    // indicates branch is compressed
                this.lo[p] = (char)this.kv.alloc(len
                                       + 1);    // use 'lo' to hold pointer to key
                strcpy(this.kv.getArray(), this.lo[p], key, start);
            } else {
                this.sc[p] = 0;
                this.lo[p] = 0;
            }
            return p;
        }

        if (this.sc[p] == 0xFFFF) {
            // branch is compressed: need to decompress
            // this will generate garbage in the external key array
            // but we can do some garbage collection later
            final char pp = this.freenode++;
            this.lo[pp] = this.lo[p];    // previous pointer to key
            this.eq[pp] = this.eq[p];    // previous pointer to data
            this.lo[p] = 0;
            if (len > 0) {
                this.sc[p] = this.kv.get(this.lo[pp]);
                this.eq[p] = pp;
                this.lo[pp]++;
                if (this.kv.get(this.lo[pp]) == 0) {
                    // key completely decompressed leaving garbage in key array
                    this.lo[pp] = 0;
                    this.sc[pp] = 0;
                    this.hi[pp] = 0;
                } else {
                    // we only got first char of key, rest is still there
                    this.sc[pp] = 0xFFFF;
                }
            } else {
                // In this case we can save a node by swapping the new node
                // with the compressed node
                this.sc[pp] = 0xFFFF;
                this.hi[p] = pp;
                this.sc[p] = 0;
                this.eq[p] = val;
                this.length++;
                return p;
            }
        }
        final char s = key[start];
        if (s < this.sc[p]) {
            this.lo[p] = insert(this.lo[p], key, start, val);
        } else if (s == this.sc[p]) {
            if (s != 0) {
                this.eq[p] = insert(this.eq[p], key, start + 1, val);
            } else {
                // key already in tree, overwrite data
                this.eq[p] = val;
            }
        } else {
            this.hi[p] = insert(this.hi[p], key, start, val);
        }
        return p;
    }

    /**
     * Compares 2 null terminated char arrays
     */
    private static int strcmp(final char[] a, int startA, final char[] b, int startB) {
        for (; a[startA] == b[startB]; startA++, startB++) {
            if (a[startA] == 0) {
                return 0;
            }
        }
        return a[startA] - b[startB];
    }



    private static void strcpy(final char[] dst, int di, final char[] src, int si) {
        while (src[si] != 0) {
            dst[di++] = src[si++];
        }
        dst[di] = 0;
    }

    private static int strlen(final char[] a, final int start) {
        int len = 0;
        for (int i = start; i < a.length && a[i] != 0; i++) {
            len++;
        }
        return len;
    }

    private static int strlen(final char[] a) {
        return strlen(a, 0);
    }

    int find(final String key) {
        final int len = key.length();
        final char strkey[] = new char[len + 1];
        key.getChars(0, len, strkey, 0);
        strkey[len] = 0;

        return find(strkey, 0);
    }

    int find(final char[] key, final int start) {
        int d;
        char p = this.root;
        int i = start;
        char c;

        while (p != 0) {
            if (this.sc[p] == 0xFFFF) {
                if (strcmp(key, i, this.kv.getArray(), this.lo[p]) == 0) {
                    return this.eq[p];
                } else {
                    return -1;
            }
            }
            c = key[i];
            d = c - this.sc[p];
            if (d == 0) {
                if (c == 0) {
                    return this.eq[p];
                }
                i++;
                p = this.eq[p];
            } else if (d < 0) {
                p = this.lo[p];
            } else {
                p = this.hi[p];
        }
        }
        return -1;
    }



    // redimension the arrays
    private void redimNodeArrays(final int newsize) {
        final int len = newsize < this.lo.length ? newsize : this.lo.length;
        char[] na = new char[newsize];
        System.arraycopy(this.lo, 0, na, 0, len);
        this.lo = na;
        na = new char[newsize];
        System.arraycopy(this.hi, 0, na, 0, len);
        this.hi = na;
        na = new char[newsize];
        System.arraycopy(this.eq, 0, na, 0, len);
        this.eq = na;
        na = new char[newsize];
        System.arraycopy(this.sc, 0, na, 0, len);
        this.sc = na;
    }



    @Override
	public Object clone() {
        final TernaryTree t = new TernaryTree();
        t.lo = this.lo.clone();
        t.hi = this.hi.clone();
        t.eq = this.eq.clone();
        t.sc = this.sc.clone();
        t.kv = (CharVector)this.kv.clone();
        t.root = this.root;
        t.freenode = this.freenode;
        t.length = this.length;

        return t;
    }

    /**
     * Recursively insert the median first and then the median of the
     * lower and upper halves, and so on in order to get a balanced
     * tree. The array of keys is assumed to be sorted in ascending
     * order.
     */
    private void insertBalanced(final String[] k, final char[] v, final int offset, final int n) {
        int m;
        if (n < 1) {
            return;
        }
        m = n >> 1;

        insert(k[m + offset], v[m + offset]);
        insertBalanced(k, v, offset, m);

        insertBalanced(k, v, offset + m + 1, n - m - 1);
    }


    /**
     * Balance the tree for best search performance
     */
    private void balance() {
        // System.out.print("Before root splitchar = "); System.out.println(sc[root]);

        int i = 0;
		final int n = this.length;
        final String[] k = new String[n];
        final char[] v = new char[n];
        final Iterator iter = new Iterator();
        while (iter.hasMoreElements()) {
            v[i] = iter.getValue();
            k[i++] = (String)iter.nextElement();
        }
        init();
        insertBalanced(k, v, 0, n);

        // With uniform letter distribution sc[root] should be around 'm'
        // System.out.print("After root splitchar = "); System.out.println(sc[root]);
    }

    /**
     * Each node stores a character (splitchar) which is part of
     * some key(s). In a compressed branch (one that only contain
     * a single string key) the trailer of the key which is not
     * already in nodes is stored  externally in the kv array.
     * As items are inserted, key substrings decrease.
     * Some substrings may completely  disappear when the whole
     * branch is totally decompressed.
     * The tree is traversed to find the key substrings actually
     * used. In addition, duplicate substrings are removed using
     * a map (implemented with a TernaryTree!).
     *
     */
    void trimToSize() {
        // first balance the tree for best performance
        balance();

        // redimension the node arrays
        redimNodeArrays(this.freenode);

        // ok, compact kv array
        final CharVector kx = new CharVector();
        kx.alloc(1);
        final TernaryTree map = new TernaryTree();
        compact(kx, map, this.root);
        this.kv = kx;
        this.kv.trimToSize();
    }

    private void compact(final CharVector kx, final TernaryTree map, final char p) {
        int k;
        if (p == 0) {
            return;
        }
        if (this.sc[p] == 0xFFFF) {
            k = map.find(this.kv.getArray(), this.lo[p]);
            if (k < 0) {
                k = kx.alloc(strlen(this.kv.getArray(), this.lo[p]) + 1);
                strcpy(kx.getArray(), k, this.kv.getArray(), this.lo[p]);
                map.insert(kx.getArray(), k, (char)k);
            }
            this.lo[p] = (char)k;
        } else {
            compact(kx, map, this.lo[p]);
            if (this.sc[p] != 0) {
                compact(kx, map, this.eq[p]);
            }
            compact(kx, map, this.hi[p]);
        }
    }




    private class Iterator implements Enumeration {

        /**
         * current node index
         */
        private int cur;

        /**
         * current key
         */
        private String curkey;

        private class Item implements Cloneable {
            char parent;
            char child;

            public Item() {
                this.parent = 0;
                this.child = 0;
            }

            public Item(final char p, final char c) {
                this.parent = p;
                this.child = c;
            }

            @Override
			public Object clone() {
                return new Item(this.parent, this.child);
            }

        }

        /**
         * Node stack
         */
        private final Stack ns;

        /**
         * key stack implemented with a StringBuffer
         */
        private final StringBuffer ks;

        public Iterator() {
            this.cur = -1;
            this.ns = new Stack();
            this.ks = new StringBuffer();
            rewind();
        }

        private void rewind() {
            this.ns.removeAllElements();
            this.ks.setLength(0);
            this.cur = TernaryTree.this.root;
            run();
        }

        @Override
		public Object nextElement() {
            final String res = this.curkey;
            this.cur = up();
            run();
            return res;
        }

        public char getValue() {
            if (this.cur >= 0) {
                return TernaryTree.this.eq[this.cur];
            }
            return 0;
        }

        @Override
		public boolean hasMoreElements() {
            return this.cur != -1;
        }

        /**
         * traverse upwards
         */
        private int up() {
            Item i = new Item();
            int res = 0;

            if (this.ns.empty()) {
                return -1;
            }

            if (this.cur != 0 && TernaryTree.this.sc[this.cur] == 0) {
                return TernaryTree.this.lo[this.cur];
            }

            boolean climb = true;

            while (climb) {
                i = (Item)this.ns.pop();
                i.child++;
                switch (i.child) {
                case 1:
                    if (TernaryTree.this.sc[i.parent] != 0) {
                        res = TernaryTree.this.eq[i.parent];
                        this.ns.push(i.clone());
                        this.ks.append(TernaryTree.this.sc[i.parent]);
                    } else {
                        i.child++;
                        this.ns.push(i.clone());
                        res = TernaryTree.this.hi[i.parent];
                    }
                    climb = false;
                    break;

                case 2:
                    res = TernaryTree.this.hi[i.parent];
                    this.ns.push(i.clone());
                    if (this.ks.length() > 0) {
                        this.ks.setLength(this.ks.length() - 1);    // pop
                    }
                    climb = false;
                    break;

                default:
                    if (this.ns.empty()) {
                        return -1;
                    }
                    climb = true;
                    break;
                }
            }
            return res;
        }

        /**
         * traverse the tree to find next key
         */
        private int run() {
            if (this.cur == -1) {
                return -1;
            }

            boolean leaf = false;
            while (true) {
                // first go down on low branch until leaf or compressed branch
                while (this.cur != 0) {
                    if (TernaryTree.this.sc[this.cur] == 0xFFFF) {
                        leaf = true;
                        break;
                    }
                    this.ns.push(new Item((char)this.cur, '\u0000'));
                    if (TernaryTree.this.sc[this.cur] == 0) {
                        leaf = true;
                        break;
                    }
                    this.cur = TernaryTree.this.lo[this.cur];
                }
                if (leaf) {
                    break;
                }
                    // nothing found, go up one node and try again
                this.cur = up();
                if (this.cur == -1) {
                    return -1;
                }
            }
            // The current node should be a data node and
            // the key should be in the key stack (at least partially)
            final StringBuffer buf = new StringBuffer(this.ks.toString());
            if (TernaryTree.this.sc[this.cur] == 0xFFFF) {
                int p = TernaryTree.this.lo[this.cur];
                while (TernaryTree.this.kv.get(p) != 0) {
                    buf.append(TernaryTree.this.kv.get(p++));
            }
            }
            this.curkey = buf.toString();
            return 0;
        }

    }

    public void printStats() {
        System.out.println("Number of keys = " + Integer.toString(this.length));
        System.out.println("Node count = " + Integer.toString(this.freenode));
        // System.out.println("Array length = " + Integer.toString(eq.length));
        System.out.println("Key Array length = "
                           + Integer.toString(this.kv.length()));

        /*
         * for(int i=0; i<kv.length(); i++)
         * if ( kv.get(i) != 0 )
         * System.out.print(kv.get(i));
         * else
         * System.out.println("");
         * System.out.println("Keys:");
         * for(Enumeration enum = keys(); enum.hasMoreElements(); )
         * System.out.println(enum.nextElement());
         */

    }

/*    public static void main(String[] args) throws Exception {
        TernaryTree tt = new TernaryTree();
        tt.insert("Carlos", 'C');
        tt.insert("Car", 'r');
        tt.insert("palos", 'l');
        tt.insert("pa", 'p');
        tt.trimToSize();
        System.out.println((char)tt.find("Car"));
        System.out.println((char)tt.find("Carlos"));
        System.out.println((char)tt.find("alto"));
        tt.printStats();
    }*/

}

