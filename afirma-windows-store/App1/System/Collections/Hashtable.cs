using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace System.Collections
{

    public class Hashtable: IDictionary
    {

        // Create a new dictionary of strings, with string keys,  
        // and access it through the IDictionary generic interface.
        IDictionary diccionario = new Dictionary<object, object>();

        public Hashtable()
        {
            diccionario = new Dictionary<object, object>();
        }
        public Hashtable(int capacidad)
        {
            diccionario = new Dictionary<object, object>(capacidad);
        }

        public Hashtable(IDictionary dicc)
        {
            diccionario = dicc;
        }

        public void Add(object k, object v){
            diccionario.Add(k,v);
        }



        public void Clear()
        {
            diccionario.Clear();
        }

        public bool Contains(object key)
        {
            return diccionario.Contains(key);
        }

        public IDictionaryEnumerator GetEnumerator()
        {
            return diccionario.GetEnumerator();
        }

        public bool IsFixedSize
        {
            get { return diccionario.IsFixedSize; }
        }

        public bool IsReadOnly
        {
            get { return diccionario.IsReadOnly; }
        }

        public ICollection Keys
        {
            get { return diccionario.Keys; }
        }

        public void Remove(object key)
        {
            diccionario.Remove(key);
        }

        public ICollection Values
        {
            get { return diccionario.Values; }
        }

        public object this[object key]
        {
            get
            {
                return diccionario[key];
            }
            set
            {
                diccionario[key] = value;
            }
        }

        public void CopyTo(Array array, int index)
        {
            diccionario.CopyTo(array, index);
        }

        public int Count
        {
            get { return diccionario.Count; }
        }

        public bool IsSynchronized
        {
            get { return diccionario.IsSynchronized; }
        }

        public object SyncRoot
        {
            get { return diccionario.SyncRoot; }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return diccionario.GetEnumerator();
        }
    }
}
