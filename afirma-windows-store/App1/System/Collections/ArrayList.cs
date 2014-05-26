using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace System.Collections
{
    public class ArrayList: IList
    {

        IList lista = new List<object>();
        public int Count = 0;

        public Object this[int index]
        {
            get
            {
                return lista[index];
            }
            set
            {
                lista[index] = index;
            }
        }
        

        public ArrayList()
        {
            lista = new List<object>();
    
        }
        public ArrayList(int capacidad)
        {
            lista = new List<object>(capacidad);

        }

        public ArrayList(IList lista)
        {
            this.lista = lista;
            Count = lista.Count;
        }

        public ArrayList(ICollection lista)
        {
            this.lista = (IList)lista;
            Count = lista.Count;
        }

        public void Add(object elemento)
        {
            lista.Add(elemento);
            Count = lista.Count;
        }

        public object Reverse()
        {
            return null;
        }



        int IList.Add(object value)
        {
            int ret = lista.Add(value);
            Count = lista.Count;
            return ret;
        }

        public void Clear()
        {
            lista.Clear();
        }

        public bool Contains(object value)
        {
            return lista.Contains(value);
        }

        public int IndexOf(object value)
        {
            return lista.IndexOf(value);
        }

        public void Insert(int index, object value)
        {
            lista.Insert(index, value);
        }

        public bool IsFixedSize
        {
            get { return lista.IsFixedSize; }
        }

        public bool IsReadOnly
        {
            get { throw new NotImplementedException(); }
        }

        public void Remove(object value)
        {
            lista.Remove(value);
        }

        public void RemoveAt(int index)
        {
            lista.RemoveAt(index);
        }

        public void CopyTo(Array array, int index)
        {
            lista.CopyTo(array, index);
        }

        int ICollection.Count
        {
            get { return lista.Count; }
        }

        public bool IsSynchronized
        {
            get { return lista.IsSynchronized; }
        }

        public object SyncRoot
        {
            get { return lista.SyncRoot; }
        }

        public IEnumerator GetEnumerator()
        {
            return lista.GetEnumerator();
        }
    }
}
