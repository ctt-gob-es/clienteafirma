using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AfirmaWMetro.Utils
{
    using System;
    using Windows.Storage;
    using Windows.UI.Xaml.Data;
    using Windows.UI.Xaml.Media;
    using Windows.UI.Xaml.Media.Imaging;
    using System.Collections.ObjectModel;

    public class KeyStoreFile
    {

        private Uri _baseUri = new Uri("ms-appx:///");

        public KeyStoreFile(string name, string date, string extension, string imagePath)
        {
            this._Name = name;
            this._Date = date;
            this._Extension = extension;
            this._Image = new BitmapImage(new Uri(_baseUri, imagePath));
        }

        private string _Name;
        public string Name
        {
            get
            {
                return this._Name;
            }
        }

        private string _Date = string.Empty;
        public string Date
        {
            get
            {
                return "Fecha:" + " " + this._Date;
            }
        }

        private string _Extension = string.Empty;
        public string Extension
        {
            get
            {
                string type = "PKCS#12";
                if ("PFX".Equals(this._Extension.Substring(1,3).ToUpper()))
                {
                    type = "Personal File Exchange (PFX)";
                }
                return "Tipo de almacén:" + " " + type;
            }
        }

        private ImageSource _Image = null;
        public ImageSource Image
        {
            get
            {
                return this._Image;
            }
        }
        
    }

    public class StoreData
    {
        public async Task<IReadOnlyList<StorageFile>> GetFiles()
        {
            IReadOnlyList<StorageFile> files = await ApplicationData.Current.LocalFolder.GetFilesAsync();
            return files;
        }

        public async Task DeleteFile(StorageFile file)
        {
            await file.DeleteAsync();
        }

        public void LoadData()
        {
            KeyStoreFile item;

            _Collection = new ItemCollection();
            // Cargamos los almacenes previamente importados (los existentes)
            //customerList = AsyncHelpers.RunSync<List<Customer>>(() => GetCustomers());
            IReadOnlyList<StorageFile> files = AsyncHelpers.RunSync<IReadOnlyList<StorageFile>>(() => GetFiles());

            // Cargamos primero los P12 en una lista, esto nos permite conocer el número de elementos antes de empezar
            // a cargar el desplegable
            List<String> stores = new List<String>();
            foreach (StorageFile file in files)
            {
                if (file.FileType.ToUpper().Equals(".PFX") || file.FileType.ToUpper().Equals(".P12"))
                {
                    item = new KeyStoreFile(file.DisplayName, file.DateCreated.ToString(), file.FileType.ToUpper(), "Images/certificados.png");
                    Collection.Add(item);
                }
            }
        }

        public void DeleteData(String nameFile)
        {
            // Cargamos los almacenes previamente importados (los existentes)
            //customerList = AsyncHelpers.RunSync<List<Customer>>(() => GetCustomers());
            IReadOnlyList<StorageFile> files = AsyncHelpers.RunSync<IReadOnlyList<StorageFile>>(() => GetFiles());

            // Cargamos primero los P12 en una lista, esto nos permite conocer el número de elementos antes de empezar
            // a cargar el desplegable
            List<String> stores = new List<String>();
            foreach (StorageFile file in files)
            {
                if (file.DisplayName.Equals(nameFile))
                {
                    AsyncHelpers.RunSync(() =>DeleteFile(file));
                }
            }

        }

        public ItemCollection _Collection = new ItemCollection();

        public ItemCollection Collection
        {
            get
            {
                return this._Collection;
            }
        }

        /// <summary>
        /// The method returns the list of groups, each containing a key and a list of items. 
        /// The key of each group is the category of the item. 
        /// </summary>
        /// <returns>
        /// The List of groups of items. 
        /// </returns>
        public ObservableCollection<KeyStoreFile> GetGroupsByCategory()
        {
            ObservableCollection<KeyStoreFile> groups = new ObservableCollection<KeyStoreFile>();

            foreach (KeyStoreFile g in this._Collection)
            {
                groups.Add(g);
            }

            return groups;
        }

    }

    // Workaround: data binding works best with an enumeration of objects that does not implement IList
    public class ItemCollection : IEnumerable<Object>
    {
        private System.Collections.ObjectModel.ObservableCollection<KeyStoreFile> itemCollection = new System.Collections.ObjectModel.ObservableCollection<KeyStoreFile>();

        public IEnumerator<Object> GetEnumerator()
        {
            return itemCollection.GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public void Add(KeyStoreFile item)
        {
            itemCollection.Add(item);            
        }

        public int Count()
        {
            return itemCollection.Count;
        }
        
    }



    
}
