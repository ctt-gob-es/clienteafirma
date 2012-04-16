package net.java.xades.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.JarURLConnection;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 *
 * @author miro
 */
public class ResourceHelper
{
    private String classFileName;
    private String resourceFolderName;
    private URL resourceURL;
    private Enumeration<ResourceEntry> resourceEntries;
    
    public ResourceHelper(Class clazz)
        throws IOException
    {
        this(clazz, null);
    }

    public ResourceHelper(Class clazz, String resourceFolderName)
        throws IOException
    {
        this.resourceFolderName = resourceFolderName;
        classFileName = clazz.getName().replace('.', '/') + ".class";
        ClassLoader classLoader = clazz.getClassLoader();
        resourceURL = classLoader.getSystemResource(classFileName);
        if(resourceURL == null)
        {
            resourceURL = classLoader.getResource(classFileName);
        }
        if(resourceURL == null)
            throw new IOException("Impossible to find Resource URL.");
    }

    public URL getResourceURL()
    {
        return resourceURL;
    }

    public Enumeration<ResourceEntry> resourceEntries()
        throws IOException
    {
        if(resourceEntries == null)
        {
            String protocol = resourceURL.getProtocol().toLowerCase();
            if("file".equals(protocol))
            {
                String path = resourceURL.getPath();
                int pos = path.length() - classFileName.length();
                String basePath = path.substring(0, pos);
                File baseFolder = new File(basePath);
                if(resourceFolderName != null)
                    baseFolder = new File(baseFolder, resourceFolderName);
                List<File> fileStore = new ArrayList<File>();
                loadFiles(baseFolder, fileStore);
                return new FileResourceEntries(fileStore);
            }
            else if("jar".equals(protocol))
            {
                JarURLConnection jarConn = (JarURLConnection)resourceURL.openConnection();
                JarFile jarFile = jarConn.getJarFile();
                resourceEntries = new JarResourceEntries(jarFile, resourceFolderName);
            }
            else
                resourceEntries = new EmptyResourceEntries();
        }

        return resourceEntries;
    }

    private void loadFiles(File folder, List<File> fileStore)
    {
        for(File file : folder.listFiles())
        {
            fileStore.add(file);
            if(file.isDirectory())
                loadFiles(file, fileStore);
        }
    }

    private static class EmptyResourceEntries
        implements Enumeration<ResourceEntry>
    {
        public boolean hasMoreElements()
        {
            return false;
        }

        public ResourceEntry nextElement()
        {
            throw new NoSuchElementException();
        }
    }

    private static class JarResourceEntries
        implements Enumeration<ResourceEntry>
    {
        private JarFile jarFile;
        private Iterator<JarEntry> entries;

        public JarResourceEntries(JarFile jarFile, String resourceFolderName)
        {
            this.jarFile = jarFile;
            List<JarEntry> jarEntryList = new ArrayList<JarEntry>();
            Enumeration<JarEntry> jarEntries = jarFile.entries();
            while(jarEntries.hasMoreElements())
            {
                JarEntry entry = jarEntries.nextElement();
                if(resourceFolderName == null || entry.getName().startsWith(resourceFolderName))
                    jarEntryList.add(entry);
            }

            entries = jarEntryList.iterator();
        }

        public boolean hasMoreElements()
        {
            return entries.hasNext();
        }

        public ResourceEntry nextElement()
        {
            return new JarResourceEntry(jarFile, entries.next());
        }
    }

    private static class FileResourceEntries
        implements Enumeration<ResourceEntry>
    {
        private Iterator<File> entries;

        public FileResourceEntries(List<File> fileStore)
        {
            entries = fileStore.iterator();
        }

        public boolean hasMoreElements()
        {
            return entries.hasNext();
        }

        public ResourceEntry nextElement()
        {
            return new FileResourceEntry(entries.next());
        }
    }

    public static interface ResourceEntry
    {
        public String getPathName();
        public boolean isDirectory();
        public InputStream getInputStream()
            throws IOException;
    }

    private static class FileResourceEntry
        implements ResourceEntry
    {
        private File file;

        public FileResourceEntry(File file)
        {
            this.file = file;
        }

        public String getPathName()
        {
            return file.getAbsolutePath();
        }

        public boolean isDirectory()
        {
            return file.isDirectory();
        }

        public InputStream getInputStream()
            throws IOException
        {
            return new FileInputStream(file);
        }
    }

    private static class JarResourceEntry
        implements ResourceEntry
    {
        private JarFile jarFile;
        private JarEntry entry;

        public JarResourceEntry(JarFile jarFile, JarEntry entry)
        {
            this.entry = entry;
            this.jarFile = jarFile;
        }

        public String getPathName()
        {
            return entry.getName();
        }

        public boolean isDirectory()
        {
            return entry.isDirectory();
        }

        public InputStream getInputStream()
            throws IOException
        {
            return jarFile.getInputStream(entry);
        }
    }
}
