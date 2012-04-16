package net.java.xades.util;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

/**
 *
 * @author miro
 */
public enum DefaultFileExtension
    implements FileExtension
{
    CERTIFICATE_KEY_STORE_P12(".p12", "P12 Certificate Files"),
    CERTIFICATE_KEY_STORE_PFX(".pfx", "PFX Certificate Files"),
    CERTIFICATE_KEY_STORES("PKCS #12 Certificate Files", CERTIFICATE_KEY_STORE_P12, CERTIFICATE_KEY_STORE_PFX),
    CERTIFICATE_CER(".cer", "CER Certificate Files"),
    CERTIFICATE_CRT(".crt", "CRT Certificate Files"),
    CERTIFICATE_DER(".der", "DER Certificate Files"),
    ALL_CERTIFICATES("All Certificate Files", CERTIFICATE_CER, CERTIFICATE_CRT, CERTIFICATE_DER),
    CERTIFICATE_REVOCATION_LIST(".crl", "Certificate Revocation Lists", "CRL Files"),
    WINDOWS_SYSTEM_LIBRARY(".dll", "Windows System Library", "DLL Files"),
    LINUX_SYSTEM_LIBRARY(".so", "Linux System Library", "SO Files"),
    TXT_FILES(".txt", "Text Documents (*.txt) with UTF-8 encoding"),
    ALL_FILES(".*", "All files", "All Files");

    private DefaultFileExtension(String extension,
                                 String description)
    {
        this(extension, description, description, "*");
    }

    private DefaultFileExtension(String extension,
                                 String description,
                                 String fileFilterName)
    {
        this(extension, description, fileFilterName, "*");
    }

    private DefaultFileExtension(String extension,
                                 String description,
                                 String fileFilterName,
                                 String prefixFileFilterPattern)
    {
        this.extension = extension;
        this.description = description;
        this.fileFilterName = fileFilterName;
        this.fileFilterPattern = prefixFileFilterPattern + extension;
    }

    private DefaultFileExtension(String description,
                                 FileExtension... exts)
    {
        this(description, description, Arrays.asList(exts));
    }

    private DefaultFileExtension(String description,
                                 String fileFilterName,
                                 FileExtension... exts)
    {
        this(description, fileFilterName, Arrays.asList(exts));
    }

    private DefaultFileExtension(String description,
                                 String fileFilterName,
                                 List<FileExtension> exts)
    {
        this.description = description;
        this.fileFilterName = fileFilterName;
        this.extensions = exts;
    }

    public String getDescription()
    {
        return description;
    }

    public String getExtension()
    {
        return extension;
    }

    public String getFileFilterName()
    {
        return fileFilterName;
    }

    public String getFileFilterPattern()
    {
        return fileFilterPattern;
    }

    public List<FileExtension> getExtensions()
    {
        return extensions;
    }

    public boolean contains(FileExtension object)
    {
        if(extension != null)
            return equals(object);
        else if(extensions != null)
        {
            Iterator<FileExtension> iter = extensions.iterator();
            while(iter.hasNext())
            {
                if(iter.next().equals(object))
                    return true;
            }
        }
        return false;
    }

    private String extension;
    private String description;
    private String fileFilterName;
    private String fileFilterPattern;
    private List<FileExtension> extensions;


    public static DefaultFileExtension getEnumById(String extension)
    {
        return (DefaultFileExtension)getFileExtensionById(DefaultFileExtension.class, extension);
    }

    private static final TreeMap<String, TreeMap<String, FileExtension>> fileExtensionsMap =
        new TreeMap<String, TreeMap<String, FileExtension>>();

    public static FileExtension getFileExtensionById(Class fileExtnsionEnumClass, String extension)
    {
        if(fileExtnsionEnumClass == null || extension == null)
            throw new NullPointerException("Invalid parameter(s): 'fileExtnsionEnumClass' and 'extension' can not be NULL.");

        if(!fileExtnsionEnumClass.isEnum())
            throw new IllegalArgumentException("Invalid parameter 'fileExtnsionEnumClass'. The parameter must exends Enum class.");

        if(!FileExtension.class.isAssignableFrom(fileExtnsionEnumClass))
            throw new IllegalArgumentException("Invalid parameter 'fileExtnsionEnumClass'. The class must implements 'FileExtension' interface.");

        String className = fileExtnsionEnumClass.getName();
        TreeMap<String, FileExtension> enumMap = fileExtensionsMap.get(className);
        if(enumMap == null)
        {
            enumMap = new TreeMap<String, FileExtension>();
            for(Object enumObject : fileExtnsionEnumClass.getEnumConstants())
            {
                FileExtension fileExt = (FileExtension)enumObject;
                String ext = fileExt.getExtension();
                if(ext != null)
                {
                    enumMap.put(ext, fileExt);
                }
            }
            fileExtensionsMap.put(className, enumMap);
        }

        if(extension.charAt(0) != '.')
            return enumMap.get("." + extension.toLowerCase());
        else
            return enumMap.get(extension.toLowerCase());
    }


    public static void main(String[] args)
    {
        DefaultFileExtension fileExt = getEnumById("CRL");
        System.out.println("fileExt: " + fileExt);
    }
}
