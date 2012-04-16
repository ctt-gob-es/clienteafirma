package net.java.xades.util;

import java.util.List;

/**
 *
 * @author miro
 */
public interface FileExtension
{
    public String getDescription();
    public String getExtension();
    public String getFileFilterName();
    public String getFileFilterPattern();
    public List<FileExtension> getExtensions();
    public boolean contains(FileExtension fileExt);
}
