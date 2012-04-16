package net.java.xades.util;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.text.DateFormat;
import java.io.LineNumberReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.security.AccessController;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;


/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
public class SystemUtils
{
    private static final String KEY_PREFIX = "com.cosmos";
    public static final String KEY_APPLICATION_NAME = KEY_PREFIX + ".apps.name";
    public static final String KEY_CLIENT_CONFIG_FOLDER = KEY_PREFIX + ".apps.client.config.folder";
    public static final String KEY_KEYSTORE_FOLDER = KEY_PREFIX + ".key.store.folder";
    private static final char[] HEX_DIGITS = "0123456789ABCDEF".toCharArray();
    private static final char[] EMPTY_CHAR_ARRAY = new char[0];

    private static DateFormat dateFormat;
    private static DecimalFormat decimalFormat;
    public static String charsetName = "UTF-8";

    private static DateFormat getDateFormatter()
    {
        if(dateFormat == null)
        {
            //String dateFormatString = ResourceBundleManager.getString("Default_Date_Formatter",
            //    "yyyy-MM-dd'T'HH:mm:ssZ");
            //dateFormat = new SimpleDateFormat(dateFormatString);
            dateFormat = new ISO8601DateFormat();
        }
        return dateFormat;
    }

    public static String formatDate(Object date)
    {
        return getDateFormatter().format(date);
    }

    public static String formatDate(Date date)
    {
        return getDateFormatter().format(date);
    }

    public static Date parseDate(String dateString)
        throws ParseException
    {
        DateFormat dateFormat = getDateFormatter();
        try
        {
            return dateFormat.parse(dateString);
        }
        catch(ParseException ex)
        {
            dateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss z");
            return dateFormat.parse(dateString);
        }
    }

    public static DecimalFormat getDecimalFormatter()
    {
        if(decimalFormat == null)
        {
            decimalFormat = new DecimalFormat("#,##0.##");
        }
        return decimalFormat;
    }

    public static String getCauseMessages(Throwable ex)
    {
        if(ex == null)
            return null;

        StringBuilder sb = new StringBuilder();
        sb.append(getErrorMessage(ex)).append("; \n");

        Throwable cause = ex.getCause();
        while(cause != null)
        {
            String message = getErrorMessage(cause);
            if(message != null)
            {
                sb.append(message).append("; \n");
            }
            cause = cause.getCause();
        }

        return sb.toString();
    }

    public static String getErrorMessage(Throwable ex)
    {
        if(ex != null)
        {
            String message = ex.getMessage();
            if(message == null)
                message = ex.getClass().getName();
            return message;
        }
        else
            return null;
    }
    
    public static byte[] toByteArray(InputStream inStream)
        throws IOException
    {
        if(inStream == null)
            return null;

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        copy(inStream, os);

        return os.toByteArray();
    }

    public static void copy(InputStream inStream, OutputStream outStream)
        throws IOException
    {
        if(inStream == null)
            throw new IllegalArgumentException("InputStream can not be NULL in copy method.");
        if(outStream == null)
            throw new IllegalArgumentException("OutputStream can not be NULL in copy method.");

        byte[] buffer = new byte[1024];
        int read = 0;

        try
        {
            while((read = inStream.read(buffer)) >= 0)
            {
                outStream.write(buffer, 0, read);
            }
            outStream.flush();
        }
        finally
        {
            buffer = null;
            inStream.close();
            inStream = null;
            outStream.close();
        }
    }
    
    public static String trimFileName(String filePathName)
    {
        if(filePathName == null)
            return filePathName;

        StringBuilder sb = new StringBuilder(filePathName);
        int size = sb.length();
        char ch;
        while(size > 0 && ((ch = sb.charAt(size - 1)) == '.' || ch == File.separatorChar))
        {
            size--;
            sb.setLength(size);
        }
        return sb.toString();
    }

    public static String getOSName()
    {
        return System.getProperty("os.name");
    }

    public static String getIOTempDir()
    {
        return System.getProperty("java.io.tmpdir");
    }

    public static String getUserHome()
    {
    	return System.getProperty("user.home");
    }

    public static String toHexString(byte[] data)
    {
        return new String(toHexChars(data));
    }

    public static char[] toHexChars(byte[] data)
    {
        if(data == null || data.length <= 0)
            return EMPTY_CHAR_ARRAY;

        int size = data.length;
        char[] result = new char[size << 1];

        for (int i = 0, j = 0; i < size; i++)
        {
            int ch = data[i];
            result[j++] = HEX_DIGITS[(ch & 0xF0) >>> 4];
            result[j++] = HEX_DIGITS[ch & 0x0F];
        }

        return result;
    }

    /**
     * Return the value of the boolean System property propName.
     */
    public static boolean getBooleanProperty(String propName, boolean defaultValue)
    {
        // if set, require value of either true or false
        String b = (String) AccessController.doPrivileged(new sun.security.action.GetPropertyAction(propName));
        
        if (b == null)
        {
        	return defaultValue;
        }

        return Boolean.parseBoolean(b);
    }
}
