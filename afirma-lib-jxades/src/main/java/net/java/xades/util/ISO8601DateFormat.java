package net.java.xades.util;

import java.text.FieldPosition;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 *
 * @author miro
 */
public class ISO8601DateFormat
    extends SimpleDateFormat
{
    
    /** Creates a new instance of ISO8601DateFormat */
    public ISO8601DateFormat()
    {
        super("yyyy-MM-dd'T'HH:mm:ssZ");
    }


    public StringBuffer format(Date date,
                               StringBuffer toAppendTo,
                               FieldPosition fieldPosition)
    {
        StringBuffer sb = super.format(date, toAppendTo, fieldPosition);
        int size = sb.length();
        sb.insert(size - 2, ':');

        return sb;
    }

    public Date parse(String source, ParsePosition pos)
    {
        int size = source.length();
        if(source.charAt(size - 3) == ':')
            source = source.substring(0, size - 3) + source.substring(size - 2);

        return super.parse(source, pos);
    }
}
