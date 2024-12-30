package org.skyve.archive.support;

import java.text.ParseException;
import java.util.Date;

import org.apache.lucene.document.DateTools;
import org.apache.lucene.document.DateTools.Resolution;
import org.apache.lucene.document.Document;
import org.skyve.domain.Bean;

public interface DocumentConverter {

    /**
     * Convert the provided bean into a lucene document to be added to the index.
     * 
     * @param bean
     * @return
     */
    public Document convert(Bean bean);

    /**
     * Does this converter handle the given module/document?
     * 
     * @param module
     * @param document
     * @return
     */
    public boolean handles(String module, String document);

    public static String dateToString(Date date) {
        return DateTools.dateToString(date, Resolution.MILLISECOND);
    }

    public static Date stringToDate(String dateStr) {
        try {
            return DateTools.stringToDate(dateStr);
        } catch (ParseException e) {
            throw new RuntimeException("Unable to parse date string: " + dateStr, e);
        }
    }
    
    /**
     * If we use the same field name for the SortedDocValuesField as we do for
     * the stored field we run into this error:
     * 
     * <pre>
     * Term [4f 66 66 69 63 65] exists in doc values but not in the terms index
     * </pre>
     * 
     * I believe this is because the terms in the stored field have gone through the
     * analyzer's filter (lowercase, most likely). Storing the sort field under
     * a different name sidesteps the issues.
     * 
     * @see https://github.com/apache/lucene/issues/12067
     * 
     * @param binding
     * @return
     */
    public static String toSortBinding(String binding) {
        return binding + "_sort";
    }
}
