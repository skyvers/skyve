package modules.admin.Audit.job.support;

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
}
