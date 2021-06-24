package modules.admin.Tag;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

import modules.admin.domain.Tag;
import modules.admin.domain.Tagged;

public class TagExtension extends Tag {
	private static final long serialVersionUID = -5043937504771795370L;

	/**
	 * Convenience method for total tag count across all documents
	 * 
	 * @return
	 */
	public long count() {
		return count(null, null);
	}

	/**
	 * Return the count of tagged document beans 
	 * 
	 * @param moduleName - the module of the document to be counted
	 * @param documentName - the document to be counted
	 * @return 
	 */
	@Override
	public long countDocument(String moduleName, String documentName) {
		if(moduleName==null || documentName==null) {
			//no need to run the query
			return 0;
		}
		return count(moduleName, documentName);
	}
	
	/**
	 * Return the number of tagged items for this tag
	 * Optionally filtered for a specific module.document
	 * 
	 * @param bean
	 * @param moduleName
	 * @param documentName
	 * @return
	 * @throws Exception
	 */
	private long count(String moduleName, String documentName) {

		//if refactoring this method for a TagExtension, ensure you re-test basic tagging functions in lists
		
		long result = 0;

		try {
			DocumentQuery q = CORE.getPersistence().newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
			q.getFilter().addEquals(Tagged.tagPropertyName, this);

			// optional filter for specified module.document
			if (moduleName != null && documentName != null) {
				q.getFilter().addEquals(Tagged.taggedModulePropertyName, moduleName);
				q.getFilter().addEquals(Tagged.taggedDocumentPropertyName, documentName);
			}
			
			q.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfTagged");

			result = q.retrieveScalar(Number.class).longValue();
		} catch (@SuppressWarnings("unused") Exception e) {
			// leave the result as zero
		}
		return result;
	}
}
