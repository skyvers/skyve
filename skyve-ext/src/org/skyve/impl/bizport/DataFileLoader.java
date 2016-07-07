package org.skyve.impl.bizport;

import java.util.Date;
import java.util.List;

import org.skyve.bizport.BizPortException;
import org.skyve.domain.Bean;

/**
 * <pre>
 * A DataFileLoader is an interface to file-format-specific classes to allow
 * a common approach to loading of data files irrespective of format
 * for the creation or finding of Skyve beans based on the data in the file.
 * 
 * For example the POISheetLoader provides a simplified way to load POI Worksheet files
 * containing data, and use the loaded data as the basis of bean creation or 
 * finding beans.
 * 
 * Data File Loader handles referential integrity, by handing mapping of data columns or fields
 * to compound bindings in beans. If a column is mapped to a compound binding
 * the data file loader will create the beans required so that the value from the file
 * can be stored successfully.
 * 
 * To use a DataFileLoader,
 * - create a loader
 * - specify an array of bindings (including compound bindings)
 * - specify the activity type
 * - retrieve the beanResults (a list of resulting beans)
 * 
 * For example, a specific implementation of the DataFileLoader may have a usage similar to:
 * 
 *  BizPortException loadExceptions = new BizPortException();
 * 
 *  String[] bindings = new String[]{"company.contact.name", "company.abn", "invoiceNo", "invoiceDate", "invoiceAmount", "dueDate"};
 *  SpecificLoader loader = new SpecificLoader();
 *  
 *  loader.setLoaderActivityType(LoaderActivityType.CREATE_FIND);
 *  loader.setFileInputStream(file.getInputStream());
 *  loader.setException(loadExceptions);
 *  loader.setDocumentContext(Invoice.MODULE_NAME, Invoice.DOCUMENT_NAME);
 *  loader.setBindings(bindings);
 *  loader.setCreateMissingAssocations(Boolean.TRUE); //create any beans required for the load
 *  loader.setDataIndex(1); //handle header row in file
 * 
 *  List<Invoice> newInvoices = loader.beanResults();
 *  if(loadExceptions.hasProblems()){
 * 	 //handle issues
 *  }
 * 
 * For example, if a CSV file contained invoice information to be created, and the first column contained 
 * the name of a company, the column could be mapped to invoice.company.contact.name
 * This implies that for the column data to be loaded into beans, the loader must either find
 * an existing company.contact.name or create a company bean, a contact bean and set that atribute to the value.
 * 
 * (DataFileLoader assumes that population of compound bindings will be handled by Binder.population)
 * 
 * There are three loader activity types: 
 * CREATE_ALL 	- create all beans implied by the data in the file and the mapped bindings provided 
 * 				- do not attempt to locate existing  beans
 * CREATE_FIND - create beans but attempt to locate existing beans, and reuse any created beans
 * 			- for example if two rows of a CSV had the same value for invoice.company.contact.name
 * 			the CREATE_FIND activity is to attempt 
 * 				- to locate an existing company with contact.name equal to the value provided, or
 * 				- if a bean has been created during the load already for that value, reuse that bean.
 * FIND		- no creation of beans - just find existing beans with matching values
 * 
 * DataFileLoader offers two modes of operation - total file load, or iterative.
 * Iterative provides an iterator style, "line by line" approach to loading values to one bean at a time, 
 * similar to this:
 * 
 * 		int consecutiveBlankRowCount = 0;
 * 		while (loader.hasNextData() && consecutiveBlankRowCount < 10) {
 * 			loader.nextData();
 * 
 * 			//ignore totals row 
 * 			String rowTitle = loader.getStringFieldValue(0, true);
 * 			if(!"Total".equals(rowTitle)){
 * 				Invoice newInvoice = loader.beanResult();
 * 			}
 * 			
 * 			...
 * 		}
 * 
 * DataFileField is provided to allow more specific ways of handling individual columns/fields in the file.
 * 
 * By default, a SpecificLoader implementation will take the array of bindings and create DataFileFields for each binding
 * with default settings applicable to that implementation.
 * 
 * For example, a DataFileField may be created for the binding company.contact.name using the LoadAction LOOKUP_EQUALS
 * while a DataFileField for the binding invoiceNo would be SET_VALUE.
 * </pre>
 **/
public interface DataFileLoader {

	public static enum LoaderActivityType {
		CREATE_ALL, CREATE_FIND, FIND
	}

	public void setCreateMissingAssocations(boolean create);

	public void setEmptyAsZero(boolean emptyAsZero);

	public void setException(BizPortException exception);

	/**
	 * Set the activity being performed as a result of the upload
	 * 
	 * @param activityType
	 */
	public void setActivityType(LoaderActivityType activityType);

	/**
	 * Set the module and document context for the loader
	 * 
	 * @param moduleName
	 * @param documentName
	 * @throws Exception
	 */
	public void setDocumentContext(String moduleName, String documentName) throws Exception;

	/**
	 * Add a field definition to the loader
	 * 
	 * @param dff
	 * @throws Exception
	 */
	public void addField(DataFileField dff) throws Exception;
	
	/**
	 * Add a field definitions to the loader, based on the bindings provided
	 * 
	 * @param dff
	 * @throws Exception
	 */
	public void addFields(String... bindings) throws Exception;

	/**
	 * Returns the current dataIndex
	 * 
	 * @return
	 */
	public int getDataIndex();

	/**
	 * Sets the current data to dataIndex
	 * 
	 * @param rowIndex
	 * @throws Exception
	 */
	public void setDataIndex(int dataIndex) throws Exception;

	/**
	 * Loads data into a bean based on the field action definitions
	 * 
	 * @return
	 */
	public <T extends Bean> T beanResult() throws Exception;

	/**
	 * Returns the beans created as a result of the load
	 * 
	 * @return
	 * @throws Exception
	 */
	public <T extends Bean> List<T> beanResults() throws Exception;

	/**
	 * Returns the BizPortException which contains the warnings and exceptions encountered
	 * 
	 * @return
	 */
	public BizPortException getException();

	/**
	 * Returns a String value from the field
	 * 
	 * @param fieldIndex
	 * @param blankAsNull
	 * @return
	 * @throws Exception
	 */
	public String getStringFieldValue(int fieldIndex, boolean blankAsNull) throws Exception;

	/**
	 * Returns a Date value from the field
	 * 
	 * @param fieldIndex
	 * @return
	 * @throws Exception
	 */
	public Date getDateFieldValue(int fieldIndex) throws Exception;

	/**
	 * Returns a numeric value from the field
	 * 
	 * @param fieldIndex
	 * @param emptyAsZero
	 * @return
	 * @throws Exception
	 */
	public Double getNumericFieldValue(int fieldIndex, boolean emptyAsZero) throws Exception;

	/**
	 * Moves to next row, line or section of data
	 * 
	 * @throws Exception
	 */
	public void nextData() throws Exception;

	/**
	 * Whether ther is more data to load
	 * 
	 * @return
	 * @throws Exception
	 */
	public boolean hasNextData() throws Exception;

	/**
	 * Sets the index of the field to be examined
	 * 
	 * @param fieldIndex
	 */
	public void setFieldIndex(int fieldIndex);

	/**
	 * Returns true if there is nothing in this data
	 * 
	 * @return
	 * @throws Exception
	 */
	public boolean isNoData() throws Exception;

	/**
	 * Returns a description indicating the location of the value at fieldIndex
	 * 
	 * @param fieldIndex
	 * @return
	 * @throws Exception
	 */
	public String getWhere(int fieldIndex) throws Exception;

	/**
	 * A string representation of the debug information for the data
	 * 
	 * @return
	 */
	public String debugData() throws Exception;

}
